//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// VirtualDtor.cpp -- generate code needed by the actor system
//
// Author           : Colby Parsons
// Created On       : Tues Mar 14 15:16:42 2023
// Last Modified By : Colby Parsons
// Last Modified On : Tues Mar 14 15:16:42 2023
// Update Count     : 0
//

#include "AST/Print.hpp"
#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "AST/Stmt.hpp"
#include "AST/TranslationUnit.hpp"
#include "AST/Expr.hpp"
#include <algorithm>
using namespace ast;
using namespace std;

namespace Virtual {

struct CtorDtor {
	FunctionDecl * dtorSetup;  // dtor init routine to add after last dtor for a struct
	FunctionDecl * deleteFn;
	FunctionDecl * lastDtor;    // pointer to last occurence of dtor to know where to insert after

	CtorDtor() : dtorSetup(nullptr), deleteFn(nullptr), lastDtor(nullptr) {}
};

class CtorDtorTable {
	unordered_map<const StructDecl *, CtorDtor> & structMap;

public:
	// if dtor is last dtor for this decl return the routine to add afterwards
	// otherwise return nullptr
	FunctionDecl * getToAddLater( const StructDecl * decl, FunctionDecl * dtor, FunctionDecl ** retDeleteFn ) {
		auto iter = structMap.find( decl );
		if ( iter == structMap.end() || iter->second.lastDtor != dtor ) return nullptr; // check if this is needed
		*retDeleteFn = iter->second.deleteFn;
		return iter->second.dtorSetup;
	}

	// return if the dtorSetup field has been defined for this decl
	bool inTable( const StructDecl * decl ) {
		auto iter = structMap.find( decl );
		return iter->second.dtorSetup != nullptr;
	}

	void addLater( const StructDecl * decl, FunctionDecl * dtorSetup, FunctionDecl * deleteFn ) {
		auto iter = structMap.find( decl );
		iter->second.dtorSetup = dtorSetup;
		iter->second.deleteFn = deleteFn;
	}

	void addDtor( const StructDecl * decl, FunctionDecl * dtor ) {
		auto iter = structMap.find( decl );
		iter->second.lastDtor = dtor;
	}

	CtorDtorTable( unordered_map<const StructDecl *, CtorDtor> & structMap ) : structMap(structMap) {}
};

struct CollectStructDecls : public ast::WithGuards {
	unordered_map<const StructDecl *, CtorDtor> & structDecls;
	StructDecl * parentDecl;
	bool insideStruct = false;
	bool namedDecl = false;

	const StructDecl ** virtualDtor;

	// finds and sets a ptr to the actor, message, and request structs, which are needed in the next pass
	void previsit( const StructDecl * decl ) {
		if ( !decl->body ) return;
		if( decl->name == "virtual_dtor" ) {
			structDecls.emplace( make_pair( decl, CtorDtor() ) );
			*virtualDtor = decl;
		} else {
			GuardValue(insideStruct);
			insideStruct = true;
			parentDecl = mutate( decl );
		}
	}

	// this catches structs of the form:
	//     struct derived_type { virtual_dtor a; };
	// since they should be:
	//     struct derived_type { inline virtual_dtor; };
	void previsit ( const ObjectDecl * decl ) {
		if ( insideStruct && ! decl->name.empty() ) {
			GuardValue(namedDecl);
			namedDecl = true;
		}
	}

	// this collects the derived actor and message struct decl ptrs
	void postvisit( const StructInstType * node ) {
		if ( ! *virtualDtor ) return;
		if ( insideStruct && !namedDecl ) {
			auto structIter = structDecls.find( node->aggr() );
			if ( structIter != structDecls.end() )
				structDecls.emplace( make_pair( parentDecl, CtorDtor() ) );
		}
	}

  public:
	CollectStructDecls( unordered_map<const StructDecl *, CtorDtor> & structDecls, const StructDecl ** virtualDtor ):
		structDecls( structDecls ), virtualDtor(virtualDtor) {}
};

// generates the forward decl of virtual dtor setting routine and delete routine
// generates the call to the virtual dtor routine in each appropriate ctor
// collects data needed for next pass that does the circular defn resolution
//     for dtor setters and delete fns (via table above)
struct GenFuncsCreateTables : public ast::WithDeclsToAdd {
	unordered_map<const StructDecl *, CtorDtor> & structDecls;
	CtorDtorTable & torDecls;
	const StructDecl ** virtualDtor;

	// collects the dtor info for actors/messages
	// gens the dtor fwd decl and dtor call in ctor
	void previsit( const FunctionDecl * decl ) {
		if ( (decl->name != "?{}" && decl->name != "^?{}") || decl->params.size() == 0
			|| !decl->stmts || (decl->name == "^?{}" && decl->params.size() != 1)) return;

		// the first param should be a reference
		const ReferenceType * ref = dynamic_cast<const ReferenceType *>(decl->params.at(0)->get_type());
		if ( !ref ) return;

		// the reference should be to a struct instance
		const StructInstType * instType = dynamic_cast<const StructInstType *>(ref->base.get());
		if ( !instType ) return;

		// return if not ctor/dtor for an actor or message
		auto structIter = structDecls.find( instType->aggr() );
		if ( structIter == structDecls.end() ) return;

		// If first param not named we need to name it to use it
		if ( decl->params.at(0)->name == "" )
			mutate( decl->params.at(0).get() )->name = "__CFA_Virt_Dtor_param";

		if ( decl->name == "^?{}") {
			torDecls.addDtor( structIter->first, mutate( decl ) );

			CompoundStmt * dtorBody = mutate( decl->stmts.get() );
			// Adds the following to the start of any actor/message dtor:
			//  __CFA_dtor_shutdown( this );
			dtorBody->push_front(
				new IfStmt( decl->location,
					new UntypedExpr (
						decl->location,
						new NameExpr( decl->location, "__CFA_dtor_shutdown" ),
						{
							new NameExpr( decl->location, decl->params.at(0)->name )
						}
					),
					new ReturnStmt( decl->location, nullptr )
				)
			);
			return;
		}

		// not dtor by this point so must be ctor
		CompoundStmt * ctorBody = mutate( decl->stmts.get() );
		// Adds the following to the end of any actor/message ctor:
		//  __CFA_set_dtor( this );
		ctorBody->push_back( new ExprStmt(
			decl->location,
			new UntypedExpr (
				decl->location,
				new NameExpr( decl->location, "__CFA_set_dtor" ),
				{
					new NameExpr( decl->location, decl->params.at(0)->name )
				}
			)
		));

		if ( torDecls.inTable( structIter->first ) ) return;

		// Generates the following:
		// void __CFA_set_dtor( Derived_type & this ){
		//     void (*__my_dtor)( Derived_type & ) = ^?{};
		//     this.__virtual_dtor = (void (*)( Base_type & ))__my_dtor;
		//     this.__virtual_obj_start = (void *)(&this);
		// }
		CompoundStmt * setDtorBody = new CompoundStmt( decl->location );

		// Function type is: (void (*)(Derived_type &))
		FunctionType * derivedDtor = new FunctionType();
		derivedDtor->params.push_back( ast::deepCopy( ref ) );

		// Generates:
		//      void (*__my_dtor)( Derived_type & ) = ^?{};
		setDtorBody->push_back( new DeclStmt(
			decl->location,
			new ObjectDecl(
				decl->location,
				"__my_dtor",
				new PointerType( derivedDtor ),
				new SingleInit( decl->location, new NameExpr( decl->location, "^?{}" ) )
			)
		));

		// Function type is: (void (*)( Base_type & ))
		FunctionType * baseDtor = new FunctionType();
		baseDtor->params.push_back( new ReferenceType( new StructInstType( *virtualDtor ) ) );

		// Generates:
		//     __CFA_set_virt_dtor( this, (void (*)( Base_type & ))__my_dtor )
		setDtorBody->push_back( new ExprStmt(
			decl->location,
			new UntypedExpr (
				decl->location,
				new NameExpr( decl->location, "__CFA_set_virt_dtor" ),
				{
					new NameExpr( decl->location, "this" ),
					new CastExpr( decl->location, new NameExpr( decl->location, "__my_dtor" ), new PointerType( baseDtor ), ExplicitCast )
				}
			)
		));

		// Generates:
		//     __CFA_set_virt_start( (void *)(&this) );
		setDtorBody->push_back( new ExprStmt(
			decl->location,
			new UntypedExpr (
				decl->location,
				new NameExpr( decl->location, "__CFA_set_virt_start" ),
				{
					new NameExpr( decl->location, "this" ),
					new CastExpr(
						decl->location,
						new AddressExpr( decl->location, new NameExpr( decl->location, "this" )),
						new PointerType( new ast::VoidType() ), ExplicitCast
						)
				}
			)
		));

		// put it all together into the complete function decl from above
		FunctionDecl * setDtorFunction = new FunctionDecl(
			decl->location,
			"__CFA_set_dtor",
			{
				new ObjectDecl(
					decl->location,
					"this",
					ast::deepCopy( ref )
				),
			},                      // params
			{},
			nullptr,               // body
			{ Storage::Static },    // storage
			Linkage::Cforall,       // linkage
			{},                     // attributes
			{ Function::Inline }
		);

		declsToAddBefore.push_back( ast::deepCopy( setDtorFunction ) );

		setDtorFunction->stmts = setDtorBody;

		// The following generates the following specialized delete routine:
		// static inline void delete( derived_type * ptr ) {
		//     if ( ptr )
		//         ^(*ptr){};
		//     __CFA_virt_free( *ptr );
		// }
		CompoundStmt * deleteFnBody = new CompoundStmt( decl->location );

		// Generates:
		//     if ( ptr )
		//         ^(*ptr){};
		deleteFnBody->push_back(
			new IfStmt(
				decl->location,
				UntypedExpr::createCall(
					decl->location,
					"?!=?",
					{
						new NameExpr( decl->location, "ptr" ),
						ConstantExpr::null( decl->location, new PointerType( ast::deepCopy( instType ) ) )
					}
				),
				new ExprStmt(
					decl->location,
					UntypedExpr::createCall(
						decl->location,
						"^?{}",
						{
							UntypedExpr::createDeref( decl->location, new NameExpr( decl->location, "ptr" ))
						}
					)
				)
			)
		);

		// Generates:
		//     __CFA_virt_free( *ptr );
		deleteFnBody->push_back( new ExprStmt(
				decl->location,
				UntypedExpr::createCall(
					decl->location,
					"__CFA_virt_free",
					{
						UntypedExpr::createDeref( decl->location, new NameExpr( decl->location, "ptr" ))
					}
				)
			)
		);

		FunctionDecl * deleteFn = new FunctionDecl(
			decl->location,
			"delete",
			{
				new ObjectDecl(
					decl->location,
					"ptr",
					new PointerType( ast::deepCopy( instType ) )
				),
			},                      // params
			{},
			nullptr,               // body
			{ Storage::Static },    // storage
			Linkage::Cforall,       // linkage
			{},                     // attributes
			{ Function::Inline }
		);

		declsToAddBefore.push_back( ast::deepCopy( deleteFn ) );

		deleteFn->stmts = deleteFnBody;

		torDecls.addLater( structIter->first, setDtorFunction, deleteFn );
	}

  public:
	GenFuncsCreateTables( unordered_map<const StructDecl *, CtorDtor> & structDecls, CtorDtorTable & torDecls, const StructDecl ** virtualDtor ):
	structDecls(structDecls), torDecls(torDecls), virtualDtor(virtualDtor) {}
};


// generates the trailing definitions of dtor setting routines for virtual dtors on messages and actors
// generates the function defns of __CFA_set_dtor
// separate pass is needed since  __CFA_set_dtor needs to be defined after
//   the last dtor defn which is found in prior pass
struct GenSetDtor : public ast::WithDeclsToAdd {
	unordered_map<const StructDecl *, CtorDtor> & structDecls; // set of decls that inherit from virt dtor
	CtorDtorTable & torDecls;

	// handles adding the declaration of the dtor init routine after the last dtor detected
	void postvisit( const FunctionDecl * decl ) {
		if ( decl->name != "^?{}" || !decl->stmts || decl->params.size() != 1 ) return;

		// the one param should be a reference
		const ReferenceType * ref = dynamic_cast<const ReferenceType *>(decl->params.at(0)->get_type());
		if ( !ref ) return;

		// the reference should be to a struct instance
		const StructInstType * instType = dynamic_cast<const StructInstType *>(ref->base.get());
		if ( !instType ) return;

		FunctionDecl * deleteRtn;

		// returns nullptr if not in table
		FunctionDecl * maybeAdd = torDecls.getToAddLater( instType->aggr(), mutate( decl ), &deleteRtn );
		if ( maybeAdd ) {
			declsToAddAfter.push_back( maybeAdd );
			declsToAddAfter.push_back( deleteRtn );
		}
	}

public:
	GenSetDtor( unordered_map<const StructDecl *, CtorDtor> & structDecls, CtorDtorTable & torDecls ):
		structDecls(structDecls), torDecls(torDecls) {}
};

void implementVirtDtors( TranslationUnit & translationUnit ) {
	// unordered_map to collect all derived types and associated data
	unordered_map<const StructDecl *, CtorDtor> structDecls;
	CtorDtorTable torDecls( structDecls );

	const StructDecl * virtualDtorPtr = nullptr;
	const StructDecl ** virtualDtor = &virtualDtorPtr;

	// first pass collects all structs that inherit from virtual_dtor
	Pass<CollectStructDecls>::run( translationUnit, structDecls, virtualDtor );

	// second pass locates all dtor/ctor routines that need modifying or need fns inserted before/after
	Pass<GenFuncsCreateTables>::run( translationUnit, structDecls, torDecls, virtualDtor );

	// The third pass adds the forward decls needed to resolve circular defn problems
	Pass<GenSetDtor>::run( translationUnit, structDecls, torDecls );
}

} // namespace Virtual

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //

