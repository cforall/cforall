//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenInit.cpp -- Generate initializers, and other stuff.
//
// Author           : Rob Schluntz
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Oct 25 13:53:00 2021
// Update Count     : 186
//
#include "GenInit.hpp"

#include <stddef.h>                    // for NULL
#include <algorithm>                   // for any_of
#include <cassert>                     // for assert, strict_dynamic_cast, assertf
#include <deque>
#include <iterator>                    // for back_inserter, inserter, back_inse...
#include <list>                        // for _List_iterator, list

#include "AST/Decl.hpp"
#include "AST/Init.hpp"
#include "AST/Pass.hpp"
#include "AST/Node.hpp"
#include "AST/Stmt.hpp"
#include "CompilationState.hpp"
#include "CodeGen/OperatorTable.hpp"
#include "Common/SemanticError.hpp"    // for SemanticError
#include "Common/ToString.hpp"         // for toCString
#include "Common/UniqueName.hpp"       // for UniqueName
#include "GenPoly/GenPoly.hpp"         // for getFunctionType, isPolyType
#include "GenPoly/ScopedSet.hpp"       // for ScopedSet, ScopedSet<>::const_iter...
#include "InitTweak.hpp"               // for isConstExpr, InitExpander, checkIn...
#include "ResolvExpr/Resolver.hpp"
#include "SymTab/GenImplicitCall.hpp"  // for genImplicitCall
#include "SymTab/Mangler.hpp"          // for Mangler
#include "Tuples/Tuples.hpp"           // for maybeImpure

namespace InitTweak {

namespace {

	// Outer pass finds declarations, for their type could wrap a type that needs hoisting
	struct HoistArrayDimension_NoResolve final :
			public ast::WithDeclsToAdd, public ast::WithShortCircuiting,
			public ast::WithGuards, public ast::WithConstTranslationUnit,
			public ast::WithVisitorRef<HoistArrayDimension_NoResolve>,
			public ast::WithSymbolTableX<ast::SymbolTable::ErrorDetection::IgnoreErrors> {

		// Inner pass looks within a type, for a part that depends on an expression
		struct HoistDimsFromTypes final :
				public ast::WithShortCircuiting, public ast::WithGuards {

			HoistArrayDimension_NoResolve * outer;
			HoistDimsFromTypes( HoistArrayDimension_NoResolve * outer ) : outer(outer) {}

			// Only intended for visiting through types.
			// Tolerate, and short-circuit at, the dimension expression of an array type.
			//    (We'll operate on the dimension expression of an array type directly
			//    from the parent type, not by visiting through it)
			// Look inside type exprs.
			void previsit( const ast::Node * ) {
				assert( false && "unsupported node type" );
			};
			const ast::Expr * allowedExpr = nullptr;
			void previsit( const ast::Type * ) {
				GuardValue( allowedExpr ) = nullptr;
			}
			void previsit( const ast::ArrayType * t ) {
				GuardValue( allowedExpr ) = t->dimension.get();
			}
			void previsit( const ast::PointerType * t ) {
				GuardValue( allowedExpr ) = t->dimension.get();
			}
			void previsit( const ast::TypeofType * t ) {
				GuardValue( allowedExpr ) = t->expr.get();
			}
			void previsit( const ast::Expr * e ) {
				assert( e == allowedExpr &&
				    "only expecting to visit exprs that are dimension exprs or typeof(-) inner exprs" );

				// Skip the tolerated expressions
				visit_children = false;
			}
			void previsit( const ast::TypeExpr * ) {}

			const ast::Type * postvisit(
					const ast::ArrayType * arrayType ) {
				static UniqueName dimensionName( "_array_dim" );

				if ( nullptr == arrayType->dimension ) {  // if no dimension is given, don't presume to invent one
					return arrayType;
				}

				// find size_t; use it as the type for a dim expr
				ast::ptr<ast::Type> dimType = outer->transUnit().global.sizeType;
				assert( dimType );
				add_qualifiers( dimType, ast::CV::Qualifiers( ast::CV::Const ) );

				// Special-case handling: leave the user's dimension expression alone
				// - requires the user to have followed a careful convention
				// - may apply to extremely simple applications, but only as windfall
				// - users of advanced applications will be following the convention on purpose
				// - CFA maintainers must protect the criteria against leaving too much alone

				// Actual leave-alone cases following are conservative approximations of "cannot vary"

				// Leave alone: literals and enum constants
				if ( dynamic_cast< const ast::ConstantExpr * >( arrayType->dimension.get() ) ) {
					return arrayType;
				}

				// Leave alone: direct use of an object declared to be const
				const ast::NameExpr * dimn = dynamic_cast< const ast::NameExpr * >( arrayType->dimension.get() );
				if ( dimn ) {
					std::vector<ast::SymbolTable::IdData> dimnDefs = outer->symtab.lookupId( dimn->name );
					if ( dimnDefs.size() == 1 ) {
						const ast::DeclWithType * dimnDef = dimnDefs[0].id.get();
						assert( dimnDef && "symbol table binds a name to nothing" );
						const ast::ObjectDecl * dimOb = dynamic_cast< const ast::ObjectDecl * >( dimnDef );
						if( dimOb ) {
							const ast::Type * dimTy = dimOb->type.get();
							assert( dimTy && "object declaration bearing no type" );
							// must not hoist some: size_t
							// must hoist all: pointers and references
							// the analysis is conservative; BasicType is a simple approximation
							if ( dynamic_cast< const ast::BasicType * >( dimTy ) ||
							     dynamic_cast< const ast::SueInstType<ast::EnumDecl> * >( dimTy ) ) {
								if ( dimTy->is_const() ) {
									// The dimension is certainly re-evaluable, giving the same answer each time.
									// Our user might be hoping to write the array type in multiple places, having them unify.
									// Leave the type alone.

									// We believe the new criterion leaves less alone than the old criterion.
									// Thus, the old criterion should have left the current case alone.
									// Catch cases that weren't thought through.
									assert( !Tuples::maybeImpure( arrayType->dimension ) );

									return arrayType;
								}
							};
						}
					}
				}

				// Leave alone: any sizeof expression (answer cannot vary during current lexical scope)
				const ast::SizeofExpr * sz = dynamic_cast< const ast::SizeofExpr * >( arrayType->dimension.get() );
				if ( sz ) {
					return arrayType;
				}

				// General-case handling: change the array-type's dim expr (hoist the user-given content out of the type)
				// - always safe
				// - user-unnoticeable in common applications (benign noise in -CFA output)
				// - may annoy a responsible user of advanced applications (but they can work around)
				// - protects against misusing advanced features
				//
				// The hoist, by example, is:
				// FROM USER:  float a[ rand() ];
				// TO GCC:     const size_t __len_of_a = rand(); float a[ __len_of_a ];
				ast::ObjectDecl * arrayDimension = nullptr;

				const ast::TypeExpr * ty = dynamic_cast< const ast::TypeExpr * >( arrayType->dimension.get() );
				if ( ty ) {
					auto inst = ty->type.as<ast::EnumInstType>();
					if ( inst ) {
						if ( inst->base->isCfa ) {
							arrayDimension = new ast::ObjectDecl(
								arrayType->dimension->location,
								dimensionName.newName(),
								new ast::BasicType( ast::BasicKind::UnsignedChar ),
								new ast::SingleInit(
									arrayType->dimension->location,
									ast::ConstantExpr::from_int( arrayType->dimension->location, inst->base->members.size() )
								)
							);
							// return arrayType;
						}
					}
				}
				if ( arrayDimension == nullptr ) {
					arrayDimension = new ast::ObjectDecl(
						arrayType->dimension->location,
						dimensionName.newName(),
						dimType,
						new ast::SingleInit(
							arrayType->dimension->location,
							arrayType->dimension
						)
					);
				}

				ast::ArrayType * mutType = ast::mutate( arrayType );
				mutType->dimension = new ast::VariableExpr(
						arrayDimension->location, arrayDimension );
				outer->declsToAddBefore.push_back( arrayDimension );

				return mutType;
			}  // postvisit( const ast::ArrayType * )
		}; // struct HoistDimsFromTypes

		ast::Storage::Classes storageClasses;
		void previsit(
				const ast::ObjectDecl * decl ) {
			GuardValue( storageClasses ) = decl->storage;
		}

		const ast::DeclWithType * postvisit(
				const ast::ObjectDecl * objectDecl ) {

			if ( !isInFunction() || storageClasses.is_static ) {
				return objectDecl;
			}

			const ast::Type * mid = objectDecl->type;

			ast::Pass<HoistDimsFromTypes> hoist{this};
			const ast::Type * result = mid->accept( hoist );

			return mutate_field( objectDecl, &ast::ObjectDecl::type, result );
		}
	};

	struct ReturnFixer final :
			public ast::WithStmtsToAdd, ast::WithGuards, ast::WithShortCircuiting {
		void previsit( const ast::FunctionDecl * decl );
		const ast::ReturnStmt * previsit( const ast::ReturnStmt * stmt );
	private:
		const ast::FunctionDecl * funcDecl = nullptr;
	};

	void ReturnFixer::previsit( const ast::FunctionDecl * decl ) {
		if (decl->linkage == ast::Linkage::Intrinsic) visit_children = false;
		GuardValue( funcDecl ) = decl;
	}

	const ast::ReturnStmt * ReturnFixer::previsit(
			const ast::ReturnStmt * stmt ) {
		auto & returns = funcDecl->returns;
		assert( returns.size() < 2 );
		// Hands off if the function returns a reference.
		// Don't allocate a temporary if the address is returned.
		if ( stmt->expr && 1 == returns.size() ) {
			ast::ptr<ast::DeclWithType> retDecl = returns.front();
			if ( isConstructable( retDecl->get_type() ) ) {
				// Explicitly construct the return value using the return
				// expression and the retVal object.
				assertf( "" != retDecl->name,
					"Function %s has unnamed return value.\n",
					funcDecl->name.c_str() );

				auto retVal = retDecl.strict_as<ast::ObjectDecl>();
				if ( auto varExpr = stmt->expr.as<ast::VariableExpr>() ) {
					// Check if the return statement is already set up.
					if ( varExpr->var == retVal ) return stmt;
				}
				const ast::Stmt * ctorStmt = genCtorDtor(
					retVal->location, "?{}", retVal, stmt->expr );
				assertf( ctorStmt,
					"ReturnFixer: genCtorDtor returned nullptr: %s / %s",
					toString( retVal ).c_str(),
					toString( stmt->expr ).c_str() );
				stmtsToAddBefore.push_back( ctorStmt );

				// Return the retVal object.
				ast::ReturnStmt * mutStmt = ast::mutate( stmt );
				mutStmt->expr = new ast::VariableExpr(
					stmt->location, retDecl );
				return mutStmt;
			}
		}
		return stmt;
	}

} // namespace

void genInit( ast::TranslationUnit & transUnit ) {
	ast::Pass<HoistArrayDimension_NoResolve>::run( transUnit );
	ast::Pass<ReturnFixer>::run( transUnit );
}

void fixReturnStatements( ast::TranslationUnit & transUnit ) {
	ast::Pass<ReturnFixer>::run( transUnit );
}

bool ManagedTypes::isManaged( const ast::Type * type ) const {
	// references are never constructed
	if ( dynamic_cast< const ast::ReferenceType * >( type ) ) return false;
	if ( auto tupleType = dynamic_cast< const ast::TupleType * > ( type ) ) {
		// tuple is also managed if any of its components are managed
		for (auto & component : tupleType->types) {
			if (isManaged(component)) return true;
		}
	}
	// need to clear and reset qualifiers when determining if a type is managed
	auto tmp = shallowCopy(type);
	tmp->qualifiers = {};
	// delete tmp at return
	ast::ptr<ast::Type> guard = tmp;
	// a type is managed if it appears in the map of known managed types, or if it contains any polymorphism (is a type variable or generic type containing a type variable)
	return managedTypes.find( Mangle::mangle( tmp, {Mangle::NoOverrideable | Mangle::NoGenericParams | Mangle::Type} ) ) != managedTypes.end() || GenPoly::isPolyType( tmp );
}

bool ManagedTypes::isManaged( const ast::ObjectDecl * objDecl ) const {
	const ast::Type * type = objDecl->type;
	while ( auto at = dynamic_cast< const ast::ArrayType * >( type ) ) {
		// must always construct VLAs with an initializer, since this is an error in C
		if ( at->isVarLen && objDecl->init ) return true;
		type = at->base;
	}
	return isManaged( type );
}

void ManagedTypes::handleDWT( const ast::DeclWithType * dwt ) {
	// if this function is a user-defined constructor or destructor, mark down the type as "managed"
	if ( ! dwt->linkage.is_overrideable && CodeGen::isCtorDtor( dwt->name ) ) {
		auto & params = GenPoly::getFunctionType( dwt->get_type())->params;
		assert( ! params.empty() );
		// Type * type = InitTweak::getPointerBase( params.front() );
		// assert( type );
		managedTypes.insert( Mangle::mangle( params.front(), {Mangle::NoOverrideable | Mangle::NoGenericParams | Mangle::Type} ) );
	}
}

void ManagedTypes::handleStruct( const ast::StructDecl * aggregateDecl ) {
	// don't construct members, but need to take note if there is a managed member,
	// because that means that this type is also managed
	for ( auto & member : aggregateDecl->members ) {
		if ( auto field = member.as<ast::ObjectDecl>() ) {
			if ( isManaged( field ) ) {
				// generic parameters should not play a role in determining whether a generic type is constructed - construct all generic types, so that
				// polymorphic constructors make generic types managed types
				ast::StructInstType inst( aggregateDecl );
				managedTypes.insert( Mangle::mangle( &inst, {Mangle::NoOverrideable | Mangle::NoGenericParams | Mangle::Type} ) );
				break;
			}
		}
	}
}

void ManagedTypes::beginScope() { managedTypes.beginScope(); }
void ManagedTypes::endScope() { managedTypes.endScope(); }

const ast::Stmt * genCtorDtor( const CodeLocation & loc, const std::string & fname, const ast::ObjectDecl * objDecl, const ast::Expr * arg ) {
	assertf(objDecl, "genCtorDtor passed null objDecl");
	InitExpander srcParam(arg);
	return SymTab::genImplicitCall(srcParam, new ast::VariableExpr(loc, objDecl), loc, fname, objDecl);
}

ast::ConstructorInit * genCtorInit( const CodeLocation & loc, const ast::ObjectDecl * objDecl ) {
	// Call genImplicitCall to generate calls to ctor/dtor for each constructable object.
	InitExpander srcParam{ objDecl->init }, nullParam{ (const ast::Init *)nullptr };
	ast::ptr< ast::Expr > dstParam = new ast::VariableExpr(loc, objDecl);

	ast::ptr< ast::Stmt > ctor = SymTab::genImplicitCall(
		srcParam, dstParam, loc, "?{}", objDecl );
	ast::ptr< ast::Stmt > dtor = SymTab::genImplicitCall(
		nullParam, dstParam, loc, "^?{}", objDecl,
		SymTab::LoopBackward );

	// check that either both ctor and dtor are present, or neither
	assert( (bool)ctor == (bool)dtor );

	if ( ctor ) {
		// need to remember init expression, in case no ctors exist. If ctor does exist, want to
		// use ctor expression instead of init.
		ctor.strict_as< ast::ImplicitCtorDtorStmt >();
		dtor.strict_as< ast::ImplicitCtorDtorStmt >();

		return new ast::ConstructorInit{ loc, ctor, dtor, objDecl->init };
	}

	return nullptr;
}

} // namespace InitTweak

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
