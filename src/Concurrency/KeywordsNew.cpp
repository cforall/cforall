//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// KeywordsNew.cpp -- Implement concurrency constructs from their keywords.
//
// Author           : Andrew Beach
// Created On       : Tue Nov 16  9:53:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Fri Mar 11 10:40:00 2022
// Update Count     : 2
//

#include <iostream>

#include "Concurrency/Keywords.h"

#include "AST/Copy.hpp"
#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Inspect.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/DeclReplacer.hpp"
#include "AST/TranslationUnit.hpp"
#include "CodeGen/OperatorTable.h"
#include "Common/Examine.h"
#include "Common/utility.h"
#include "Common/UniqueName.h"
#include "ControlStruct/LabelGeneratorNew.hpp"
#include "InitTweak/InitTweak.h"
#include "Virtual/Tables.h"

namespace Concurrency {

namespace {

// --------------------------------------------------------------------------
// Loose Helper Functions:

/// Detect threads constructed with the keyword thread.
bool isThread( const ast::DeclWithType * decl ) {
	auto baseType = decl->get_type()->stripDeclarator();
	auto instType = dynamic_cast<const ast::StructInstType *>( baseType );
	if ( nullptr == instType ) { return false; }
	return instType->base->is_thread();
}

/// Get the virtual type id if given a type name.
std::string typeIdType( std::string const & exception_name ) {
	return exception_name.empty() ? std::string()
		: Virtual::typeIdType( exception_name );
}

/// Get the vtable type name if given a type name.
std::string vtableTypeName( std::string const & exception_name ) {
	return exception_name.empty() ? std::string()
		: Virtual::vtableTypeName( exception_name );
}

static ast::Type * mutate_under_references( ast::ptr<ast::Type>& type ) {
	ast::Type * mutType = type.get_and_mutate();
	for ( ast::ReferenceType * mutRef
		; (mutRef = dynamic_cast<ast::ReferenceType *>( mutType ))
		; mutType = mutRef->base.get_and_mutate() );
	return mutType;
}

// Describe that it adds the generic parameters and the uses of the generic
// parameters on the function and first "this" argument.
ast::FunctionDecl * fixupGenerics(
		const ast::FunctionDecl * func, const ast::StructDecl * decl ) {
	const CodeLocation & location = decl->location;
	// We have to update both the declaration
	auto mutFunc = ast::mutate( func );
	auto mutType = mutFunc->type.get_and_mutate();

	if ( decl->params.empty() ) {
		return mutFunc;
	}

	assert( 0 != mutFunc->params.size() );
	assert( 0 != mutType->params.size() );

	// Add the "forall" clause information.
	for ( const ast::ptr<ast::TypeDecl> & typeParam : decl->params ) {
		auto typeDecl = ast::deepCopy( typeParam );
		mutFunc->type_params.push_back( typeDecl );
		mutType->forall.push_back( new ast::TypeInstType( typeDecl ) );
		for ( auto & assertion : typeDecl->assertions ) {
			mutFunc->assertions.push_back( assertion );
			mutType->assertions.emplace_back(
				new ast::VariableExpr( location, assertion ) );
		}
		typeDecl->assertions.clear();
	}

	// Even chain_mutate is not powerful enough for this:
	ast::ptr<ast::Type>& paramType = strict_dynamic_cast<ast::ObjectDecl *>(
		mutFunc->params[0].get_and_mutate() )->type;
	auto paramTypeInst = strict_dynamic_cast<ast::StructInstType *>(
		mutate_under_references( paramType ) );
	auto typeParamInst = strict_dynamic_cast<ast::StructInstType *>(
		mutate_under_references( mutType->params[0] ) );

	for ( const ast::ptr<ast::TypeDecl> & typeDecl : mutFunc->type_params ) {
		paramTypeInst->params.push_back(
			new ast::TypeExpr( location, new ast::TypeInstType( typeDecl ) ) );
		typeParamInst->params.push_back(
			new ast::TypeExpr( location, new ast::TypeInstType( typeDecl ) ) );
	}

	return mutFunc;
}

// --------------------------------------------------------------------------
struct ConcurrentSueKeyword : public ast::WithDeclsToAdd<> {
	ConcurrentSueKeyword(
		std::string&& type_name, std::string&& field_name,
		std::string&& getter_name, std::string&& context_error,
		std::string&& exception_name,
		bool needs_main, ast::AggregateDecl::Aggregate cast_target
	) :
		type_name( type_name ), field_name( field_name ),
		getter_name( getter_name ), context_error( context_error ),
		exception_name( exception_name ),
		typeid_name( typeIdType( exception_name ) ),
		vtable_name( vtableTypeName( exception_name ) ),
		needs_main( needs_main ), cast_target( cast_target )
	{}

	virtual ~ConcurrentSueKeyword() {}

	const ast::Decl * postvisit( const ast::StructDecl * decl );
	const ast::DeclWithType * postvisit( const ast::FunctionDecl * decl );
	const ast::Expr * postvisit( const ast::KeywordCastExpr * expr );

	struct StructAndField {
		const ast::StructDecl * decl;
		const ast::ObjectDecl * field;
	};

	const ast::StructDecl * handleStruct( const ast::StructDecl * );
	void handleMain( const ast::FunctionDecl *, const ast::StructInstType * );
	void addTypeId( const ast::StructDecl * );
	void addVtableForward( const ast::StructDecl * );
	const ast::FunctionDecl * forwardDeclare( const ast::StructDecl * );
	StructAndField addField( const ast::StructDecl * );
	void addGetRoutines( const ast::ObjectDecl *, const ast::FunctionDecl * );
	void addLockUnlockRoutines( const ast::StructDecl * );

private:
	const std::string type_name;
	const std::string field_name;
	const std::string getter_name;
	const std::string context_error;
	const std::string exception_name;
	const std::string typeid_name;
	const std::string vtable_name;
	const bool needs_main;
	const ast::AggregateDecl::Aggregate cast_target;

	const ast::StructDecl   * type_decl = nullptr;
	const ast::FunctionDecl * dtor_decl = nullptr;
	const ast::StructDecl * except_decl = nullptr;
	const ast::StructDecl * typeid_decl = nullptr;
	const ast::StructDecl * vtable_decl = nullptr;

};

// Handles thread type declarations:
//
// thread Mythread {                         struct MyThread {
//  int data;                                  int data;
//  a_struct_t more_data;                      a_struct_t more_data;
//                                =>             thread$ __thrd_d;
// };                                        };
//                                           static inline thread$ * get_thread( MyThread * this ) { return &this->__thrd_d; }
//
struct ThreadKeyword final : public ConcurrentSueKeyword {
	ThreadKeyword() : ConcurrentSueKeyword(
		"thread$",
		"__thrd",
		"get_thread",
		"thread keyword requires threads to be in scope, add #include <thread.hfa>\n",
		"ThreadCancelled",
		true,
		ast::AggregateDecl::Thread )
	{}

	virtual ~ThreadKeyword() {}
};

// Handles coroutine type declarations:
//
// coroutine MyCoroutine {                   struct MyCoroutine {
//  int data;                                  int data;
//  a_struct_t more_data;                      a_struct_t more_data;
//                                =>             coroutine$ __cor_d;
// };                                        };
//                                           static inline coroutine$ * get_coroutine( MyCoroutine * this ) { return &this->__cor_d; }
//
struct CoroutineKeyword final : public ConcurrentSueKeyword {
	CoroutineKeyword() : ConcurrentSueKeyword(
		"coroutine$",
		"__cor",
		"get_coroutine",
		"coroutine keyword requires coroutines to be in scope, add #include <coroutine.hfa>\n",
		"CoroutineCancelled",
		true,
		ast::AggregateDecl::Coroutine )
	{}

	virtual ~CoroutineKeyword() {}
};

// Handles monitor type declarations:
//
// monitor MyMonitor {                       struct MyMonitor {
//  int data;                                  int data;
//  a_struct_t more_data;                      a_struct_t more_data;
//                                =>             monitor$ __mon_d;
// };                                        };
//                                           static inline monitor$ * get_coroutine( MyMonitor * this ) {
//                                               return &this->__cor_d;
//                                           }
//                                           void lock(MyMonitor & this) {
//                                               lock(get_monitor(this));
//                                           }
//                                           void unlock(MyMonitor & this) {
//                                               unlock(get_monitor(this));
//                                           }
//
struct MonitorKeyword final : public ConcurrentSueKeyword {
	MonitorKeyword() : ConcurrentSueKeyword(
		"monitor$",
		"__mon",
		"get_monitor",
		"monitor keyword requires monitors to be in scope, add #include <monitor.hfa>\n",
		"",
		false,
		ast::AggregateDecl::Monitor )
	{}

	virtual ~MonitorKeyword() {}
};

// Handles generator type declarations:
//
// generator MyGenerator {                   struct MyGenerator {
//  int data;                                  int data;
//  a_struct_t more_data;                      a_struct_t more_data;
//                                =>             int __generator_state;
// };                                        };
//
struct GeneratorKeyword final : public ConcurrentSueKeyword {
	GeneratorKeyword() : ConcurrentSueKeyword(
		"generator$",
		"__generator_state",
		"get_generator",
		"Unable to find builtin type generator$\n",
		"",
		true,
		ast::AggregateDecl::Generator )
	{}

	virtual ~GeneratorKeyword() {}
};

const ast::Decl * ConcurrentSueKeyword::postvisit(
		const ast::StructDecl * decl ) {
	if ( !decl->body ) {
		return decl;
	} else if ( cast_target == decl->kind ) {
		return handleStruct( decl );
	} else if ( type_name == decl->name ) {
		assert( !type_decl );
		type_decl = decl;
	} else if ( exception_name == decl->name ) {
		assert( !except_decl );
		except_decl = decl;
	} else if ( typeid_name == decl->name ) {
		assert( !typeid_decl );
		typeid_decl = decl;
	} else if ( vtable_name == decl->name ) {
		assert( !vtable_decl );
		vtable_decl = decl;
	}
	return decl;
}

// Try to get the full definition, but raise an error on conflicts.
const ast::FunctionDecl * getDefinition(
		const ast::FunctionDecl * old_decl,
		const ast::FunctionDecl * new_decl ) {
	if ( !new_decl->stmts ) {
		return old_decl;
	} else if ( !old_decl->stmts ) {
		return new_decl;
	} else {
		assert( !old_decl->stmts || !new_decl->stmts );
		return nullptr;
	}
}

const ast::DeclWithType * ConcurrentSueKeyword::postvisit(
		const ast::FunctionDecl * decl ) {
	if ( type_decl && isDestructorFor( decl, type_decl ) ) {
		// Check for forward declarations, try to get the full definition.
		dtor_decl = (dtor_decl) ? getDefinition( dtor_decl, decl ) : decl;
	} else if ( !vtable_name.empty() && decl->has_body() ) {
		if (const ast::DeclWithType * param = isMainFor( decl, cast_target )) {
			if ( !vtable_decl ) {
				SemanticError( decl, context_error );
			}
			// Should be safe because of isMainFor.
			const ast::StructInstType * struct_type =
				static_cast<const ast::StructInstType *>(
					static_cast<const ast::ReferenceType *>(
						param->get_type() )->base.get() );

			handleMain( decl, struct_type );
		}
	}
	return decl;
}

const ast::Expr * ConcurrentSueKeyword::postvisit(
		const ast::KeywordCastExpr * expr ) {
	if ( cast_target == expr->target ) {
		// Convert `(thread &)ex` to `(thread$ &)*get_thread(ex)`, etc.
		if ( !type_decl || !dtor_decl ) {
			SemanticError( expr, context_error );
		}
		assert( nullptr == expr->result );
		auto cast = ast::mutate( expr );
		cast->result = new ast::ReferenceType( new ast::StructInstType( type_decl ) );
		cast->concrete_target.field  = field_name;
		cast->concrete_target.getter = getter_name;
		return cast;
	}
	return expr;
}

const ast::StructDecl * ConcurrentSueKeyword::handleStruct(
		const ast::StructDecl * decl ) {
	assert( decl->body );

	if ( !type_decl || !dtor_decl ) {
		SemanticError( decl, context_error );
	}

	if ( !exception_name.empty() ) {
		if( !typeid_decl || !vtable_decl ) {
			SemanticError( decl, context_error );
		}
		addTypeId( decl );
		addVtableForward( decl );
	}

	const ast::FunctionDecl * func = forwardDeclare( decl );
	StructAndField addFieldRet = addField( decl );
	decl = addFieldRet.decl;
	const ast::ObjectDecl * field = addFieldRet.field;

	addGetRoutines( field, func );
	// Add routines to monitors for use by mutex stmt.
	if ( ast::AggregateDecl::Monitor == cast_target ) {
		addLockUnlockRoutines( decl );
	}

	return decl;
}

void ConcurrentSueKeyword::handleMain(
		const ast::FunctionDecl * decl, const ast::StructInstType * type ) {
	assert( vtable_decl );
	assert( except_decl );

	const CodeLocation & location = decl->location;

	std::vector<ast::ptr<ast::Expr>> poly_args = {
		new ast::TypeExpr( location, type ),
	};
	ast::ObjectDecl * vtable_object = Virtual::makeVtableInstance(
		location,
		"_default_vtable_object_declaration",
		new ast::StructInstType( vtable_decl, copy( poly_args ) ),
		type,
		nullptr
	);
	declsToAddAfter.push_back( vtable_object );
	declsToAddAfter.push_back(
		new ast::ObjectDecl(
			location,
			Virtual::concurrentDefaultVTableName(),
			new ast::ReferenceType( vtable_object->type, ast::CV::Const ),
			new ast::SingleInit( location,
				new ast::VariableExpr( location, vtable_object ) )
		)
	);
	declsToAddAfter.push_back( Virtual::makeGetExceptionFunction(
		location,
		vtable_object,
		new ast::StructInstType( except_decl, copy( poly_args ) )
	) );
}

void ConcurrentSueKeyword::addTypeId( const ast::StructDecl * decl ) {
	assert( typeid_decl );
	const CodeLocation & location = decl->location;

	ast::StructInstType * typeid_type =
		new ast::StructInstType( typeid_decl, ast::CV::Const );
	typeid_type->params.push_back(
		new ast::TypeExpr( location, new ast::StructInstType( decl ) ) );
	declsToAddBefore.push_back(
		Virtual::makeTypeIdInstance( location, typeid_type ) );
	// If the typeid_type is going to be kept, the other reference will have
	// been made by now, but we also get to avoid extra mutates.
	ast::ptr<ast::StructInstType> typeid_cleanup = typeid_type;
}

void ConcurrentSueKeyword::addVtableForward( const ast::StructDecl * decl ) {
	assert( vtable_decl );
	const CodeLocation& location = decl->location;

	std::vector<ast::ptr<ast::Expr>> poly_args = {
		new ast::TypeExpr( location, new ast::StructInstType( decl ) ),
	};
	declsToAddBefore.push_back( Virtual::makeGetExceptionForward(
		location,
		new ast::StructInstType( vtable_decl, copy( poly_args ) ),
		new ast::StructInstType( except_decl, copy( poly_args ) )
	) );
	ast::ObjectDecl * vtable_object = Virtual::makeVtableForward(
		location,
		"_default_vtable_object_declaration",
		new ast::StructInstType( vtable_decl, std::move( poly_args ) )
	);
	declsToAddBefore.push_back( vtable_object );
	declsToAddBefore.push_back(
		new ast::ObjectDecl(
			location,
			Virtual::concurrentDefaultVTableName(),
			new ast::ReferenceType( vtable_object->type, ast::CV::Const ),
			nullptr,
			ast::Storage::Extern,
			ast::Linkage::Cforall
		)
	);
}

const ast::FunctionDecl * ConcurrentSueKeyword::forwardDeclare(
		const ast::StructDecl * decl ) {
	const CodeLocation & location = decl->location;

	ast::StructDecl * forward = ast::deepCopy( decl );
	{
		// If removing members makes ref-count go to zero, do not free.
		ast::ptr<ast::StructDecl> forward_ptr = forward;
		forward->body = false;
		forward->members.clear();
		forward_ptr.release();
	}

	ast::ObjectDecl * this_decl = new ast::ObjectDecl(
		location,
		"this",
		new ast::ReferenceType( new ast::StructInstType( decl ) )
	);

	ast::ObjectDecl * ret_decl = new ast::ObjectDecl(
		location,
		"ret",
		new ast::PointerType( new ast::StructInstType( type_decl ) )
	);

	ast::FunctionDecl * get_decl = new ast::FunctionDecl(
		location,
		getter_name,
		{}, // forall
		{ this_decl }, // params
		{ ret_decl }, // returns
		nullptr, // stmts
		ast::Storage::Static,
		ast::Linkage::Cforall,
		{ new ast::Attribute( "const" ) },
		ast::Function::Inline
	);
	get_decl = fixupGenerics( get_decl, decl );

	ast::FunctionDecl * main_decl = nullptr;
	if ( needs_main ) {
		// `this_decl` is copied here because the original was used above.
		main_decl = new ast::FunctionDecl(
			location,
			"main",
			{},
			{ ast::deepCopy( this_decl ) },
			{},
			nullptr,
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		);
		main_decl = fixupGenerics( main_decl, decl );
	}

	declsToAddBefore.push_back( forward );
	if ( needs_main ) declsToAddBefore.push_back( main_decl );
	declsToAddBefore.push_back( get_decl );

	return get_decl;
}

ConcurrentSueKeyword::StructAndField ConcurrentSueKeyword::addField(
		const ast::StructDecl * decl ) {
	const CodeLocation & location = decl->location;

	ast::ObjectDecl * field = new ast::ObjectDecl(
		location,
		field_name,
		new ast::StructInstType( type_decl )
	);

	auto mutDecl = ast::mutate( decl );
	mutDecl->members.push_back( field );

	return {mutDecl, field};
}

void ConcurrentSueKeyword::addGetRoutines(
		const ast::ObjectDecl * field, const ast::FunctionDecl * forward ) {
	// Say it is generated at the "same" places as the forward declaration.
	const CodeLocation & location = forward->location;

	const ast::DeclWithType * param = forward->params.front();
	ast::Stmt * stmt = new ast::ReturnStmt( location,
		new ast::AddressExpr( location,
			new ast::MemberExpr( location,
				field,
				new ast::CastExpr( location,
					new ast::VariableExpr( location, param ),
					ast::deepCopy( param->get_type()->stripReferences() ),
					ast::ExplicitCast
				)
			)
		)
	);

	ast::FunctionDecl * decl = ast::deepCopy( forward );
	decl->stmts = new ast::CompoundStmt( location, { stmt } );
	declsToAddAfter.push_back( decl );
}

void ConcurrentSueKeyword::addLockUnlockRoutines(
		const ast::StructDecl * decl ) {
	// This should only be used on monitors.
	assert( ast::AggregateDecl::Monitor == cast_target );

	const CodeLocation & location = decl->location;

	// The parameter for both routines.
	ast::ObjectDecl * this_decl = new ast::ObjectDecl(
		location,
		"this",
		new ast::ReferenceType( new ast::StructInstType( decl ) )
	);

	ast::FunctionDecl * lock_decl = new ast::FunctionDecl(
		location,
		"lock",
		{ /* forall */ },
		{
			// Copy the declaration of this.
			ast::deepCopy( this_decl ),
		},
		{ /* returns */ },
		nullptr,
		ast::Storage::Static,
		ast::Linkage::Cforall,
		{ /* attributes */ },
		ast::Function::Inline
	);
	lock_decl = fixupGenerics( lock_decl, decl );

	lock_decl->stmts = new ast::CompoundStmt( location, {
		new ast::ExprStmt( location,
			new ast::UntypedExpr( location,
				new ast::NameExpr( location, "lock" ),
				{
					new ast::UntypedExpr( location,
						new ast::NameExpr( location, "get_monitor" ),
						{ new ast::VariableExpr( location,
							InitTweak::getParamThis( lock_decl ) ) }
					)
				}
			)
		)
	} );

	ast::FunctionDecl * unlock_decl = new ast::FunctionDecl(
		location,
		"unlock",
		{ /* forall */ },
		{
			// Last use, consume the declaration of this.
			this_decl,
		},
		{ /* returns */ },
		nullptr,
		ast::Storage::Static,
		ast::Linkage::Cforall,
		{ /* attributes */ },
		ast::Function::Inline
	);
	unlock_decl = fixupGenerics( unlock_decl, decl );

	unlock_decl->stmts = new ast::CompoundStmt( location, {
		new ast::ExprStmt( location,
			new ast::UntypedExpr( location,
				new ast::NameExpr( location, "unlock" ),
				{
					new ast::UntypedExpr( location,
						new ast::NameExpr( location, "get_monitor" ),
						{ new ast::VariableExpr( location,
							InitTweak::getParamThis( unlock_decl ) ) }
					)
				}
			)
		)
	} );

	declsToAddAfter.push_back( lock_decl );
	declsToAddAfter.push_back( unlock_decl );
}


// --------------------------------------------------------------------------
struct SuspendKeyword final :
		public ast::WithStmtsToAdd<>, public ast::WithGuards {
	SuspendKeyword() = default;
	virtual ~SuspendKeyword() = default;

	void previsit( const ast::FunctionDecl * );
	const ast::DeclWithType * postvisit( const ast::FunctionDecl * );
	const ast::Stmt * postvisit( const ast::SuspendStmt * );

private:
	bool is_real_suspend( const ast::FunctionDecl * );

	const ast::Stmt * make_generator_suspend( const ast::SuspendStmt * );
	const ast::Stmt * make_coroutine_suspend( const ast::SuspendStmt * );

	struct LabelPair {
		ast::Label obj;
		int idx;
	};

	LabelPair make_label(const ast::Stmt * stmt ) {
		labels.push_back( ControlStruct::newLabel( "generator", stmt ) );
		return { labels.back(), int(labels.size()) };
	}

	const ast::DeclWithType * in_generator = nullptr;
	const ast::FunctionDecl * decl_suspend = nullptr;
	std::vector<ast::Label> labels;
};

void SuspendKeyword::previsit( const ast::FunctionDecl * decl ) {
	GuardValue( in_generator ); in_generator = nullptr;

	// If it is the real suspend, grab it if we don't have one already.
	if ( is_real_suspend( decl ) ) {
		decl_suspend = decl_suspend ? decl_suspend : decl;
		return;
	}

	// Otherwise check if this is a generator main and, if so, handle it.
	auto param = isMainFor( decl, ast::AggregateDecl::Generator );
	if ( !param ) return;

	if ( 0 != decl->returns.size() ) {
		SemanticError( decl->location, "Generator main must return void" );
	}

	in_generator = param;
	GuardValue( labels ); labels.clear();
}

const ast::DeclWithType * SuspendKeyword::postvisit(
		const ast::FunctionDecl * decl ) {
	// Only modify a full definition of a generator with states.
	if ( !decl->stmts || !in_generator || labels.empty() ) return decl;

	const CodeLocation & location = decl->location;

	// Create a new function body:
	// static void * __generator_labels[] = {&&s0, &&s1, ...};
	// void * __generator_label = __generator_labels[GEN.__generator_state];
	// goto * __generator_label;
	// s0: ;
	// OLD_BODY

	// This is the null statement inserted right before the body.
	ast::NullStmt * noop = new ast::NullStmt( location );
	noop->labels.push_back( ControlStruct::newLabel( "generator", noop ) );
	const ast::Label & first_label = noop->labels.back();

	// Add each label to the init, starting with the first label.
	std::vector<ast::ptr<ast::Init>> inits = {
		new ast::SingleInit( location,
			new ast::LabelAddressExpr( location, copy( first_label ) ) ) };
	// Then go through all the stored labels, and clear the store.
	for ( auto && label : labels ) {
		inits.push_back( new ast::SingleInit( label.location,
			new ast::LabelAddressExpr( label.location, std::move( label )
			) ) );
	}
	labels.clear();
	// Then construct the initializer itself.
	auto init = new ast::ListInit( location, std::move( inits ) );

	ast::ObjectDecl * generatorLabels = new ast::ObjectDecl(
		location,
		"__generator_labels",
		new ast::ArrayType(
			new ast::PointerType( new ast::VoidType() ),
			nullptr,
			ast::FixedLen,
			ast::DynamicDim
		),
		init,
		ast::Storage::Classes(),
		ast::Linkage::AutoGen
	);

	ast::ObjectDecl * generatorLabel = new ast::ObjectDecl(
		location,
		"__generator_label",
		new ast::PointerType( new ast::VoidType() ),
		new ast::SingleInit( location,
			new ast::UntypedExpr( location,
				new ast::NameExpr( location, "?[?]" ),
				{
					// TODO: Could be a variable expr.
					new ast::NameExpr( location, "__generator_labels" ),
					new ast::UntypedMemberExpr( location,
						new ast::NameExpr( location, "__generator_state" ),
						new ast::VariableExpr( location, in_generator )
					)
				}
			)
		),
		ast::Storage::Classes(),
		ast::Linkage::AutoGen
	);

	ast::BranchStmt * theGoTo = new ast::BranchStmt(
		location, new ast::VariableExpr( location, generatorLabel )
	);

	// The noop goes here in order.

	ast::CompoundStmt * body = new ast::CompoundStmt( location, {
		{ new ast::DeclStmt( location, generatorLabels ) },
		{ new ast::DeclStmt( location, generatorLabel ) },
		{ theGoTo },
		{ noop },
		{ decl->stmts },
	} );

	auto mutDecl = ast::mutate( decl );
	mutDecl->stmts = body;
	return mutDecl;
}

const ast::Stmt * SuspendKeyword::postvisit( const ast::SuspendStmt * stmt ) {
	switch ( stmt->kind ) {
	case ast::SuspendStmt::None:
		// Use the context to determain the implicit target.
		if ( in_generator ) {
			return make_generator_suspend( stmt );
		} else {
			return make_coroutine_suspend( stmt );
		}
	case ast::SuspendStmt::Coroutine:
		return make_coroutine_suspend( stmt );
	case ast::SuspendStmt::Generator:
		// Generator suspends must be directly in a generator.
		if ( !in_generator ) SemanticError( stmt->location, "'suspend generator' must be used inside main of generator type." );
		return make_generator_suspend( stmt );
	}
	assert( false );
	return stmt;
}

/// Find the real/official suspend declaration.
bool SuspendKeyword::is_real_suspend( const ast::FunctionDecl * decl ) {
	return ( !decl->linkage.is_mangled
		&& 0 == decl->params.size()
		&& 0 == decl->returns.size()
		&& "__cfactx_suspend" == decl->name );
}

const ast::Stmt * SuspendKeyword::make_generator_suspend(
		const ast::SuspendStmt * stmt ) {
	assert( in_generator );
	// Target code is:
	//   GEN.__generator_state = X;
	//   THEN
	//   return;
	//   __gen_X:;

	const CodeLocation & location = stmt->location;

	LabelPair label = make_label( stmt );

	// This is the context saving statement.
	stmtsToAddBefore.push_back( new ast::ExprStmt( location,
		new ast::UntypedExpr( location,
			new ast::NameExpr( location, "?=?" ),
			{
				new ast::UntypedMemberExpr( location,
					new ast::NameExpr( location, "__generator_state" ),
					new ast::VariableExpr( location, in_generator )
				),
				ast::ConstantExpr::from_int( location, label.idx ),
			}
		)
	) );

	// The THEN component is conditional (return is not).
	if ( stmt->then ) {
		stmtsToAddBefore.push_back( stmt->then.get() );
	}
	stmtsToAddBefore.push_back( new ast::ReturnStmt( location, nullptr ) );

	// The null statement replaces the old suspend statement.
	return new ast::NullStmt( location, { label.obj } );
}

const ast::Stmt * SuspendKeyword::make_coroutine_suspend(
		const ast::SuspendStmt * stmt ) {
	// The only thing we need from the old statement is the location.
	const CodeLocation & location = stmt->location;

	if ( !decl_suspend ) {
		SemanticError( location, "suspend keyword applied to coroutines requires coroutines to be in scope, add #include <coroutine.hfa>\n" );
	}
	if ( stmt->then ) {
		SemanticError( location, "Compound statement following coroutines is not implemented." );
	}

	return new ast::ExprStmt( location,
		new ast::UntypedExpr( location,
			ast::VariableExpr::functionPointer( location, decl_suspend ) )
	);
}

// --------------------------------------------------------------------------
struct MutexKeyword final : public ast::WithDeclsToAdd<> {
	const ast::FunctionDecl * postvisit( const ast::FunctionDecl * decl );
	void postvisit( const ast::StructDecl * decl );
	const ast::Stmt * postvisit( const ast::MutexStmt * stmt );

	static std::vector<const ast::DeclWithType *> findMutexArgs(
			const ast::FunctionDecl * decl, bool & first );
	static void validate( const ast::DeclWithType * decl );

	ast::CompoundStmt * addDtorStatements( const ast::FunctionDecl* func, const ast::CompoundStmt *, const std::vector<const ast::DeclWithType *> &);
	ast::CompoundStmt * addStatements( const ast::FunctionDecl* func, const ast::CompoundStmt *, const std::vector<const ast::DeclWithType *> &);
	ast::CompoundStmt * addStatements( const ast::CompoundStmt * body, const std::vector<ast::ptr<ast::Expr>> & args );
	ast::CompoundStmt * addThreadDtorStatements( const ast::FunctionDecl* func, const ast::CompoundStmt * body, const std::vector<const ast::DeclWithType *> & args );
	ast::ExprStmt * genVirtLockUnlockExpr( const std::string & fnName, ast::ptr<ast::Expr> expr, const CodeLocation & location, ast::Expr * param);
	ast::IfStmt * genTypeDiscrimLockUnlock( const std::string & fnName, const std::vector<ast::ptr<ast::Expr>> & args, const CodeLocation & location, ast::UntypedExpr * thisParam );
private:
	const ast::StructDecl * monitor_decl = nullptr;
	const ast::StructDecl * guard_decl = nullptr;
	const ast::StructDecl * dtor_guard_decl = nullptr;
	const ast::StructDecl * thread_guard_decl = nullptr;
	const ast::StructDecl * lock_guard_decl = nullptr;

	static ast::ptr<ast::Type> generic_func;

	UniqueName mutex_func_namer = UniqueName("__lock_unlock_curr");
};

const ast::FunctionDecl * MutexKeyword::postvisit(
		const ast::FunctionDecl * decl ) {
	bool is_first_argument_mutex = false;
	const std::vector<const ast::DeclWithType *> mutexArgs =
		findMutexArgs( decl, is_first_argument_mutex );
	bool const isDtor = CodeGen::isDestructor( decl->name );

	// Does this function have any mutex arguments that connect to monitors?
	if ( mutexArgs.empty() ) {
		// If this is the destructor for a monitor it must be mutex.
		if ( isDtor ) {
			// This reflects MutexKeyword::validate, but no error messages.
			const ast::Type * type = decl->type->params.front();

			// If it's a copy, it's not a mutex.
			const ast::ReferenceType * refType = dynamic_cast<const ast::ReferenceType *>( type );
			if ( nullptr == refType ) {
				return decl;
			}

			// If it is not pointing directly to a type, it's not a mutex.
			auto base = refType->base;
			if ( base.as<ast::ReferenceType>() ) return decl;
			if ( base.as<ast::PointerType>() ) return decl;

			// If it is not a struct, it's not a mutex.
			auto baseStruct = base.as<ast::StructInstType>();
			if ( nullptr == baseStruct ) return decl;

			// If it is a monitor, then it is a monitor.
			if( baseStruct->base->is_monitor() || baseStruct->base->is_thread() ) {
				SemanticError( decl, "destructors for structures declared as \"monitor\" must use mutex parameters\n" );
			}
		}
		return decl;
	}

	// Monitors can't be constructed with mutual exclusion.
	if ( CodeGen::isConstructor( decl->name ) && is_first_argument_mutex ) {
		SemanticError( decl, "constructors cannot have mutex parameters\n" );
	}

	// It makes no sense to have multiple mutex parameters for the destructor.
	if ( isDtor && mutexArgs.size() != 1 ) {
		SemanticError( decl, "destructors can only have 1 mutex argument\n" );
	}

	// Make sure all the mutex arguments are monitors.
	for ( auto arg : mutexArgs ) {
		validate( arg );
	}

	// Check to see if the body needs to be instrument the body.
	const ast::CompoundStmt * body = decl->stmts;
	if ( !body ) return decl;

	// Check to if the required headers have been seen.
	if ( !monitor_decl || !guard_decl || !dtor_guard_decl ) {
		SemanticError( decl, "mutex keyword requires monitors to be in scope, add #include <monitor.hfa>\n" );
	}

	// Instrument the body.
	ast::CompoundStmt * newBody = nullptr;
	if ( isDtor && isThread( mutexArgs.front() ) ) {
		if ( !thread_guard_decl ) {
			SemanticError( decl, "thread destructor requires threads to be in scope, add #include <thread.hfa>\n" );
		}
		newBody = addThreadDtorStatements( decl, body, mutexArgs );
	} else if ( isDtor ) {
		newBody = addDtorStatements( decl, body, mutexArgs );
	} else {
		newBody = addStatements( decl, body, mutexArgs );
	}
	assert( newBody );
	return ast::mutate_field( decl, &ast::FunctionDecl::stmts, newBody );
}

void MutexKeyword::postvisit( const ast::StructDecl * decl ) {
	if ( !decl->body ) {
		return;
	} else if ( decl->name == "monitor$" ) {
		assert( !monitor_decl );
		monitor_decl = decl;
	} else if ( decl->name == "monitor_guard_t" ) {
		assert( !guard_decl );
		guard_decl = decl;
	} else if ( decl->name == "monitor_dtor_guard_t" ) {
		assert( !dtor_guard_decl );
		dtor_guard_decl = decl;
	} else if ( decl->name == "thread_dtor_guard_t" ) {
		assert( !thread_guard_decl );
		thread_guard_decl = decl;
	} else if ( decl->name == "__mutex_stmt_lock_guard" ) {
		assert( !lock_guard_decl );
		lock_guard_decl = decl;
	}
}

const ast::Stmt * MutexKeyword::postvisit( const ast::MutexStmt * stmt ) {
	if ( !lock_guard_decl ) {
		SemanticError( stmt->location, "mutex stmt requires a header, add #include <mutex_stmt.hfa>\n" );
	}
	ast::CompoundStmt * body =
			new ast::CompoundStmt( stmt->location, { stmt->stmt } );
	
	return addStatements( body, stmt->mutexObjs );;
}

std::vector<const ast::DeclWithType *> MutexKeyword::findMutexArgs(
		const ast::FunctionDecl * decl, bool & first ) {
	std::vector<const ast::DeclWithType *> mutexArgs;

	bool once = true;
	for ( auto arg : decl->params ) {
		const ast::Type * type = arg->get_type();
		if ( type->is_mutex() ) {
			if ( once ) first = true;
			mutexArgs.push_back( arg.get() );
		}
		once = false;
	}
	return mutexArgs;
}

void MutexKeyword::validate( const ast::DeclWithType * decl ) {
	const ast::Type * type = decl->get_type();

	// If it's a copy, it's not a mutex.
	const ast::ReferenceType * refType = dynamic_cast<const ast::ReferenceType *>( type );
	if ( nullptr == refType ) {
		SemanticError( decl, "Mutex argument must be of reference type " );
	}

	// If it is not pointing directly to a type, it's not a mutex.
	auto base = refType->base;
	if ( base.as<ast::ReferenceType>() || base.as<ast::PointerType>() ) {
		SemanticError( decl, "Mutex argument have exactly one level of indirection " );
	}

	// If it is not a struct, it's not a mutex.
	auto baseStruct = base.as<ast::StructInstType>();
	if ( nullptr == baseStruct ) return;

	// Make sure that only the outer reference is mutex.
	if( baseStruct->is_mutex() ) {
		SemanticError( decl, "mutex keyword may only appear once per argument " );
	}
}

ast::CompoundStmt * MutexKeyword::addDtorStatements(
		const ast::FunctionDecl* func, const ast::CompoundStmt * body,
		const std::vector<const ast::DeclWithType *> & args ) {
	ast::Type * argType = ast::shallowCopy( args.front()->get_type() );
	argType->set_mutex( false );

	ast::CompoundStmt * mutBody = ast::mutate( body );

	// Generated code goes near the beginning of body:
	const CodeLocation & location = mutBody->location;

	const ast::ObjectDecl * monitor = new ast::ObjectDecl(
		location,
		"__monitor",
		new ast::PointerType( new ast::StructInstType( monitor_decl ) ),
		new ast::SingleInit(
			location,
			new ast::UntypedExpr(
				location,
				new ast::NameExpr( location, "get_monitor" ),
				{ new ast::CastExpr(
					location,
					new ast::VariableExpr( location, args.front() ),
					argType, ast::ExplicitCast
				) }
			)
		)
	);

	assert( generic_func );

	// In reverse order:
	// monitor_dtor_guard_t __guard = { __monitor, func, false };
	mutBody->push_front(
		new ast::DeclStmt( location, new ast::ObjectDecl(
			location,
			"__guard",
			new ast::StructInstType( dtor_guard_decl ),
			new ast::ListInit(
				location,
				{
					new ast::SingleInit( location,
						new ast::AddressExpr( location,
							new ast::VariableExpr( location, monitor ) ) ),
					new ast::SingleInit( location,
						new ast::CastExpr( location,
							new ast::VariableExpr( location, func ),
							generic_func,
							ast::ExplicitCast ) ),
					new ast::SingleInit( location,
						ast::ConstantExpr::from_bool( location, false ) ),
				},
				{},
				ast::MaybeConstruct
			)
		))
	);

	// monitor$ * __monitor = get_monitor(a);
	mutBody->push_front( new ast::DeclStmt( location, monitor ) );

	return mutBody;
}

ast::CompoundStmt * MutexKeyword::addStatements(
		const ast::FunctionDecl* func, const ast::CompoundStmt * body,
		const std::vector<const ast::DeclWithType * > & args ) {
	ast::CompoundStmt * mutBody = ast::mutate( body );

	// Code is generated near the beginning of the compound statement.
	const CodeLocation & location = mutBody->location;

	// Make pointer to the monitors.
	ast::ObjectDecl * monitors = new ast::ObjectDecl(
		location,
		"__monitors",
		new ast::ArrayType(
			new ast::PointerType(
				new ast::StructInstType( monitor_decl )
			),
			ast::ConstantExpr::from_ulong( location, args.size() ),
			ast::FixedLen,
			ast::DynamicDim
		),
		new ast::ListInit(
			location,
			map_range<std::vector<ast::ptr<ast::Init>>>(
				args,
				[]( const ast::DeclWithType * decl ) {
					return new ast::SingleInit(
						decl->location,
						new ast::UntypedExpr(
							decl->location,
							new ast::NameExpr( decl->location, "get_monitor" ),
							{
								new ast::CastExpr(
									decl->location,
									new ast::VariableExpr( decl->location, decl ),
									decl->get_type(),
									ast::ExplicitCast
								)
							}
						)
					);
				}
			)
		)
	);

	assert( generic_func );

	// In Reverse Order:
	mutBody->push_front(
		new ast::DeclStmt( location, new ast::ObjectDecl(
			location,
			"__guard",
			new ast::StructInstType( guard_decl ),
			new ast::ListInit(
				location,
				{
					new ast::SingleInit( location,
						new ast::VariableExpr( location, monitors ) ),
					new ast::SingleInit( location,
						ast::ConstantExpr::from_ulong( location, args.size() ) ),
					new ast::SingleInit( location, new ast::CastExpr(
						location,
						new ast::VariableExpr( location, func ),
						generic_func,
						ast::ExplicitCast
					) ),
				},
				{},
				ast::MaybeConstruct
			)
		))
	);

	// monitor$ * __monitors[] = { get_monitor(a), get_monitor(b) };
	mutBody->push_front( new ast::DeclStmt( location, monitors ) );

	return mutBody;
}

// generates a cast to the void ptr to the appropriate lock type and dereferences it before calling lock or unlock on it
// used to undo the type erasure done by storing all the lock pointers as void 
ast::ExprStmt * MutexKeyword::genVirtLockUnlockExpr( const std::string & fnName, ast::ptr<ast::Expr> expr, const CodeLocation & location, ast::Expr * param ) {
	return new ast::ExprStmt( location,
		new ast::UntypedExpr( location,
			new ast::NameExpr( location, fnName ), {
				ast::UntypedExpr::createDeref(
					location,
					new ast::CastExpr( location, 
						param,
						new ast::PointerType( new ast::TypeofType( new ast::UntypedExpr(
							expr->location,
							new ast::NameExpr( expr->location, "__get_mutexstmt_lock_type" ),
							{ expr }
						) ) ),
						ast::GeneratedFlag::ExplicitCast
					)
				)
			}
		)
	);
}

ast::IfStmt * MutexKeyword::genTypeDiscrimLockUnlock( const std::string & fnName, const std::vector<ast::ptr<ast::Expr>> & args, const CodeLocation & location, ast::UntypedExpr * thisParam ) {
	ast::IfStmt * outerLockIf = nullptr;
	ast::IfStmt * lastLockIf = nullptr;

	//adds an if/elif clause for each lock to assign type from void ptr based on ptr address
	for ( long unsigned int i = 0; i < args.size(); i++ ) {
		
		ast::UntypedExpr * ifCond = new ast::UntypedExpr( location,
			new ast::NameExpr( location, "?==?" ), {
				ast::deepCopy( thisParam ),
				new ast::CastExpr( location, new ast::AddressExpr( location, args.at(i) ), new ast::PointerType( new ast::VoidType() ))
			}
		);

		ast::IfStmt * currLockIf = new ast::IfStmt( 
			location,
			ifCond,
			genVirtLockUnlockExpr( fnName, args.at(i), location, ast::deepCopy( thisParam ) )
		);
		
		if ( i == 0 ) {
			outerLockIf = currLockIf;
		} else {
			// add ifstmt to else of previous stmt
			lastLockIf->else_ = currLockIf;
		}

		lastLockIf = currLockIf;
	}
	return outerLockIf;
}

void flattenTuple( const ast::UntypedTupleExpr * tuple, std::vector<ast::ptr<ast::Expr>> & output ) {
    for ( auto & expr : tuple->exprs ) {
        const ast::UntypedTupleExpr * innerTuple = dynamic_cast<const ast::UntypedTupleExpr *>(expr.get());
        if ( innerTuple ) flattenTuple( innerTuple, output );
        else output.emplace_back( ast::deepCopy( expr ));
    }
}

ast::CompoundStmt * MutexKeyword::addStatements(
		const ast::CompoundStmt * body,
		const std::vector<ast::ptr<ast::Expr>> & args ) {

	// Code is generated near the beginning of the compound statement.
	const CodeLocation & location = body->location;

		// final body to return
	ast::CompoundStmt * newBody = new ast::CompoundStmt( location );

	// std::string lockFnName = mutex_func_namer.newName();
	// std::string unlockFnName = mutex_func_namer.newName();

    // If any arguments to the mutex stmt are tuples, flatten them
    std::vector<ast::ptr<ast::Expr>> flattenedArgs;
    for ( auto & arg : args ) {
        const ast::UntypedTupleExpr * tuple = dynamic_cast<const ast::UntypedTupleExpr *>(args.at(0).get());
        if ( tuple ) flattenTuple( tuple, flattenedArgs );
        else flattenedArgs.emplace_back( ast::deepCopy( arg ));
    }

	// Make pointer to the monitors.
	ast::ObjectDecl * monitors = new ast::ObjectDecl(
		location,
		"__monitors",
		new ast::ArrayType(
			new ast::PointerType(
				new ast::VoidType()
			),
			ast::ConstantExpr::from_ulong( location, flattenedArgs.size() ),
			ast::FixedLen,
			ast::DynamicDim
		),
		new ast::ListInit(
			location,
			map_range<std::vector<ast::ptr<ast::Init>>>(
				flattenedArgs, [](const ast::Expr * expr) {
					return new ast::SingleInit(
						expr->location,
						new ast::UntypedExpr(
							expr->location,
							new ast::NameExpr( expr->location, "__get_mutexstmt_lock_ptr" ),
							{ expr }
						)
					);
				}
			)
		)
	);

	ast::StructInstType * lock_guard_struct =
			new ast::StructInstType( lock_guard_decl );

	// use try stmts to lock and finally to unlock
	ast::TryStmt * outerTry = nullptr;
	ast::TryStmt * currentTry;
	ast::CompoundStmt * lastBody = nullptr;

	// adds a nested try stmt for each lock we are locking
	for ( long unsigned int i = 0; i < flattenedArgs.size(); i++ ) {
		ast::UntypedExpr * innerAccess = new ast::UntypedExpr( 
			location,
			new ast::NameExpr( location,"?[?]" ), {
				new ast::NameExpr( location, "__monitors" ),
				ast::ConstantExpr::from_int( location, i )
			}
		);

		// make the try body
		ast::CompoundStmt * currTryBody = new ast::CompoundStmt( location );
		ast::IfStmt * lockCall = genTypeDiscrimLockUnlock( "lock", flattenedArgs, location, innerAccess );
		currTryBody->push_back( lockCall );

		// make the finally stmt
		ast::CompoundStmt * currFinallyBody = new ast::CompoundStmt( location );
		ast::IfStmt * unlockCall = genTypeDiscrimLockUnlock( "unlock", flattenedArgs, location, innerAccess );
		currFinallyBody->push_back( unlockCall );

		// construct the current try
		currentTry = new ast::TryStmt(
			location,
			currTryBody,
			{},
			new ast::FinallyClause( location, currFinallyBody )
		);
		if ( i == 0 ) outerTry = currentTry;
		else {
			// pushback try into the body of the outer try
			lastBody->push_back( currentTry );
		}
		lastBody = currTryBody;
	}

	// push body into innermost try body
	if ( lastBody != nullptr ) {
		lastBody->push_back( body );
		newBody->push_front( outerTry );
	}

	// monitor_guard_t __guard = { __monitors, # };
	newBody->push_front(
		new ast::DeclStmt(
			location,
			new ast::ObjectDecl(
				location,
				"__guard",
				lock_guard_struct,
				new ast::ListInit(
					location,
					{
						new ast::SingleInit(
							location,
							new ast::VariableExpr( location, monitors ) ),
						new ast::SingleInit(
							location,
							ast::ConstantExpr::from_ulong( location, flattenedArgs.size() ) ),
					},
					{},
					ast::MaybeConstruct
				)
			)
		)
	);

	// monitor$ * __monitors[] = { get_monitor(a), get_monitor(b) };
	newBody->push_front( new ast::DeclStmt( location, monitors ) );

	// // The parameter for both __lock_curr/__unlock_curr routines.
	// ast::ObjectDecl * this_decl = new ast::ObjectDecl(
	// 	location,
	// 	"this",
	// 	new ast::PointerType( new ast::VoidType() ),
	// 	nullptr,
	// 	{},
	// 	ast::Linkage::Cforall
	// );

	// ast::FunctionDecl * lock_decl = new ast::FunctionDecl(
	// 	location,
	// 	lockFnName,
	// 	{ /* forall */ },
	// 	{
	// 		// Copy the declaration of this.
	// 		this_decl,
	// 	},
	// 	{ /* returns */ },
	// 	nullptr,
	// 	0,
	// 	ast::Linkage::Cforall,
	// 	{ /* attributes */ },
	// 	ast::Function::Inline
	// );

	// ast::FunctionDecl * unlock_decl = new ast::FunctionDecl(
	// 	location,
	// 	unlockFnName,
	// 	{ /* forall */ },
	// 	{
	// 		// Copy the declaration of this.
	// 		ast::deepCopy( this_decl ),
	// 	},
	// 	{ /* returns */ },
	// 	nullptr,
	// 	0,
	// 	ast::Linkage::Cforall,
	// 	{ /* attributes */ },
	// 	ast::Function::Inline
	// );

	// ast::IfStmt * outerLockIf = nullptr;
	// ast::IfStmt * outerUnlockIf = nullptr;
	// ast::IfStmt * lastLockIf = nullptr;
	// ast::IfStmt * lastUnlockIf = nullptr;

	// //adds an if/elif clause for each lock to assign type from void ptr based on ptr address
	// for ( long unsigned int i = 0; i < args.size(); i++ ) {
	// 	ast::VariableExpr * thisParam = new ast::VariableExpr( location, InitTweak::getParamThis( lock_decl ) );
	// 	ast::UntypedExpr * ifCond = new ast::UntypedExpr( location,
	// 		new ast::NameExpr( location, "?==?" ), {
	// 			thisParam,
	// 			new ast::CastExpr( location, new ast::AddressExpr( location, args.at(i) ), new ast::PointerType( new ast::VoidType() ))
	// 		}
	// 	);

	// 	ast::IfStmt * currLockIf = new ast::IfStmt( 
	// 		location,
	// 		ast::deepCopy( ifCond ),
	// 		genVirtLockUnlockExpr( "lock", args.at(i), location, ast::deepCopy( thisParam ) )
	// 	);

	// 	ast::IfStmt * currUnlockIf = new ast::IfStmt( 
	// 		location,
	// 		ifCond,
	// 		genVirtLockUnlockExpr( "unlock", args.at(i), location, ast::deepCopy( thisParam ) )
	// 	);
		
	// 	if ( i == 0 ) {
	// 		outerLockIf = currLockIf;
	// 		outerUnlockIf = currUnlockIf;
	// 	} else {
	// 		// add ifstmt to else of previous stmt
	// 		lastLockIf->else_ = currLockIf;
	// 		lastUnlockIf->else_ = currUnlockIf;
	// 	}

	// 	lastLockIf = currLockIf;
	// 	lastUnlockIf = currUnlockIf;
	// }
	
	// // add pointer typing if/elifs to body of routines
	// lock_decl->stmts = new ast::CompoundStmt( location, { outerLockIf } );
	// unlock_decl->stmts = new ast::CompoundStmt( location, { outerUnlockIf } );

	// // add routines to scope
	// declsToAddBefore.push_back( lock_decl );
	// declsToAddBefore.push_back( unlock_decl );

	// newBody->push_front(new ast::DeclStmt( location, lock_decl ));
	// newBody->push_front(new ast::DeclStmt( location, unlock_decl ));

	return newBody;
}

ast::CompoundStmt * MutexKeyword::addThreadDtorStatements(
		const ast::FunctionDecl*, const ast::CompoundStmt * body,
		const std::vector<const ast::DeclWithType * > & args ) {
	assert( args.size() == 1 );
	const ast::DeclWithType * arg = args.front();
	const ast::Type * argType = arg->get_type();
	assert( argType->is_mutex() );

	ast::CompoundStmt * mutBody = ast::mutate( body );

	// The code is generated near the front of the body.
	const CodeLocation & location = mutBody->location;

	// thread_dtor_guard_t __guard = { this, intptr( 0 ) };
	mutBody->push_front( new ast::DeclStmt(
		location,
		new ast::ObjectDecl(
			location,
			"__guard",
			new ast::StructInstType( thread_guard_decl ),
			new ast::ListInit(
				location,
				{
					new ast::SingleInit( location,
						new ast::CastExpr( location,
							new ast::VariableExpr( location, arg ), argType ) ),
					new ast::SingleInit(
						location,
						new ast::UntypedExpr(
							location,
							new ast::NameExpr( location, "intptr" ), {
								ast::ConstantExpr::from_int( location, 0 ),
							}
						) ),
				},
				{},
				ast::MaybeConstruct
			)
		)
	));

	return mutBody;
}

ast::ptr<ast::Type> MutexKeyword::generic_func =
	new ast::FunctionType( ast::VariableArgs );

// --------------------------------------------------------------------------
struct ThreadStarter final {
	void previsit( const ast::StructDecl * decl );
	const ast::FunctionDecl * postvisit( const ast::FunctionDecl * decl );

private:
	bool thread_ctor_seen = false;
	const ast::StructDecl * thread_decl = nullptr;
};

void ThreadStarter::previsit( const ast::StructDecl * decl ) {
	if ( decl->body && decl->name == "thread$" ) {
		assert( !thread_decl );
		thread_decl = decl;
	}
}

const ast::FunctionDecl * ThreadStarter::postvisit( const ast::FunctionDecl * decl ) {
	if ( !CodeGen::isConstructor( decl->name ) ) return decl;

	// Seach for the thread constructor.
	// (Are the "prefixes" of these to blocks the same?)
	const ast::Type * typeof_this = InitTweak::getTypeofThis( decl->type );
	auto ctored_type = dynamic_cast<const ast::StructInstType *>( typeof_this );
	if ( ctored_type && ctored_type->base == thread_decl ) {
		thread_ctor_seen = true;
	}

	// Modify this declaration, the extra checks to see if we will are first.
	const ast::ptr<ast::DeclWithType> & param = decl->params.front();
	auto type = dynamic_cast<const ast::StructInstType *>(
		ast::getPointerBase( param->get_type() ) );
	if ( nullptr == type ) return decl;
	if ( !type->base->is_thread() ) return decl;
	if ( !thread_decl || !thread_ctor_seen ) {
		SemanticError( type->base->location, "thread keyword requires threads to be in scope, add #include <thread.hfa>" );
	}
	const ast::CompoundStmt * stmt = decl->stmts;
	if ( nullptr == stmt ) return decl;

	// Now do the actual modification:
	ast::CompoundStmt * mutStmt = ast::mutate( stmt );
	const CodeLocation & location = mutStmt->location;
	mutStmt->push_back(
		new ast::ExprStmt(
			location,
			new ast::UntypedExpr(
				location,
				new ast::NameExpr( location, "__thrd_start" ),
				{
					new ast::VariableExpr( location, param ),
					new ast::NameExpr( location, "main" ),
				}
			)
		)
	);

	return ast::mutate_field( decl, &ast::FunctionDecl::stmts, mutStmt );
}

} // namespace

// --------------------------------------------------------------------------
// Interface Functions:

void implementKeywords( ast::TranslationUnit & translationUnit ) {
	ast::Pass<ThreadKeyword>::run( translationUnit );
	ast::Pass<CoroutineKeyword>::run( translationUnit );
	ast::Pass<MonitorKeyword>::run( translationUnit );
	ast::Pass<GeneratorKeyword>::run( translationUnit );
	ast::Pass<SuspendKeyword>::run( translationUnit );
}

void implementMutex( ast::TranslationUnit & translationUnit ) {
	ast::Pass<MutexKeyword>::run( translationUnit );
}

void implementThreadStarter( ast::TranslationUnit & translationUnit ) {
	ast::Pass<ThreadStarter>::run( translationUnit );
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
