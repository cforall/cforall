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
// Last Modified On : Wed Dec  1 11:24:00 2021
// Update Count     : 1
//

#include "Concurrency/Keywords.h"

#include "AST/Copy.hpp"
#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/TranslationUnit.hpp"
#include "CodeGen/OperatorTable.h"
#include "Common/utility.h"
#include "InitTweak/InitTweak.h"

namespace Concurrency {

namespace {

inline static bool isThread( const ast::DeclWithType * decl ) {
	auto baseType = decl->get_type()->stripDeclarator();
	auto instType = dynamic_cast<const ast::StructInstType *>( baseType );
	if ( nullptr == instType ) { return false; }
	return instType->base->is_thread();
}

// --------------------------------------------------------------------------
struct MutexKeyword final {
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

private:
	const ast::StructDecl * monitor_decl = nullptr;
	const ast::StructDecl * guard_decl = nullptr;
	const ast::StructDecl * dtor_guard_decl = nullptr;
	const ast::StructDecl * thread_guard_decl = nullptr;
	const ast::StructDecl * lock_guard_decl = nullptr;

	static ast::ptr<ast::Type> generic_func;
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
	ast::CompoundStmt * body =
			new ast::CompoundStmt( stmt->location, { stmt->stmt } );
	addStatements( body, stmt->mutexObjs );
	return body;
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
		),
		ast::Storage::Classes(),
		ast::Linkage::Cforall
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
						new ast::AddressExpr(
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
			),
			ast::Storage::Classes(),
			ast::Linkage::Cforall
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
		),
		ast::Storage::Classes(),
		ast::Linkage::Cforall
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
			),
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		))
	);

	// monitor$ * __monitors[] = { get_monitor(a), get_monitor(b) };
	mutBody->push_front( new ast::DeclStmt( location, monitors ) );

	return mutBody;
}

ast::CompoundStmt * MutexKeyword::addStatements(
		const ast::CompoundStmt * body,
		const std::vector<ast::ptr<ast::Expr>> & args ) {
	ast::CompoundStmt * mutBody = ast::mutate( body );

	// Code is generated near the beginning of the compound statement.
	const CodeLocation & location = mutBody->location;

	// Make pointer to the monitors.
	ast::ObjectDecl * monitors = new ast::ObjectDecl(
		location,
		"__monitors",
		new ast::ArrayType(
			new ast::PointerType(
				new ast::TypeofType(
					new ast::UntypedExpr(
						location,
						new ast::NameExpr( location, "__get_type" ),
						{ args.front() }
					)
				)
			),
			ast::ConstantExpr::from_ulong( location, args.size() ),
			ast::FixedLen,
			ast::DynamicDim
		),
		new ast::ListInit(
			location,
			map_range<std::vector<ast::ptr<ast::Init>>>(
				args, [](const ast::Expr * expr) {
					return new ast::SingleInit(
						expr->location,
						new ast::UntypedExpr(
							expr->location,
							new ast::NameExpr( expr->location, "__get_ptr" ),
							{ expr }
						)
					);
				}
			)
		),
		ast::Storage::Classes(),
		ast::Linkage::Cforall
	);

	ast::StructInstType * lock_guard_struct =
			new ast::StructInstType( lock_guard_decl );
	ast::TypeExpr * lock_type_expr = new ast::TypeExpr(
		location,
		new ast::TypeofType(
			new ast::UntypedExpr(
				location,
				new ast::NameExpr( location, "__get_type" ),
				{ args.front() }
			)
		)
	);

	lock_guard_struct->params.push_back( lock_type_expr );

	// In reverse order:
	// monitor_guard_t __guard = { __monitors, # };
	mutBody->push_front(
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
							ast::ConstantExpr::from_ulong( location, args.size() ) ),
					},
					{},
					ast::MaybeConstruct
				),
				ast::Storage::Classes(),
				ast::Linkage::Cforall
			)
		)
	);

	// monitor$ * __monitors[] = { get_monitor(a), get_monitor(b) };
	mutBody->push_front( new ast::DeclStmt( location, monitors ) );

	return mutBody;
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
			),
			ast::Storage::Classes(),
			ast::Linkage::Cforall
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
		InitTweak::getPointerBase( param->get_type() ) );
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

void implementKeywords( ast::TranslationUnit & translationUnit ) {
	(void)translationUnit;
	assertf(false, "Apply Keywords not implemented." );
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
