//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExceptTranslate.cpp -- Conversion of exception control flow structures.
//
// Author           : Andrew Beach
// Created On       : Mon Nov  8 11:53:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Fri Mar 11 17:51:00 2022
// Update Count     : 2
//

#include "ExceptTranslate.hpp"

#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/TranslationUnit.hpp"
#include "AST/DeclReplacer.hpp"

namespace ControlStruct {

namespace {

typedef std::list<ast::CatchClause*> CatchList;

void appendDeclStmt( ast::CompoundStmt * block, ast::DeclWithType * item ) {
	block->push_back( new ast::DeclStmt( block->location, item ) );
}

class TranslateThrowsCore final : public ast::WithGuards {
	const ast::ObjectDecl * terminateHandlerExcept;
	enum Context { NoHandler, TerHandler, ResHandler } currentContext;

	const ast::Stmt * createEitherThrow(
		const ast::ThrowStmt * throwStmt, const char * funcName );
	const ast::Stmt * createTerminateRethrow( const ast::ThrowStmt * );

public:
	TranslateThrowsCore() :
		terminateHandlerExcept( nullptr ), currentContext( NoHandler )
	{}

	void previsit( const ast::CatchClause * stmt );
	const ast::Stmt * postvisit( const ast::ThrowStmt * stmt );
};

const ast::Stmt * TranslateThrowsCore::createEitherThrow(
		const ast::ThrowStmt * throwStmt, const char * funcName ) {
	// `throwFunc`( `throwStmt->name` );
	ast::UntypedExpr * call = new ast::UntypedExpr( throwStmt->location,
		new ast::NameExpr( throwStmt->location, funcName )
	);
	call->args.push_back( throwStmt->expr );
	return new ast::ExprStmt( throwStmt->location, call );
}

ast::VariableExpr * varOf( const ast::DeclWithType * decl ) {
	return new ast::VariableExpr( decl->location, decl );
}

const ast::Stmt * TranslateThrowsCore::createTerminateRethrow(
		const ast::ThrowStmt * stmt ) {
	// { `terminate_handler_except` = 0p; __rethrow_terminate(); }
	assert( nullptr == stmt->expr );
	assert( terminateHandlerExcept );

	ast::CompoundStmt * result = new ast::CompoundStmt(
		stmt->location, {}, std::vector<ast::Label>( stmt->labels ) );
	result->push_back( new ast::ExprStmt( stmt->location,
		ast::UntypedExpr::createAssign(
			stmt->location,
			varOf( terminateHandlerExcept ),
			ast::ConstantExpr::null(
				stmt->location,
				terminateHandlerExcept->type
			)
		)
	) );
	result->push_back( new ast::ExprStmt( stmt->location, new ast::UntypedExpr(
		stmt->location,
		new ast::NameExpr( stmt->location, "__cfaehm_rethrow_terminate" )
	) ) );
	return result;
}

void TranslateThrowsCore::previsit( const ast::CatchClause * stmt ) {
	// Validate the statement's form.
	const ast::ObjectDecl * decl = stmt->decl.as<ast::ObjectDecl>();
	// Also checking the type would be nice.
	if ( !decl || !decl->type.as<ast::PointerType>() ) {
		std::string kind = (ast::Terminate == stmt->kind) ? "catch" : "catchResume";
		SemanticError( stmt->location, kind + " must have pointer to an exception type" );
	}

	// Track the handler context.
	if ( ast::Terminate == stmt->kind ) {
		GuardValue( currentContext ) = TerHandler;
		GuardValue( terminateHandlerExcept ) = decl;
	} else {
		GuardValue( currentContext ) = ResHandler;
	}
}

const ast::Stmt * TranslateThrowsCore::postvisit(
		const ast::ThrowStmt * stmt ) {
	// Ignoring ThrowStmt::target for now.
	// Handle Termination (Raise, Reraise, Error):
	if ( ast::Terminate == stmt->kind ) {
		if ( stmt->expr ) {
			return createEitherThrow( stmt, "$throw" );
		} else if ( TerHandler == currentContext ) {
			return createTerminateRethrow( stmt );
		} else {
			abort( "Invalid throw in %s at %i\n",
				stmt->location.filename.c_str(),
				stmt->location.first_line);
		}
	// Handle Resumption (Raise, Reraise, Error):
	} else {
		if ( stmt->expr ) {
			return createEitherThrow( stmt, "$throwResume" );
		} else if ( ResHandler == currentContext ) {
			// This has to be handled later.
			return stmt;
		} else {
			abort( "Invalid throwResume in %s at %i\n",
				stmt->location.filename.c_str(),
				stmt->location.first_line);
		}
	}
}


class TryMutatorCore final {
	// The built in types used in translation.
	const ast::StructDecl * except_decl;
	const ast::StructDecl * node_decl;
	const ast::StructDecl * hook_decl;

	// The many helper functions for code/syntree generation.
	ast::CompoundStmt * take_try_block( ast::TryStmt * tryStmt );
	ast::FunctionDecl * create_try_wrapper( const ast::CompoundStmt * body );
	ast::CaseClause * create_terminate_catch_case(
		const ast::DeclWithType * except_obj, int index, ast::CatchClause * clause );
	ast::CompoundStmt * create_single_matcher(
		const ast::DeclWithType * except_obj, ast::CatchClause * modded_handler );
	ast::FunctionDecl * create_terminate_match( CatchList &handlers );
	ast::CompoundStmt * create_terminate_caller( const CodeLocation & location,
		ast::FunctionDecl * try_wrapper, ast::FunctionDecl * terminate_match,
		CatchList & terminate_handlers );
	ast::FunctionDecl * create_resume_handler( CatchList &handlers );
	ast::CompoundStmt * create_resume_wrapper(
		const ast::Stmt * wraps, const ast::FunctionDecl * resume_handler );
	ast::FunctionDecl * create_finally_wrapper( ast::TryStmt * tryStmt );
	ast::ObjectDecl * create_finally_hook( ast::FunctionDecl * finally_wrapper );
	ast::Stmt * create_resume_rethrow( const ast::ThrowStmt * throwStmt );

	// Types used in translation, first group are internal.
	ast::ObjectDecl * make_index_object( CodeLocation const & ) const;
	ast::ObjectDecl * make_exception_object( CodeLocation const & ) const;
	ast::ObjectDecl * make_bool_object( CodeLocation const & ) const;
	ast::ObjectDecl * make_voidptr_object( CodeLocation const & ) const;
	ast::ObjectDecl * make_unused_index_object( CodeLocation const & ) const;

public:
	TryMutatorCore() :
		except_decl( nullptr ), node_decl( nullptr ), hook_decl( nullptr )
	{}

	void previsit( const ast::StructDecl *structDecl );
	ast::Stmt * postvisit( const ast::TryStmt *tryStmt );
	ast::Stmt * postvisit( const ast::ThrowStmt *throwStmt );
};

ast::ObjectDecl * TryMutatorCore::make_index_object(
		CodeLocation const & location ) const {
	return new ast::ObjectDecl(
		location,
		"__handler_index",
		new ast::BasicType( ast::BasicKind::SignedInt )
		);
}

ast::ObjectDecl * TryMutatorCore::make_exception_object(
		CodeLocation const & location ) const {
	assert( except_decl );
	return new ast::ObjectDecl(
		location,
		"__exception_inst",
		new ast::PointerType( new ast::StructInstType( except_decl ) )
		);
}

ast::ObjectDecl * TryMutatorCore::make_bool_object(
		CodeLocation const & location ) const {
	return new ast::ObjectDecl(
		location,
		"__ret_bool",
		new ast::BasicType( ast::BasicKind::Bool ),
		nullptr, //init
		ast::Storage::Classes{},
		ast::Linkage::Cforall,
		nullptr, //width
		std::vector<ast::ptr<ast::Attribute>>{ new ast::Attribute( "unused" ) }
		);
}

ast::ObjectDecl * TryMutatorCore::make_voidptr_object(
		CodeLocation const & location ) const {
	return new ast::ObjectDecl(
		location,
		"__hook",
		new ast::PointerType(
			new ast::VoidType()
		),
		nullptr, //init
		ast::Storage::Classes{},
		ast::Linkage::Cforall,
		nullptr, //width
		std::vector<ast::ptr<ast::Attribute>>{ new ast::Attribute( "unused" ) }
		);
}

ast::ObjectDecl * TryMutatorCore::make_unused_index_object(
		CodeLocation const & location ) const {
	return new ast::ObjectDecl(
		location,
		"__handler_index",
		new ast::BasicType( ast::BasicKind::SignedInt ),
		nullptr,
		ast::Storage::Classes{},
		ast::Linkage::Cforall,
		nullptr, //width
		std::vector<ast::ptr<ast::Attribute>>{ new ast::Attribute( "unused" ) }
	);
}

// TryStmt Mutation Helpers

ast::FunctionDecl * TryMutatorCore::create_try_wrapper(
		const ast::CompoundStmt *body ) {
	// void (*try)(void) `body`
	return new ast::FunctionDecl(
		body->location,
		"try",
		{}, //no param
		{}, //no return
		ast::mutate( body ),
		ast::Storage::Classes{},
		ast::Linkage::Cforall
	);
}

ast::CaseClause * TryMutatorCore::create_terminate_catch_case(
		const ast::DeclWithType * except_obj, int index, ast::CatchClause * clause ) {
	// case `index`:
	// {
	//     __attribute__((cleanup(__cfaehm_cleanup_terminate)))
	//     `handler.decl` = { (virtual `decl.type`)`except` };
	//     `handler.body`;
	// }
	// break;
	const CodeLocation & location = clause->location;

	auto clause_except = clause->decl.strict_as<ast::ObjectDecl>();
	auto local_except = ast::deepCopy( clause_except );
	local_except->attributes.push_back( new ast::Attribute(
		"cleanup",
		{ new ast::NameExpr( location, "__cfaehm_cleanup_terminate" ) }
	) );

	local_except->init = new ast::ListInit(	location, {
		new ast::SingleInit( location,
			new ast::VirtualCastExpr( location,
				new ast::VariableExpr( location, except_obj ),
				ast::deepCopy( local_except->get_type() )
			)
		)
	} );

	ast::DeclReplacer::DeclMap mapping;
	mapping[clause_except] = local_except;
	const ast::Stmt * body = strict_dynamic_cast<const ast::Stmt *>(
		ast::DeclReplacer::replace(clause->body, mapping));

	return new ast::CaseClause( location,
		ast::ConstantExpr::from_int( location, index ),
		{
			new ast::CompoundStmt( location, {
				new ast::DeclStmt( location, local_except ),
				body,
			} ),
			new ast::BranchStmt( location, ast::BranchStmt::Break,
				ast::Label( location ) ),
		}
	);
}

// Create a single check from a moddified handler.
// except_obj is referenced, modded_handler will be freed.
ast::CompoundStmt * TryMutatorCore::create_single_matcher(
		const ast::DeclWithType * except_obj, ast::CatchClause * modded_handler ) {
	// {
	//     `modded_handler.decl`
	//     if ( `decl.name = (virtual `decl.type`)`except`
	//             [&& `modded_handler.cond`] ) {
	//         `modded_handler.body`
	//     }
	// }

	const CodeLocation loc = modded_handler->location;
	ast::CompoundStmt * block = new ast::CompoundStmt(loc);

	// Local Declaration
	const ast::ObjectDecl * local_except =
		modded_handler->decl.strict_as<ast::ObjectDecl>();
	block->push_back( new ast::DeclStmt( loc,  local_except ) );

	// Check for type match.
	ast::VirtualCastExpr * vcex = new ast::VirtualCastExpr(loc,
		new ast::VariableExpr(loc, except_obj ),
		local_except->get_type()
		);
	ast::Expr * cond = ast::UntypedExpr::createAssign(loc,
		new ast::VariableExpr(loc, local_except ), vcex );

	// Add the check on the conditional if it is provided.
	if ( modded_handler->cond ) {
		cond = new ast::LogicalExpr( loc, cond, modded_handler->cond, ast::LogicalFlag::AndExpr );
	}
	// Construct the match condition.
	block->push_back( new ast::IfStmt(loc,
		cond, modded_handler->body, nullptr ) );

	return block;
}

ast::FunctionDecl * TryMutatorCore::create_terminate_match(
		CatchList &handlers ) {
	// int match(exception * except) {
	//     HANDLER WRAPPERS { return `index`; }
	// }

	assert( !handlers.empty() );
	const CodeLocation & location = handlers.front()->location;

	ast::CompoundStmt * body = new ast::CompoundStmt( location );
	const ast::DeclWithType * except_obj = make_exception_object( location );

	// Index 1..{number of handlers}
	int index = 0;
	for ( ast::CatchClause * handler : handlers ) {
		++index;

		ast::ptr<ast::Stmt> other_body = new ast::ReturnStmt( handler->location,
			ast::ConstantExpr::from_int( handler->location, index ) );
		handler->body.swap( other_body );

		body->push_back( create_single_matcher( except_obj, handler ) );

		handler->body.swap( other_body );
	}

	body->push_back( new ast::ReturnStmt( location,
		ast::ConstantExpr::from_int( location, 0 ) ));

	// void (*match)(exception_t *) `body`
	return new ast::FunctionDecl(
		location,
		"match",
		{ except_obj },
		{ make_unused_index_object( location ) },
		body,
		ast::Storage::Classes{},
		ast::Linkage::Cforall
	);
}

ast::CompoundStmt * TryMutatorCore::create_terminate_caller(
		const CodeLocation & loc,
		ast::FunctionDecl * try_wrapper,
		ast::FunctionDecl * terminate_match,
		CatchList & terminate_handlers ) {
	// {
	//     int __handler_index = __cfaehm_try_terminate(`try`, `match`);
	//     if ( __handler_index ) {
	//         `catch`( __handler_index, __cfaehm_get_current_termination() );
	//     }
	// }

	ast::ObjectDecl * index = make_index_object( loc );
	index->init = new ast::SingleInit( loc,
		new ast::UntypedExpr( loc,
			new ast::NameExpr( loc, "__cfaehm_try_terminate" ),
			{
				new ast::VariableExpr( loc, try_wrapper ),
				new ast::VariableExpr( loc, terminate_match ),
			}
		)
	);

	ast::ObjectDecl * except = make_exception_object( loc );
	except->init = new ast::SingleInit( loc,
		new ast::UntypedExpr( loc,
			new ast::NameExpr( loc, "__cfaehm_get_current_termination" )
		)
	);

	std::vector<ast::ptr<ast::CaseClause>> cases;
	for ( auto const & [index, handler] : enumerate( terminate_handlers ) ) {
		cases.emplace_back(
			create_terminate_catch_case( except, index + 1, handler ) );
	}
	auto switch_stmt = new ast::SwitchStmt( loc,
		new ast::VariableExpr( loc, index ), std::move( cases ) );

	return new ast::CompoundStmt( loc, {
		new ast::DeclStmt( loc, index ),
		new ast::IfStmt( loc,
			new ast::VariableExpr( loc, index ),
			new ast::CompoundStmt( loc, {
				new ast::DeclStmt( loc, except ),
				switch_stmt,
			} )
		),
	} );
}

ast::FunctionDecl * TryMutatorCore::create_resume_handler(
		CatchList &handlers ) {
	// bool handle(exception * except) {
	//     HANDLER WRAPPERS { `hander->body`; return true; }
	// }

	assert( !handlers.empty() );
	const CodeLocation & location = handlers.front()->location;

	ast::CompoundStmt * body = new ast::CompoundStmt( location );
	const ast::DeclWithType * except_obj = make_exception_object( location );

	CatchList::iterator it;
	for ( it = handlers.begin() ; it != handlers.end() ; ++it ) {
		ast::CatchClause * handler = *it;
		const CodeLocation loc = handler->location;
		// Modifiy body.
		ast::CompoundStmt * handling_code;
		if (handler->body.as<ast::CompoundStmt>()) {
			handling_code = strict_dynamic_cast<ast::CompoundStmt*>(
				handler->body.get_and_mutate() );
		} else {
			handling_code = new ast::CompoundStmt(loc);
			handling_code->push_back( handler->body );
		}
		handling_code->push_back( new ast::ReturnStmt(loc,
			ast::ConstantExpr::from_bool(loc, true ) ) );
		handler->body = handling_code;

		// Create the handler.
		body->push_back( create_single_matcher( except_obj, handler ) );
		*it = nullptr;
	}

	body->push_back( new ast::ReturnStmt( location,
		ast::ConstantExpr::from_bool( location, false ) ) );

	// bool (*handle)(exception_t *) `body`
	return new ast::FunctionDecl(
		location,
		"handle",
		{ except_obj },
		{ make_bool_object( location ) },
		body,
		ast::Storage::Classes{},
		ast::Linkage::Cforall
	);
}

ast::CompoundStmt * TryMutatorCore::create_resume_wrapper(
		const ast::Stmt * wraps,
		const ast::FunctionDecl * resume_handler ) {
	const CodeLocation loc = wraps->location;
	ast::CompoundStmt * body = new ast::CompoundStmt(loc);

	// struct __try_resume_node __resume_node
	//  	__attribute__((cleanup( __cfaehm_try_resume_cleanup )));
	// ** unwinding of the stack here could cause problems **
	// ** however I don't think that can happen currently **
	// __cfaehm_try_resume_setup( &__resume_node, resume_handler );

	ast::ObjectDecl * obj = new ast::ObjectDecl(
		loc,
		"__resume_node",
		new ast::StructInstType(
			node_decl
			),
		nullptr,
		ast::Storage::Classes{},
		ast::Linkage::Cforall,
		nullptr,
		{new ast::Attribute("cleanup", {new ast::NameExpr(loc, "__cfaehm_try_resume_cleanup")})}
		);
	appendDeclStmt( body, obj );

	ast::UntypedExpr *setup = new ast::UntypedExpr(loc, new ast::NameExpr(loc,
		"__cfaehm_try_resume_setup" ) );
	setup->args.push_back( new ast::AddressExpr( loc, new ast::VariableExpr(loc, obj ) ) );
	setup->args.push_back( new ast::VariableExpr( loc, resume_handler ) );

	body->push_back( new ast::ExprStmt(loc, setup ) );

	body->push_back( wraps );
	return body;
}

ast::FunctionDecl * TryMutatorCore::create_finally_wrapper(
		ast::TryStmt * tryStmt ) {
	// void finally(__attribute__((unused)) void *) `finally->body`
	const ast::FinallyClause * finally = tryStmt->finally;
	return new ast::FunctionDecl(
		tryStmt->location,
		"finally",
		{ make_voidptr_object( tryStmt->location ) },
		{}, //return void
		ast::mutate( finally->body.get() ),
		ast::Storage::Classes{},
		ast::Linkage::Cforall,
		{},
		{ ast::Function::Inline }
	);
}

ast::ObjectDecl * TryMutatorCore::create_finally_hook(
		ast::FunctionDecl * finally_wrapper ) {
	// struct __cfaehm_cleanup_hook __finally_hook
	//   	__attribute__((cleanup( `finally_wrapper` )));

	const CodeLocation & location = finally_wrapper->location;
	return new ast::ObjectDecl(
		location,
		"__finally_hook",
		new ast::StructInstType(
			hook_decl
			),
		nullptr,
		ast::Storage::Classes{},
		ast::Linkage::Cforall,
		nullptr,
		{new ast::Attribute("cleanup", {new ast::VariableExpr(location, finally_wrapper)})}
		);
}

ast::Stmt * TryMutatorCore::create_resume_rethrow( const ast::ThrowStmt *throwStmt ) {
	// return false;
	const CodeLocation loc = throwStmt->location;
	ast::Stmt * result = new ast::ReturnStmt(loc,
		ast::ConstantExpr::from_bool( loc, false )
		);
	result->labels = throwStmt->labels;
	return result;
}

// Visiting/Mutating Functions
void TryMutatorCore::previsit( const ast::StructDecl *structDecl ) {
	if ( !structDecl->body ) {
		// Skip children?
		return;
	} else if ( structDecl->name == "__cfaehm_base_exception_t" ) {
		assert( nullptr == except_decl );
		except_decl = structDecl;
	} else if ( structDecl->name == "__cfaehm_try_resume_node" ) {
		assert( nullptr == node_decl );
		node_decl = structDecl;
	} else if ( structDecl->name == "__cfaehm_cleanup_hook" ) {
		assert( nullptr == hook_decl );
		hook_decl = structDecl;
	}
}

ast::Stmt * TryMutatorCore::postvisit( const ast::TryStmt *tryStmt ) {
	assert( except_decl );
	assert( node_decl );
	assert( hook_decl );

	const CodeLocation loc = tryStmt->location;
	ast::TryStmt * mutStmt = mutate(tryStmt);
	// Generate a prefix for the function names?

	ast::CompoundStmt * block = new ast::CompoundStmt( loc );
	// ast::CompoundStmt * inner = take_try_block( mutStmt );
	// this is never mutated so let node deletion do its job?
	const ast::CompoundStmt * inner = mutStmt->body;

	if ( mutStmt->finally ) {
		// Define the helper function.
		ast::FunctionDecl * finally_block =
			create_finally_wrapper( mutStmt );
		appendDeclStmt( block, finally_block );
		// Create and add the finally cleanup hook.
		appendDeclStmt( block, create_finally_hook( finally_block ) );
	}

	CatchList termination_handlers;
	CatchList resumption_handlers;

	for (auto & handler: mutStmt->handlers) {
		// xxx - should always be unique? mutate as safe const-cast
		assert(handler->unique());
		if (handler->kind == ast::ExceptionKind::Resume) {
			resumption_handlers.push_back(handler.get_and_mutate());
		}
		else {
			termination_handlers.push_back(handler.get_and_mutate());
		}
	}

	if ( resumption_handlers.size() ) {
		// Define the helper function.
		ast::FunctionDecl * resume_handler =
			create_resume_handler( resumption_handlers );
		appendDeclStmt( block, resume_handler );
		// Prepare hooks
		inner = create_resume_wrapper( inner, resume_handler );
	}

	if ( termination_handlers.size() ) {
		// Define the two helper functions.
		ast::FunctionDecl * try_wrapper = create_try_wrapper( inner );
		appendDeclStmt( block, try_wrapper );
		ast::FunctionDecl * terminate_match =
			create_terminate_match( termination_handlers );
		appendDeclStmt( block, terminate_match );
		// Build the call to the try wrapper.
		inner = create_terminate_caller(inner->location,
			try_wrapper, terminate_match, termination_handlers );
	}

	// Embed the try block.
	block->push_back( inner );

	return block;
}

ast::Stmt * TryMutatorCore::postvisit( const ast::ThrowStmt *throwStmt ) {
	// Only valid `throwResume;` statements should remain. (2/3 checks)
	assert( ast::ExceptionKind::Resume == throwStmt->kind && ! throwStmt->expr );
	return create_resume_rethrow( throwStmt );
}

} // namespace

void translateThrows( ast::TranslationUnit & transUnit ) {
	ast::Pass<TranslateThrowsCore>::run( transUnit );
}

void translateTries( ast::TranslationUnit & transUnit ) {
	ast::Pass<TryMutatorCore>::run(transUnit);
}

} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
