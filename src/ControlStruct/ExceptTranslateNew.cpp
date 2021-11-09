//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExceptTranslateNew.cpp -- Conversion of exception control flow structures.
//
// Author           : Andrew Beach
// Created On       : Mon Nov  8 11:53:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Mon Nov  8 16:50:00 2021
// Update Count     : 0
//

#include "ExceptTranslate.h"

#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/TranslationUnit.hpp"

namespace ControlStruct {

namespace {

class TranslateThrowsCore : public ast::WithGuards {
	const ast::ObjectDecl * terminateHandlerExcept;
	enum Context { NoHandler, TerHandler, ResHandler } currentContext;

	const ast::Stmt * createEitherThrow(
		const ast::ThrowStmt * throwStmt, const char * funcName );
	const ast::Stmt * createTerminateRethrow( const ast::ThrowStmt * );

public:
	TranslateThrowsCore() :
		terminateHandlerExcept( nullptr ), currentContext( NoHandler )
	{}

	void previsit( const ast::CatchStmt * stmt );
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

void TranslateThrowsCore::previsit( const ast::CatchStmt * stmt ) {
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

} // namespace

void translateThrows( ast::TranslationUnit & transUnit ) {
	ast::Pass<TranslateThrowsCore>::run( transUnit );
}

} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
