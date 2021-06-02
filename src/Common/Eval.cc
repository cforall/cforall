//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// utility.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Jul 24 15:09:06 2019
// Update Count     : 64
//

#include <utility> // for pair

#include "Common/PassVisitor.h"
#include "AST/Pass.hpp"
#include "InitTweak/InitTweak.h"
#include "SynTree/Expression.h"

//-------------------------------------------------------------
// Old AST
struct EvalOld : public WithShortCircuiting {
	long long int value = 0;
	bool valid = true;

	void previsit( const BaseSyntaxNode * ) { visit_children = false; }
	void postvisit( const BaseSyntaxNode * ) { valid = false; }

	void postvisit( const SizeofExpr * ) {
	}

	void postvisit( const ConstantExpr * expr ) {
		value = expr->intValue();
	}

	void postvisit( const CastExpr * expr ) {
		auto arg = eval(expr->arg);
		valid = arg.second;
		value = arg.first;
		// TODO: perform type conversion on value if valid
	}

	void postvisit( const VariableExpr * const expr ) {
		if ( EnumInstType * inst = dynamic_cast<EnumInstType *>(expr->result) ) {
			if ( EnumDecl * decl = inst->baseEnum ) {
				if ( decl->valueOf( expr->var, value ) ) { // value filled by valueOf
					return;
				}
			}
		}
		valid = false;
	}

	void postvisit( const ApplicationExpr * expr ) {
		DeclarationWithType * function = InitTweak::getFunction(const_cast<ApplicationExpr *>(expr));
		if ( ! function || function->linkage != LinkageSpec::Intrinsic ) { valid = false; return; }
		const std::string & fname = function->name;
		assertf( expr->args.size() == 1 || expr->args.size() == 2, "Intrinsic function with %zd arguments: %s", expr->args.size(), fname.c_str() );
		std::pair<long long int, bool> arg1, arg2;
		arg1 = eval(expr->args.front());
		valid = valid && arg1.second;
		if ( ! valid ) return;
		if ( expr->args.size() == 2 ) {
			arg2 = eval(expr->args.back());
			valid = valid && arg2.second;
			if ( ! valid ) return;
		}
		if (fname == "?+?") {
			value = arg1.first + arg2.first;
		} else if (fname == "?-?") {
			value = arg1.first - arg2.first;
		} else if (fname == "?*?") {
			value = arg1.first * arg2.first;
		} else if (fname == "?/?") {
			value = arg1.first / arg2.first;
		} else if (fname == "?%?") {
			value = arg1.first % arg2.first;
		} else {
			valid = false;
		}
		// TODO: implement other intrinsic functions
	}
};

//-------------------------------------------------------------
// New AST
struct EvalNew : public ast::WithShortCircuiting {
	long long int value = 0;
	bool valid = true;

	void previsit( const ast::Node * ) { visit_children = false; }
	void postvisit( const ast::Node * ) { valid = false; }

	void postvisit( const ast::ConstantExpr * expr ) {
		value = expr->intValue();
	}

	void postvisit( const ast::SizeofExpr * expr ) {
		if ( expr->expr ) value = eval(expr->expr).first;
		else if ( expr->type ) value = eval(expr->expr).first;
		else SemanticError( expr->location, ::toString( "Internal error: SizeofExpr has no expression or type value" ) );
	}

	void postvisit( const ast::CastExpr * expr ) {
		auto arg = eval(expr->arg);
		valid = arg.second;
		value = arg.first;
		// TODO: perform type conversion on value if valid
	}

	void postvisit( const ast::VariableExpr * expr ) {
		if ( const ast::EnumInstType * inst = dynamic_cast<const ast::EnumInstType *>(expr->result.get()) ) {
			if ( const ast::EnumDecl * decl = inst->base ) {
				if ( decl->valueOf( expr->var, value ) ) { // value filled by valueOf
					return;
				}
			}
		}
		valid = false;
	}

	void postvisit( const ast::ApplicationExpr * expr ) {
		const ast::DeclWithType * function = InitTweak::getFunction(expr);
		if ( ! function || function->linkage != ast::Linkage::Intrinsic ) { valid = false; return; }
		const std::string & fname = function->name;
		assertf( expr->args.size() == 1 || expr->args.size() == 2, "Intrinsic function with %zd arguments: %s", expr->args.size(), fname.c_str() );
		std::pair<long long int, bool> arg1, arg2;
		arg1 = eval(expr->args.front());
		valid = valid && arg1.second;
		if ( ! valid ) return;
		if ( expr->args.size() == 2 ) {
			arg2 = eval(expr->args.back());
			valid = valid && arg2.second;
			if ( ! valid ) return;
		}
		if (fname == "?+?") {
			value = arg1.first + arg2.first;
		} else if (fname == "?-?") {
			value = arg1.first - arg2.first;
		} else if (fname == "?*?") {
			value = arg1.first * arg2.first;
		} else if (fname == "?/?") {
			value = arg1.first / arg2.first;
		} else if (fname == "?%?") {
			value = arg1.first % arg2.first;
		} else {
			valid = false;
		}
		// TODO: implement other intrinsic functions
	}
};

std::pair<long long int, bool> eval( const Expression * expr) {
	PassVisitor<EvalOld> ev;
	if (expr) {
		expr->accept(ev);
		return std::make_pair(ev.pass.value, ev.pass.valid);
	} else {
		return std::make_pair(0, false);
	}
}

std::pair<long long int, bool> eval(const ast::Expr * expr) {
	ast::Pass<EvalNew> ev;
	if (expr) {
		expr->accept(ev);
		return std::make_pair(ev.core.value, ev.core.valid);
	} else {
		return std::make_pair(0, false);
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
