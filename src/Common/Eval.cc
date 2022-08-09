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
// Last Modified On : Sat Aug  6 12:11:59 2022
// Update Count     : 119
//

#include <utility> // for pair

#include "Common/PassVisitor.h"
#include "CodeGen/OperatorTable.h"						// access: OperatorInfo
#include "AST/Pass.hpp"
#include "InitTweak/InitTweak.h"
#include "SynTree/Expression.h"

//-------------------------------------------------------------
// Old AST
struct EvalOld : public WithShortCircuiting {
	long long int value = 0;							// compose the result of the constant expression
	bool valid = true;									// true => constant expression and value is the result
														// false => not constant expression, e.g., ++i
	bool cfavalid = true;								// true => constant expression and value computable
														// false => constant expression but value not computable, e.g., sizeof(int)

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
	long long int value = 0;							// compose the result of the constant expression
	bool valid = true;									// true => constant expression and value is the result
														// false => not constant expression, e.g., ++i
	bool cfavalid = true;								// true => constant expression and value computable
														// false => constant expression but value not computable, e.g., sizeof(int)

	void previsit( const ast::Node * ) { visit_children = false; }
	void postvisit( const ast::Node * ) { cfavalid = valid = false; }

	void postvisit( const ast::UntypedExpr * ) {
		assertf( false, "UntypedExpr in constant expression evaluation" ); // FIX ME, resolve variable
	}

	void postvisit( const ast::ConstantExpr * expr ) {	// only handle int constants
		value = expr->intValue();
	}

	void postvisit( const ast::SizeofExpr * ) {
		// do not change valid or value => let C figure it out
		cfavalid = false;
	}

	void postvisit( const ast::AlignofExpr * ) {
		// do not change valid or value => let C figure it out
		cfavalid = false;
	}

	void postvisit( const ast::OffsetofExpr * ) {
		// do not change valid or value => let C figure it out
		cfavalid = false;
	}

	void postvisit( const ast::LogicalExpr * expr ) {
		std::pair<long long int, bool> arg1, arg2;
		arg1 = eval( expr->arg1 );
		valid &= arg1.second;
		if ( ! valid ) return;
		arg2 = eval( expr->arg2 );
		valid &= arg2.second;
		if ( ! valid ) return;

		if ( expr->isAnd ) {
			value = arg1.first && arg2.first;
		} else {
			value = arg1.first || arg2.first;
		} // if
	}

	void postvisit( const ast::ConditionalExpr * expr ) {
		std::pair<long long int, bool> arg1, arg2, arg3;
		arg1 = eval( expr->arg1 );
		valid &= arg1.second;
		if ( ! valid ) return;
		arg2 = eval( expr->arg2 );
		valid &= arg2.second;
		if ( ! valid ) return;
		arg3 = eval( expr->arg3 );
		valid &= arg3.second;
		if ( ! valid ) return;

		value = arg1.first ? arg2.first : arg3.first;
	}

	void postvisit( const ast::CastExpr * expr ) {		
		// cfa-cc generates a cast before every constant and many other places, e.g., (int)3, so the cast argument must
		// be evaluated to get the constant value.
		auto arg = eval(expr->arg);
		valid = arg.second;
		value = arg.first;
		cfavalid = false;
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

		if ( expr->args.size() == 1 ) {
			// pre/postfix operators ++ and -- => assignment, which is not constant
			std::pair<long long int, bool> arg1;
			arg1 = eval(expr->args.front());
			valid &= arg1.second;
			if ( ! valid ) return;

			if (fname == "+?") {
				value = arg1.first;
			} else if (fname == "-?") {
				value = -arg1.first;
			} else if (fname == "~?") {
				value = ~arg1.first;
			} else if (fname == "!?") {
				value = ! arg1.first;
			} else {
				valid = false;
			} // if
		} else { // => expr->args.size() == 2
			// infix assignment operators => assignment, which is not constant
			std::pair<long long int, bool> arg1, arg2;
			arg1 = eval(expr->args.front());
			valid &= arg1.second;
			if ( ! valid ) return;
			arg2 = eval(expr->args.back());
			valid &= arg2.second;
			if ( ! valid ) return;

			if (fname == "?+?") {
				value = arg1.first + arg2.first;
			} else if (fname == "?-?") {
				value = arg1.first - arg2.first;
			} else if (fname == "?*?") {
				value = arg1.first * arg2.first;
			} else if (fname == "?/?") {
				if ( arg2.first ) value = arg1.first / arg2.first;
			} else if (fname == "?%?") {
				if ( arg2.first ) value = arg1.first % arg2.first;
			} else if (fname == "?<<?") {
				value = arg1.first << arg2.first;
			} else if (fname == "?>>?") {
				value = arg1.first >> arg2.first;
			} else if (fname == "?<?") {
				value = arg1.first < arg2.first;
			} else if (fname == "?>?") {
				value = arg1.first > arg2.first;
			} else if (fname == "?<=?") {
				value = arg1.first <= arg2.first;
			} else if (fname == "?>=?") {
				value = arg1.first >= arg2.first;
			} else if (fname == "?==?") {
				value = arg1.first == arg2.first;
			} else if (fname == "?!=?") {
				value = arg1.first != arg2.first;
			} else if (fname == "?&?") {
				value = arg1.first & arg2.first;
			} else if (fname == "?^?") {
				value = arg1.first ^ arg2.first;
			} else if (fname == "?|?") {
				value = arg1.first | arg2.first;
			} else {
				valid = false;
			}
		} // if
		// TODO: implement other intrinsic functions
	}
};

std::pair<long long int, bool> eval( const Expression * expr ) {
	PassVisitor<EvalOld> ev;
	if ( expr ) {
		expr->accept( ev );
		return std::make_pair( ev.pass.value, ev.pass.valid );
	} else {
		return std::make_pair( 0, false );
	}
}

std::pair<long long int, bool> eval( const ast::Expr * expr ) {
	ast::Pass<EvalNew> ev;
	if ( expr ) {
		expr->accept( ev );
		return std::make_pair( ev.core.value, ev.core.valid );
	} else {
		return std::make_pair( 0, false );
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
