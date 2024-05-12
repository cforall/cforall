//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Eval.cpp -- Evaluate parts of the ast at compile time.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Aug  6 12:11:59 2022
// Update Count     : 119
//

#include "Eval.hpp"

#include <utility> // for pair

#include "AST/Inspect.hpp"
#include "CodeGen/OperatorTable.hpp"						// access: OperatorInfo
#include "AST/Pass.hpp"
#include "InitTweak/InitTweak.hpp"

struct EvalNew : public ast::WithShortCircuiting {
	Evaluation result = { 0, true, true };

	void previsit( const ast::Node * ) { visit_children = false; }
	void postvisit( const ast::Node * ) { result.isEvaluableInGCC = result.hasKnownValue = false; }

	void postvisit( const ast::UntypedExpr * ) {
		assertf( false, "UntypedExpr in constant expression evaluation" ); // FIX ME, resolve variable
	}

	void postvisit( const ast::ConstantExpr * expr ) {	// only handle int constants
		result.knownValue = expr->intValue();
		result.hasKnownValue = true;
		result.isEvaluableInGCC = true;
	}

	void postvisit( const ast::SizeofExpr * ) {
		result.hasKnownValue = false;
		result.isEvaluableInGCC = true;
	}

	void postvisit( const ast::AlignofExpr * ) {
		result.hasKnownValue = false;
		result.isEvaluableInGCC = true;
	}

	void postvisit( const ast::OffsetofExpr * ) {
		result.hasKnownValue = false;
		result.isEvaluableInGCC = true;
	}

	void postvisit( const ast::LogicalExpr * expr ) {
		Evaluation arg1, arg2;
		arg1 = eval( expr->arg1 );
		result.isEvaluableInGCC &= arg1.isEvaluableInGCC;
		if ( ! result.isEvaluableInGCC ) return;
		arg2 = eval( expr->arg2 );
		result.isEvaluableInGCC &= arg2.isEvaluableInGCC;
		if ( ! result.isEvaluableInGCC ) return;

		result.hasKnownValue &= arg1.hasKnownValue;
		result.hasKnownValue &= arg2.hasKnownValue;
		if ( ! result.hasKnownValue ) return;

		if ( expr->isAnd ) {
			result.knownValue = arg1.knownValue && arg2.knownValue;
		} else {
			result.knownValue = arg1.knownValue || arg2.knownValue;
		} // if
	}

	void postvisit( const ast::ConditionalExpr * expr ) {
		Evaluation arg1, arg2, arg3;
		arg1 = eval( expr->arg1 );
		result.isEvaluableInGCC &= arg1.isEvaluableInGCC;
		if ( ! result.isEvaluableInGCC ) return;
		arg2 = eval( expr->arg2 );
		result.isEvaluableInGCC &= arg2.isEvaluableInGCC;
		if ( ! result.isEvaluableInGCC ) return;
		arg3 = eval( expr->arg3 );
		result.isEvaluableInGCC &= arg3.isEvaluableInGCC;
		if ( ! result.isEvaluableInGCC ) return;

		result.hasKnownValue &= arg1.hasKnownValue;
		result.hasKnownValue &= arg2.hasKnownValue;
		result.hasKnownValue &= arg3.hasKnownValue;
		if ( ! result.hasKnownValue ) return;

		result.knownValue = arg1.knownValue ? arg2.knownValue : arg3.knownValue;
	}

	void postvisit( const ast::CastExpr * expr ) {		
		// cfa-cc generates a cast before every constant and many other places, e.g., (int)3, 
		// so we must use the value from the cast argument, even though we lack any basis for evaluating wraparound effects, etc
		result = eval(expr->arg);
	}

	void postvisit( const ast::VariableExpr * expr ) {
		result.hasKnownValue = false;
		result.isEvaluableInGCC = false;
		if ( const ast::EnumInstType * inst = dynamic_cast<const ast::EnumInstType *>(expr->result.get()) ) {
			if ( const ast::EnumDecl * decl = inst->base ) {
				result.isEvaluableInGCC = true;
				result.hasKnownValue = decl->valueOf( expr->var, result.knownValue ); // result.knownValue filled by valueOf
			}
		}
	}

	void postvisit( const ast::ApplicationExpr * expr ) {
		const ast::DeclWithType * function = ast::getFunction(expr);
		if ( ! function || function->linkage != ast::Linkage::Intrinsic ) { 
			result.isEvaluableInGCC = false;
			result.hasKnownValue = false;
			return;
		}
		const std::string & fname = function->name;
		assertf( expr->args.size() == 1 || expr->args.size() == 2, "Intrinsic function with %zd arguments: %s", expr->args.size(), fname.c_str() );

		if ( expr->args.size() == 1 ) {
			// pre/postfix operators ++ and -- => assignment, which is not constant
			Evaluation arg1;
			arg1 = eval(expr->args.front());
			result.isEvaluableInGCC &= arg1.isEvaluableInGCC;
			if ( ! result.isEvaluableInGCC ) return;

			result.hasKnownValue &= arg1.hasKnownValue;
			if ( ! result.hasKnownValue ) return;

			if (fname == "+?") {
				result.knownValue = arg1.knownValue;
			} else if (fname == "-?") {
				result.knownValue = -arg1.knownValue;
			} else if (fname == "~?") {
				result.knownValue = ~arg1.knownValue;
			} else if (fname == "!?") {
				result.knownValue = ! arg1.knownValue;
			} else {
				result.isEvaluableInGCC = false;
				result.hasKnownValue = false;
			} // if
		} else { // => expr->args.size() == 2
			// infix assignment operators => assignment, which is not constant
			Evaluation arg1, arg2;
			arg1 = eval(expr->args.front());
			result.isEvaluableInGCC &= arg1.isEvaluableInGCC;
			if ( ! result.isEvaluableInGCC ) return;
			arg2 = eval(expr->args.back());
			result.isEvaluableInGCC &= arg2.isEvaluableInGCC;
			if ( ! result.isEvaluableInGCC ) return;

			result.hasKnownValue &= arg1.hasKnownValue;
			result.hasKnownValue &= arg2.hasKnownValue;
			if ( ! result.hasKnownValue ) return;

			if (fname == "?+?") {
				result.knownValue = arg1.knownValue + arg2.knownValue;
			} else if (fname == "?-?") {
				result.knownValue = arg1.knownValue - arg2.knownValue;
			} else if (fname == "?*?") {
				result.knownValue = arg1.knownValue * arg2.knownValue;
			} else if (fname == "?/?") {
				if ( arg2.knownValue ) result.knownValue = arg1.knownValue / arg2.knownValue;
			} else if (fname == "?%?") {
				if ( arg2.knownValue ) result.knownValue = arg1.knownValue % arg2.knownValue;
			} else if (fname == "?<<?") {
				result.knownValue = arg1.knownValue << arg2.knownValue;
			} else if (fname == "?>>?") {
				result.knownValue = arg1.knownValue >> arg2.knownValue;
			} else if (fname == "?<?") {
				result.knownValue = arg1.knownValue < arg2.knownValue;
			} else if (fname == "?>?") {
				result.knownValue = arg1.knownValue > arg2.knownValue;
			} else if (fname == "?<=?") {
				result.knownValue = arg1.knownValue <= arg2.knownValue;
			} else if (fname == "?>=?") {
				result.knownValue = arg1.knownValue >= arg2.knownValue;
			} else if (fname == "?==?") {
				result.knownValue = arg1.knownValue == arg2.knownValue;
			} else if (fname == "?!=?") {
				result.knownValue = arg1.knownValue != arg2.knownValue;
			} else if (fname == "?&?") {
				result.knownValue = arg1.knownValue & arg2.knownValue;
			} else if (fname == "?^?") {
				result.knownValue = arg1.knownValue ^ arg2.knownValue;
			} else if (fname == "?|?") {
				result.knownValue = arg1.knownValue | arg2.knownValue;
			} else {
				result.isEvaluableInGCC = false;
				result.hasKnownValue = false;
			}
		} // if
		// TODO: implement other intrinsic functions
	}
};

Evaluation eval( const ast::Expr * expr ) {
	if ( expr ) {

		return ast::Pass<EvalNew>::read(expr);
		// Evaluation ret = ast::Pass<EvalNew>::read(expr);
		// ret.knownValue = 777;
		// return ret;

	} else {
		return { 0, false, false };
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
