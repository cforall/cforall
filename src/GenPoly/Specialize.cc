//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Specialize.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Thr Jul  2 17:42:00 2020
// Update Count     : 33
//

#include <cassert>                       // for assert, assertf
#include <iterator>                      // for back_insert_iterator, back_i...
#include <map>                           // for _Rb_tree_iterator, _Rb_tree_...
#include <memory>                        // for unique_ptr
#include <string>                        // for string
#include <tuple>                         // for get
#include <utility>                       // for pair

#include "Common/PassVisitor.h"
#include "Common/UniqueName.h"           // for UniqueName
#include "Common/utility.h"              // for group_iterate
#include "GenPoly.h"                     // for getFunctionType
#include "InitTweak/InitTweak.h"         // for isIntrinsicCallExpr
#include "ResolvExpr/FindOpenVars.h"     // for findOpenVars
#include "ResolvExpr/TypeEnvironment.h"  // for OpenVarSet, AssertionSet
#include "Specialize.h"
#include "SynTree/LinkageSpec.h"         // for C
#include "SynTree/Attribute.h"           // for Attribute
#include "SynTree/Declaration.h"         // for FunctionDecl, DeclarationWit...
#include "SynTree/Expression.h"          // for ApplicationExpr, Expression
#include "SynTree/Label.h"               // for Label
#include "SynTree/Mutator.h"             // for mutateAll
#include "SynTree/Statement.h"           // for CompoundStmt, DeclStmt, Expr...
#include "SynTree/Type.h"                // for FunctionType, TupleType, Type
#include "SynTree/TypeSubstitution.h"    // for TypeSubstitution
#include "SynTree/Visitor.h"             // for Visitor

namespace GenPoly {
	struct Specialize final : public WithConstTypeSubstitution,
			public WithDeclsToAdd, public WithVisitorRef<Specialize> {
		Expression * postmutate( ApplicationExpr *applicationExpr );
		Expression * postmutate( CastExpr *castExpr );

		void handleExplicitParams( ApplicationExpr *appExpr );
		Expression * createThunkFunction( FunctionType *funType, Expression *actual, InferredParams *inferParams );
		Expression * doSpecialization( Type *formalType, Expression *actual, InferredParams *inferParams );

		std::string paramPrefix = "_p";
	};

	/// Looks up open variables in actual type, returning true if any of them are bound in the environment or formal type.
	bool needsPolySpecialization( Type *formalType, Type *actualType, const TypeSubstitution *env ) {
		if ( env ) {
			using namespace ResolvExpr;
			OpenVarSet openVars, closedVars;
			AssertionSet need, have;
			findOpenVars( formalType, openVars, closedVars, need, have, false );
			findOpenVars( actualType, openVars, closedVars, need, have, true );
			for ( OpenVarSet::const_iterator openVar = openVars.begin(); openVar != openVars.end(); ++openVar ) {
				Type *boundType = env->lookup( openVar->first );
				if ( ! boundType ) continue;
				if ( TypeInstType *typeInst = dynamic_cast< TypeInstType* >( boundType ) ) {
					// bound to another type variable
					if ( closedVars.find( typeInst->get_name() ) == closedVars.end() ) {
						// bound to a closed variable => must specialize
						return true;
					} // if
				} else {
					// variable is bound to a concrete type => must specialize
					return true;
				} // if
			} // for
			// none of the type variables are bound
			return false;
		} else {
			// no env
			return false;
		} // if
	}

	/// True if both types have the same structure, but not necessarily the same types.
	/// That is, either both types are tuple types with the same size (recursively), or
	/// both are not tuple types.
	bool matchingTupleStructure( Type * t1, Type * t2 ) {
		TupleType * tuple1 = dynamic_cast< TupleType * >( t1 );
		TupleType * tuple2 = dynamic_cast< TupleType * >( t2 );
		if ( tuple1 && tuple2 ) {
			if ( tuple1->size() != tuple2->size() ) return false;
			for ( auto types : group_iterate( tuple1->get_types(), tuple2->get_types() ) ) {
				if ( ! matchingTupleStructure( std::get<0>( types ), std::get<1>( types ) ) ) return false;
			}
			return true;
		} else if ( ! tuple1 && ! tuple2 ) return true;
		return false;
	}

	// walk into tuple type and find the number of components
	size_t singleParameterSize( Type * type ) {
		if ( TupleType * tt = dynamic_cast< TupleType * >( type ) ) {
			size_t sz = 0;
			for ( Type * t : *tt ) {
				sz += singleParameterSize( t );
			}
			return sz;
		} else {
			return 1;
		}
	}

	// find the total number of components in a parameter list
	size_t functionParameterSize( FunctionType * ftype ) {
		size_t sz = 0;
		for ( DeclarationWithType * p : ftype->get_parameters() ) {
			sz += singleParameterSize( p->get_type() );
		}
		return sz;
	}

	bool needsTupleSpecialization( Type *formalType, Type *actualType ) {
		// Needs tuple specialization if the structure of the formal type and actual type do not match.
		// This is the case if the formal type has ttype polymorphism, or if the structure  of tuple types
		// between the function do not match exactly.
		if ( FunctionType * fftype = getFunctionType( formalType ) ) {
			if ( fftype->isTtype() ) return true;
			// conversion of 0 (null) to function type does not require tuple specialization
			if ( dynamic_cast< ZeroType * >( actualType ) ) return false;
			FunctionType * aftype = getFunctionType( actualType->stripReferences() );
			assertf( aftype, "formal type is a function type, but actual type is not: %s", toString( actualType ).c_str() );
			// Can't tuple specialize if parameter sizes deeply-differ.
			if ( functionParameterSize( fftype ) != functionParameterSize( aftype ) ) return false;
			// tuple-parameter sizes are the same, but actual parameter sizes differ - must tuple specialize
			if ( fftype->parameters.size() != aftype->parameters.size() ) return true;
			// total parameter size can be the same, while individual parameters can have different structure
			for ( auto params : group_iterate( fftype->parameters, aftype->parameters ) ) {
				DeclarationWithType * formal = std::get<0>(params);
				DeclarationWithType * actual = std::get<1>(params);
				if ( ! matchingTupleStructure( formal->get_type(), actual->get_type() ) ) return true;
			}
		}
		return false;
	}

	bool needsSpecialization( Type *formalType, Type *actualType, const TypeSubstitution *env ) {
		return needsPolySpecialization( formalType, actualType, env ) || needsTupleSpecialization( formalType, actualType );
	}

	Expression * Specialize::doSpecialization( Type *formalType, Expression *actual, InferredParams *inferParams ) {
		assertf( actual->result, "attempting to specialize an untyped expression" );
		if ( needsSpecialization( formalType, actual->get_result(), env ) ) {
			if ( FunctionType *funType = getFunctionType( formalType ) ) {
				if ( ApplicationExpr * appExpr = dynamic_cast<ApplicationExpr*>( actual ) ) {
					return createThunkFunction( funType, appExpr->get_function(), inferParams );
				} else if ( VariableExpr * varExpr = dynamic_cast<VariableExpr*>( actual ) ) {
					return createThunkFunction( funType, varExpr, inferParams );
				} else {
					// This likely won't work, as anything that could build an ApplicationExpr probably hit one of the previous two branches
					return createThunkFunction( funType, actual, inferParams );
				}
			} else {
				return actual;
			} // if
		} else {
			return actual;
		} // if
	}

	/// restructures the arguments to match the structure of the formal parameters of the actual function.
	/// [begin, end) are the exploded arguments.
	template< typename Iterator, typename OutIterator >
	void structureArg( Type * type, Iterator & begin, Iterator end, OutIterator out ) {
		if ( TupleType * tuple = dynamic_cast< TupleType * >( type ) ) {
			std::list< Expression * > exprs;
			for ( Type * t : *tuple ) {
				structureArg( t, begin, end, back_inserter( exprs ) );
			}
			*out++ = new TupleExpr( exprs );
		} else {
			assertf( begin != end, "reached the end of the arguments while structuring" );
			*out++ = *begin++;
		}
	}

	/// explode assuming simple cases: either type is pure tuple (but not tuple expr) or type is non-tuple.
	template< typename OutputIterator >
	void explodeSimple( Expression * expr, OutputIterator out ) {
		if ( TupleType * tupleType = dynamic_cast< TupleType * > ( expr->get_result() ) ) {
			// tuple type, recursively index into its components
			for ( unsigned int i = 0; i < tupleType->size(); i++ ) {
				explodeSimple( new TupleIndexExpr( expr->clone(), i ), out );
			}
			delete expr;
		} else {
			// non-tuple type - output a clone of the expression
			*out++ = expr;
		}
	}

	/// Generates a thunk that calls `actual` with type `funType` and returns its address
	Expression * Specialize::createThunkFunction( FunctionType *funType, Expression *actual, InferredParams *inferParams ) {
		static UniqueName thunkNamer( "_thunk" );

		FunctionType *newType = funType->clone();
		if ( env ) {
			// it is important to replace only occurrences of type variables that occur free in the
			// thunk's type
			env->applyFree( newType );
		} // if
		// create new thunk with same signature as formal type (C linkage, empty body)
		FunctionDecl *thunkFunc = new FunctionDecl( thunkNamer.newName(), Type::StorageClasses(), LinkageSpec::C, newType, new CompoundStmt() );
		thunkFunc->fixUniqueId();

		// thunks may be generated and not used - silence warning with attribute
		thunkFunc->get_attributes().push_back( new Attribute( "unused" ) );

		// Thunks at the global level must be static to avoid collisions between files.
		// (Conversly thunks inside a function must be unique and not static.)
		thunkFunc->storageClasses.is_static = !isInFunction();

		// thread thunk parameters into call to actual function, naming thunk parameters as we go
		UniqueName paramNamer( paramPrefix );
		ApplicationExpr *appExpr = new ApplicationExpr( actual );

		FunctionType * actualType = getFunctionType( actual->get_result() )->clone();
		if ( env ) {
			// need to apply the environment to the actual function's type, since it may itself be polymorphic
			env->apply( actualType );
		}
		std::unique_ptr< FunctionType > actualTypeManager( actualType ); // for RAII
		std::list< DeclarationWithType * >::iterator actualBegin = actualType->get_parameters().begin();
		std::list< DeclarationWithType * >::iterator actualEnd = actualType->get_parameters().end();

		std::list< Expression * > args;
		for ( DeclarationWithType* param : thunkFunc->get_functionType()->get_parameters() ) {
			// name each thunk parameter and explode it - these are then threaded back into the actual function call.
			param->set_name( paramNamer.newName() );
			explodeSimple( new VariableExpr( param ), back_inserter( args ) );
		}

		// walk parameters to the actual function alongside the exploded thunk parameters and restructure the arguments to match the actual parameters.
		std::list< Expression * >::iterator argBegin = args.begin(), argEnd = args.end();
		for ( ; actualBegin != actualEnd; ++actualBegin ) {
			structureArg( (*actualBegin)->get_type(), argBegin, argEnd, back_inserter( appExpr->get_args() ) );
		}

		appExpr->env = TypeSubstitution::newFromExpr( appExpr, env );
		if ( inferParams ) {
			appExpr->inferParams = *inferParams;
		} // if

		// Handle any specializations that may still be present.
		{
			std::string oldParamPrefix = paramPrefix;
			paramPrefix += "p";
			std::list< Declaration * > oldDecls;
			oldDecls.splice( oldDecls.end(), declsToAddBefore );

			appExpr->acceptMutator( *visitor );
			// Write recursive specializations into the thunk body.
			for ( Declaration * decl : declsToAddBefore ) {
				thunkFunc->statements->kids.push_back( new DeclStmt( decl ) );
			}

			declsToAddBefore = std::move( oldDecls );
			paramPrefix = oldParamPrefix;
		}

		// add return (or valueless expression) to the thunk
		Statement *appStmt;
		if ( funType->returnVals.empty() ) {
			appStmt = new ExprStmt( appExpr );
		} else {
			appStmt = new ReturnStmt( appExpr );
		} // if
		thunkFunc->statements->kids.push_back( appStmt );

		// Add the thunk definition (converted to DeclStmt if appproprate).
		declsToAddBefore.push_back( thunkFunc );
		// return address of thunk function as replacement expression
		return new AddressExpr( new VariableExpr( thunkFunc ) );
	}

	void Specialize::handleExplicitParams( ApplicationExpr *appExpr ) {
		// create thunks for the explicit parameters
		assert( appExpr->function->result );
		FunctionType *function = getFunctionType( appExpr->function->result );
		assert( function );
		std::list< DeclarationWithType* >::iterator formal;
		std::list< Expression* >::iterator actual;
		for ( formal = function->get_parameters().begin(), actual = appExpr->get_args().begin(); formal != function->get_parameters().end() && actual != appExpr->get_args().end(); ++formal, ++actual ) {
			*actual = doSpecialization( (*formal)->get_type(), *actual, &appExpr->inferParams );
		}
	}

	Expression * Specialize::postmutate( ApplicationExpr *appExpr ) {
		if ( ! InitTweak::isIntrinsicCallExpr( appExpr ) ) {
			// create thunks for the inferred parameters
			// don't need to do this for intrinsic calls, because they aren't actually passed
			// need to handle explicit params before inferred params so that explicit params do not recieve a changed set of inferParams (and change them again)
			// alternatively, if order starts to matter then copy appExpr's inferParams and pass them to handleExplicitParams.
			handleExplicitParams( appExpr );
			for ( InferredParams::iterator inferParam = appExpr->inferParams.begin(); inferParam != appExpr->inferParams.end(); ++inferParam ) {
				inferParam->second.expr = doSpecialization( inferParam->second.formalType, inferParam->second.expr, &inferParam->second.expr->inferParams );
			}
		}
		return appExpr;
	}

	Expression * Specialize::postmutate( CastExpr *castExpr ) {
		if ( castExpr->result->isVoid() ) {
			// can't specialize if we don't have a return value
			return castExpr;
		}
		Expression *specialized = doSpecialization( castExpr->result, castExpr->arg, &castExpr->inferParams );
		if ( specialized != castExpr->arg ) {
			// assume here that the specialization incorporates the cast
			return specialized;
		} else {
			return castExpr;
		}
	}

	void convertSpecializations( std::list< Declaration* >& translationUnit ) {
		PassVisitor<Specialize> spec;
		mutateAll( translationUnit, spec );
	}
} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
