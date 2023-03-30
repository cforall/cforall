//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Lvalue.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon May 16 14:09:00 2022
// Update Count     : 8
//

#include <cassert>                       // for strict_dynamic_cast
#include <string>                        // for string

#include "Common/ToString.hpp"           // for toCString
#include "Common/UniqueName.h"
#include "Common/PassVisitor.h"
#include "GenPoly.h"                     // for isPolyType
#include "Lvalue.h"

#include "InitTweak/InitTweak.h"
#include "ResolvExpr/TypeEnvironment.h"  // for AssertionSet, OpenVarSet
#include "ResolvExpr/Unify.h"            // for unify
#include "ResolvExpr/typeops.h"
#include "SymTab/Indexer.h"              // for Indexer
#include "SynTree/LinkageSpec.h"         // for Spec, isBuiltin, Intrinsic
#include "SynTree/Declaration.h"         // for Declaration, FunctionDecl
#include "SynTree/Expression.h"          // for Expression, ConditionalExpr
#include "SynTree/Mutator.h"             // for mutateAll, Mutator
#include "SynTree/Statement.h"           // for ReturnStmt, Statement (ptr o...
#include "SynTree/Type.h"                // for PointerType, Type, FunctionType
#include "SynTree/Visitor.h"             // for Visitor, acceptAll
#include "Validate/FindSpecialDecls.h"   // for dereferenceOperator

#if 0
#define PRINT(x) x
#else
#define PRINT(x)
#endif

namespace GenPoly {
	namespace {
		// TODO: fold this into the general createDeref function??
		Expression * mkDeref( Expression * arg ) {
			if ( Validate::dereferenceOperator ) {
				// note: reference depth can be arbitrarily deep here, so peel off the outermost pointer/reference, not just pointer because they are effecitvely equivalent in this pass
				VariableExpr * deref = new VariableExpr( Validate::dereferenceOperator );
				deref->result = new PointerType( Type::Qualifiers(), deref->result );
				Type * base = InitTweak::getPointerBase( arg->result );
				assertf( base, "expected pointer type in dereference (type was %s)", toString( arg->result ).c_str() );
				ApplicationExpr * ret = new ApplicationExpr( deref, { arg } );
				delete ret->result;
				ret->result = base->clone();
				return ret;
			} else {
				return UntypedExpr::createDeref( arg );
			}
		}

		struct ReferenceConversions final : public WithStmtsToAdd, public WithGuards {
			Expression * postmutate( CastExpr * castExpr );
			Expression * postmutate( AddressExpr * addrExpr );
		};

		/// Intrinsic functions that take reference parameters don't REALLY take reference parameters -- their reference arguments must always be implicitly dereferenced.
		struct FixIntrinsicArgs final {
			Expression * postmutate( ApplicationExpr * appExpr );
		};

		struct FixIntrinsicResult final : public WithGuards {
			enum {
				NoSkip,
				Skip,
				SkipInProgress
			} skip = NoSkip;

			void premutate( AsmExpr * ) { GuardValue( skip ); skip = Skip; }
			void premutate( ApplicationExpr * ) { GuardValue( skip ); skip = (skip == Skip) ? SkipInProgress : NoSkip; }


			Expression * postmutate( ApplicationExpr * appExpr );
			void premutate( FunctionDecl * funcDecl );
			bool inIntrinsic = false;
		};

		/// Replace reference types with pointer types
		struct ReferenceTypeElimination final {
			Type * postmutate( ReferenceType * refType );
		};

		/// GCC-like Generalized Lvalues (which have since been removed from GCC)
		/// https://gcc.gnu.org/onlinedocs/gcc-3.4.6/gcc/Lvalues.html#Lvalues
		/// Replaces &(a,b) with (a, &b), &(a ? b : c) with (a ? &b : &c)
		struct GeneralizedLvalue final : public WithVisitorRef<GeneralizedLvalue> {
			Expression * postmutate( AddressExpr * addressExpr );
			Expression * postmutate( MemberExpr * memExpr );

			template<typename Func>
			Expression * applyTransformation( Expression * expr, Expression * arg, Func mkExpr );
		};

		/// Removes redundant &*/*& pattern that this pass can generate
		struct CollapseAddrDeref final {
			Expression * postmutate( AddressExpr * addressExpr );
			Expression * postmutate( ApplicationExpr * appExpr );
		};

		struct AddrRef final : public WithGuards, public WithVisitorRef<AddrRef>, public WithShortCircuiting {
			void premutate( AddressExpr * addrExpr );
			Expression * postmutate( AddressExpr * addrExpr );
			void premutate( Expression * expr );
			void premutate( ApplicationExpr * appExpr );
			void premutate( SingleInit * init );

			void handleNonAddr( Expression * );

			bool first = true;
			bool current = false;
			int refDepth = 0;
			bool addCast = false;
		};
	} // namespace

	// Stored elsewhere (Lvalue2, initially false).
	extern bool referencesEliminated;

	void convertLvalue( std::list< Declaration* > & translationUnit ) {
		PassVisitor<ReferenceConversions> refCvt;
		PassVisitor<ReferenceTypeElimination> elim;
		PassVisitor<GeneralizedLvalue> genLval;
		PassVisitor<FixIntrinsicArgs> fixer;
		PassVisitor<CollapseAddrDeref> collapser;
		PassVisitor<AddrRef> addrRef;
		PassVisitor<FixIntrinsicResult> intrinsicResults;
		mutateAll( translationUnit, intrinsicResults );
		mutateAll( translationUnit, addrRef );
		mutateAll( translationUnit, refCvt );
		mutateAll( translationUnit, fixer );
		mutateAll( translationUnit, collapser );
		mutateAll( translationUnit, genLval );
		mutateAll( translationUnit, elim );  // last because other passes need reference types to work

		// from this point forward, no other pass should create reference types.
		referencesEliminated = true;
	}

	Expression * generalizedLvalue( Expression * expr ) {
		PassVisitor<GeneralizedLvalue> genLval;
		return expr->acceptMutator( genLval );
	}

	namespace {
		// true for intrinsic function calls that return an lvalue in C
		bool isIntrinsicReference( Expression * expr ) {
			// known intrinsic-reference prelude functions
			static std::set<std::string> lvalueFunctions = { "*?", "?[?]" };
			if ( UntypedExpr * untyped = dynamic_cast< UntypedExpr * >( expr ) ) {
				std::string fname = InitTweak::getFunctionName( untyped );
				return lvalueFunctions.count(fname);
			} else if ( ApplicationExpr * appExpr = dynamic_cast< ApplicationExpr * > ( expr ) ) {
				if ( DeclarationWithType * func = InitTweak::getFunction( appExpr ) ) {
					return func->linkage == LinkageSpec::Intrinsic && lvalueFunctions.count(func->name);
				}
			}
			return false;
		}

		Expression * FixIntrinsicResult::postmutate( ApplicationExpr * appExpr ) {
			if ( skip != SkipInProgress && isIntrinsicReference( appExpr ) ) {
				// eliminate reference types from intrinsic applications - now they return lvalues
				ReferenceType * result = strict_dynamic_cast< ReferenceType * >( appExpr->result );
				appExpr->result = result->base->clone();
				if ( ! inIntrinsic ) {
					// when not in an intrinsic function, add a cast to
					// don't add cast when in an intrinsic function, since they already have the cast
					Expression * ret = new CastExpr( appExpr, result );
					std::swap( ret->env, appExpr->env );
					return ret;
				}
				delete result;
			}
			return appExpr;
		}

		void FixIntrinsicResult::premutate( FunctionDecl * funcDecl ) {
			GuardValue( inIntrinsic );
			inIntrinsic = funcDecl->linkage == LinkageSpec::Intrinsic;
		}

		Expression * FixIntrinsicArgs::postmutate( ApplicationExpr * appExpr ) {
			// intrinsic functions don't really take reference-typed parameters, so they require an implicit dereference on their arguments.
			if ( DeclarationWithType * function = InitTweak::getFunction( appExpr ) ) {
				FunctionType * ftype = GenPoly::getFunctionType( function->get_type() );
				assertf( ftype, "Function declaration does not have function type." );
				// can be of differing lengths only when function is variadic
				assertf( ftype->parameters.size() == appExpr->args.size() || ftype->isVarArgs, "ApplicationExpr args do not match formal parameter type." );


				unsigned int i = 0;
				const unsigned int end = ftype->parameters.size();

				/// The for loop may eagerly dereference the iterators and fail on empty lists
				if(i == end) { return appExpr; }
				for ( auto p : unsafe_group_iterate( appExpr->args, ftype->parameters ) ) {
					Expression *& arg = std::get<0>( p );
					Type * formal = std::get<1>( p )->get_type();
					PRINT(
						std::cerr << "pair<0>: " << arg << std::endl;
						std::cerr << " -- " << arg->result << std::endl;
						std::cerr << "pair<1>: " << formal << std::endl;
					)
					if ( dynamic_cast<ReferenceType*>( formal ) ) {
						PRINT(
							std::cerr << "===formal is reference" << std::endl;
						)
						// TODO: it's likely that the second condition should be ... && ! isIntrinsicReference( arg ), but this requires investigation.

						if ( function->linkage != LinkageSpec::Intrinsic && isIntrinsicReference( arg ) ) {
							// needed for definition of prelude functions, etc.
							// if argument is dereference or array subscript, the result isn't REALLY a reference, but non-intrinsic functions expect a reference: take address

							// NOTE: previously, this condition fixed
							//   void f(int *&);
							//   int & x = ...;
							//   f(&x);
							// But now this is taken care of by a reference cast added by AddrRef. Need to find a new
							// example or remove this branch.

							PRINT(
								std::cerr << "===is intrinsic arg in non-intrinsic call - adding address" << std::endl;
							)
							arg = new AddressExpr( arg );
						// } else if ( function->get_linkage() == LinkageSpec::Intrinsic && InitTweak::getPointerBase( arg->result ) ) {
						} else if ( function->linkage == LinkageSpec::Intrinsic && arg->result->referenceDepth() != 0 ) {
							// argument is a 'real' reference, but function expects a C lvalue: add a dereference to the reference-typed argument
							PRINT(
								std::cerr << "===is non-intrinsic arg in intrinsic call - adding deref to arg" << std::endl;
							)
							Type * baseType = InitTweak::getPointerBase( arg->result );
							assertf( baseType, "parameter is reference, arg must be pointer or reference: %s", toString( arg->result ).c_str() );
							PointerType * ptrType = new PointerType( Type::Qualifiers(), baseType->clone() );
							delete arg->result;
							arg->result = ptrType;
							arg = mkDeref( arg );
							// assertf( arg->result->referenceDepth() == 0, "Reference types should have been eliminated from intrinsic function calls, but weren't: %s", toCString( arg->result ) );
						}
					}
					++i;
					if (i == end) break;
				}
			}
			return appExpr;
		}

		// idea: &&&E: get outer &, inner &
		// at inner &, record depth D of reference type of argument of &
		// at outer &, add D derefs.
		void AddrRef::handleNonAddr( Expression * ) {
			// non-address-of: reset status variables:
			// * current expr is NOT the first address-of expr in an address-of chain
			// * next seen address-of expr IS the first in the chain.
			GuardValue( current );
			GuardValue( first );
			current = false;
			first = true;
		}

		void AddrRef::premutate( Expression * expr ) {
			handleNonAddr( expr );
			GuardValue( addCast );
			addCast = false;
		}

		void AddrRef::premutate( AddressExpr * ) {
			GuardValue( current );
			GuardValue( first );
			current = first; // is this the first address-of in the chain?
			first = false;   // from here out, no longer possible for next address-of to be first in chain
			if ( current ) { // this is the outermost address-of in a chain
				GuardValue( refDepth );
				refDepth = 0;  // set depth to 0 so that postmutate can find the innermost address-of easily
			}
		}

		Expression * AddrRef::postmutate( AddressExpr * addrExpr ) {
			PRINT( std::cerr << "addr ref at " << addrExpr << std::endl; )
			if ( refDepth == 0 ) {
				PRINT( std::cerr << "depth 0, get new depth..." << std::endl; )
				// this is the innermost address-of in a chain, record depth D
				if ( ! isIntrinsicReference( addrExpr->arg ) ) {
					// try to avoid ?[?]
					// xxx - is this condition still necessary? intrinsicReferences should have a cast around them at this point, so I don't think this condition ever fires.
					refDepth = addrExpr->arg->result->referenceDepth();
					PRINT( std::cerr << "arg not intrinsic reference, new depth is: " << refDepth << std::endl; )
				} else {
					assertf( false, "AddrRef : address-of should not have intrinsic reference argument: %s", toCString( addrExpr->arg ) );
				}
			}
			if ( current ) { // this is the outermost address-of in a chain
				PRINT( std::cerr << "current, depth is: " << refDepth << std::endl; )
				Expression * ret = addrExpr;
				while ( refDepth ) {
					// add one dereference for each
					ret = mkDeref( ret );
					refDepth--;
				}

				// if addrExpr depth is 0, then the result is a pointer because the arg was depth 1 and not lvalue.
				// This means the dereference result is not a reference, is lvalue, and one less pointer depth than
				// the addrExpr. Thus the cast is meaningless.
				// TODO: One thing to double check is whether it is possible for the types to differ outside of the single
				// pointer level (i.e. can the base type of addrExpr differ from the type of addrExpr-arg?).
				// If so then the cast might need to be added, conditional on a more sophisticated check.
				if ( addCast && addrExpr->result->referenceDepth() != 0 ) {
					PRINT( std::cerr << "adding cast to " << addrExpr->result << std::endl; )
					return new CastExpr( ret, addrExpr->result->clone() );
				}
				return ret;
			}
			PRINT( std::cerr << "not current..." << std::endl; )
			return addrExpr;
		}

		void AddrRef::premutate( ApplicationExpr * appExpr ) {
			visit_children = false;
			GuardValue( addCast );
			handleNonAddr( appExpr );
			for ( Expression *& arg : appExpr->args ) {
				// each argument with address-of requires a cast
				addCast = true;
				arg = arg->acceptMutator( *visitor );
			}
		}

		void AddrRef::premutate( SingleInit * ) {
			GuardValue( addCast );
			// each initialization context with address-of requires a cast
			addCast = true;
		}


		Expression * ReferenceConversions::postmutate( AddressExpr * addrExpr ) {
			// Inner expression may have been lvalue to reference conversion, which becomes an address expression.
			// In this case, remove the outer address expression and return the argument.
			// TODO: It's possible that this might catch too much and require a more sophisticated check.
			return addrExpr;
		}

		Expression * ReferenceConversions::postmutate( CastExpr * castExpr ) {
			// xxx - is it possible to convert directly between reference types with a different base? E.g.,
			//   int x;
			//   (double&)x;
			// At the moment, I am working off of the assumption that this is illegal, thus the cast becomes redundant
			// after this pass, so trash the cast altogether. If that changes, care must be taken to insert the correct
			// pointer casts in the right places.

			// Note: reference depth difference is the determining factor in what code is run, rather than whether something is
			// reference type or not, since conversion still needs to occur when both types are references that differ in depth.

			Type * destType = castExpr->result;
			Type * srcType = castExpr->arg->result;
			assertf( destType, "Cast to no type in: %s", toCString( castExpr ) );
			assertf( srcType, "Cast from no type in: %s", toCString( castExpr ) );
			int depth1 = destType->referenceDepth();
			int depth2 = srcType->referenceDepth();
			int diff = depth1 - depth2;

			if ( diff > 0 && ! castExpr->arg->get_lvalue() ) {
				// rvalue to reference conversion -- introduce temporary
				// know that reference depth of cast argument is 0, need to introduce n temporaries for reference depth of n, e.g.
				//   (int &&&)3;
				// becomes
				//   int __ref_tmp_0 = 3;
				//   int & __ref_tmp_1 = _&_ref_tmp_0;
				//   int && __ref_tmp_2 = &__ref_tmp_1;
				//   &__ref_tmp_2;
				// the last & comes from the remaining reference conversion code
				SemanticWarning( castExpr->arg->location, Warning::RvalueToReferenceConversion, toCString( castExpr->arg ) );

				static UniqueName tempNamer( "__ref_tmp_" );
				ObjectDecl * temp = ObjectDecl::newObject( tempNamer.newName(), castExpr->arg->result->clone(), new SingleInit( castExpr->arg ) );
				PRINT( std::cerr << "made temp: " << temp << std::endl; )
				stmtsToAddBefore.push_back( new DeclStmt( temp ) );
				for ( int i = 0; i < depth1-1; i++ ) { // xxx - maybe this should be diff-1? check how this works with reference type for srcType
					ObjectDecl * newTemp = ObjectDecl::newObject( tempNamer.newName(), new ReferenceType( Type::Qualifiers(), temp->type->clone() ), new SingleInit( new AddressExpr( new VariableExpr( temp ) ) ) );
					PRINT( std::cerr << "made temp" << i << ": " << newTemp << std::endl; )
					stmtsToAddBefore.push_back( new DeclStmt( newTemp ) );
					temp = newTemp;
				}
				// update diff so that remaining code works out correctly
				castExpr->arg = new VariableExpr( temp );
				PRINT( std::cerr << "update cast to: " << castExpr << std::endl; )
				srcType = castExpr->arg->result;
				depth2 = srcType->referenceDepth();
				diff = depth1 - depth2;
				assert( diff == 1 );
			}

			// handle conversion between different depths
			PRINT (
				if ( depth1 || depth2 ) {
					std::cerr << "destType: " << destType << " / srcType: " << srcType << std::endl;
					std::cerr << "depth: " << depth1 << " / " << depth2 << std::endl;
				}
			)
			if ( diff > 0 ) {
				// conversion to type with more depth (e.g. int & -> int &&): add address-of for each level of difference
				Expression * ret = castExpr->arg;
				for ( int i = 0; i < diff; ++i ) {
					ret = new AddressExpr( ret );
				}
				if ( castExpr->arg->get_lvalue() && ! ResolvExpr::typesCompatible( srcType, strict_dynamic_cast<ReferenceType *>( destType )->base, SymTab::Indexer() ) ) {
					// must keep cast if cast-to type is different from the actual type
					castExpr->arg = ret;
					return castExpr;
				}
				ret->env = castExpr->env;
				delete ret->result;
				ret->result = castExpr->result;
				castExpr->env = nullptr;
				castExpr->arg = nullptr;
				castExpr->result = nullptr;
				delete castExpr;
				return ret;
			} else if ( diff < 0 ) {
				// conversion to type with less depth (e.g. int && -> int &): add dereferences for each level of difference
				diff = -diff; // care only about magnitude now
				Expression * ret = castExpr->arg;
				for ( int i = 0; i < diff; ++i ) {
					ret = mkDeref( ret );
					// xxx - try removing one reference here? actually, looks like mkDeref already does this, so more closely look at the types generated.
				}
				if ( ! ResolvExpr::typesCompatibleIgnoreQualifiers( destType->stripReferences(), srcType->stripReferences(), SymTab::Indexer() ) ) {
					// must keep cast if types are different
					castExpr->arg = ret;
					return castExpr;
				}
				ret->env = castExpr->env;
				delete ret->result;
				ret->result = castExpr->result;
				assert( ret->get_lvalue() ); // ensure result is lvalue
				castExpr->env = nullptr;
				castExpr->arg = nullptr;
				castExpr->result = nullptr;
				delete castExpr;
				return ret;
			} else {
				assert( diff == 0 );
				// conversion between references of the same depth
				if ( ResolvExpr::typesCompatible( castExpr->result, castExpr->arg->result, SymTab::Indexer() ) && castExpr->isGenerated ) {
					// Remove useless generated casts
					PRINT(
						std::cerr << "types are compatible, removing cast: " << castExpr << std::endl;
						std::cerr << "-- " << castExpr->result << std::endl;
						std::cerr << "-- " << castExpr->arg->result << std::endl;
					)
					Expression * ret = castExpr->arg;
					castExpr->arg = nullptr;
					std::swap( castExpr->env, ret->env );
					delete castExpr;
					return ret;
				}
				return castExpr;
			}
		}

		Type * ReferenceTypeElimination::postmutate( ReferenceType * refType ) {
			Type * base = refType->base;
			Type::Qualifiers qualifiers = refType->get_qualifiers();
			refType->base = nullptr;
			delete refType;
			return new PointerType( qualifiers, base );
		}

		template<typename Func>
		Expression * GeneralizedLvalue::applyTransformation( Expression * expr, Expression * arg, Func mkExpr ) {
			if ( CommaExpr * commaExpr = dynamic_cast< CommaExpr * >( arg ) ) {
				Expression * arg1 = commaExpr->arg1->clone();
				Expression * arg2 = commaExpr->arg2->clone();
				Expression * ret = new CommaExpr( arg1, mkExpr( arg2 )->acceptMutator( *visitor ) );
				ret->env = expr->env;
				expr->env = nullptr;
				delete expr;
				return ret;
			} else if ( ConditionalExpr * condExpr = dynamic_cast< ConditionalExpr * >( arg ) ) {
				Expression * arg1 = condExpr->arg1->clone();
				Expression * arg2 = condExpr->arg2->clone();
				Expression * arg3 = condExpr->arg3->clone();
				ConditionalExpr * ret = new ConditionalExpr( arg1, mkExpr( arg2 )->acceptMutator( *visitor ), mkExpr( arg3 )->acceptMutator( *visitor ) );
				ret->env = expr->env;
				expr->env = nullptr;
				delete expr;

				// conditional expr type may not be either of the argument types, need to unify
				using namespace ResolvExpr;
				Type* commonType = nullptr;
				TypeEnvironment newEnv;
				AssertionSet needAssertions, haveAssertions;
				OpenVarSet openVars;
				unify( ret->arg2->result, ret->arg3->result, newEnv, needAssertions, haveAssertions, openVars, SymTab::Indexer(), commonType );
				ret->result = commonType ? commonType : ret->arg2->result->clone();
				return ret;
			}
			return expr;
		}

		Expression * GeneralizedLvalue::postmutate( MemberExpr * memExpr ) {
			return applyTransformation( memExpr, memExpr->aggregate, [=]( Expression * aggr ) { return new MemberExpr( memExpr->member, aggr ); } );
		}

		Expression * GeneralizedLvalue::postmutate( AddressExpr * addrExpr ) {
			return applyTransformation( addrExpr, addrExpr->arg, []( Expression * arg ) { return new AddressExpr( arg ); } );
		}

		Expression * CollapseAddrDeref::postmutate( AddressExpr * addrExpr ) {
			Expression * arg = addrExpr->arg;
			if ( isIntrinsicReference( arg ) ) {
				std::string fname = InitTweak::getFunctionName( arg );
				if ( fname == "*?" ) {
					Expression *& arg0 = InitTweak::getCallArg( arg, 0 );
					Expression * ret = arg0;
					ret->set_env( addrExpr->env );
					arg0 = nullptr;
					addrExpr->env = nullptr;
					delete addrExpr;
					return ret;
				}
			} else if ( CastExpr * castExpr = dynamic_cast< CastExpr * > ( arg ) ) {
				// need to move cast to pointer type out a level since address of pointer
				// is not valid C code (can be introduced in prior passes, e.g., InstantiateGeneric)
				if ( InitTweak::getPointerBase( castExpr->result ) ) {
					addrExpr->arg = castExpr->arg;
					castExpr->arg = addrExpr;
					castExpr->result = new PointerType( Type::Qualifiers(), castExpr->result );
					return castExpr;
				}
			}
			return addrExpr;
		}

		Expression * CollapseAddrDeref::postmutate( ApplicationExpr * appExpr ) {
			if ( isIntrinsicReference( appExpr ) ) {
				std::string fname = InitTweak::getFunctionName( appExpr );
				if ( fname == "*?" ) {
					Expression * arg = InitTweak::getCallArg( appExpr, 0 );
					// xxx - this isn't right, because it can remove casts that should be there...
					// while ( CastExpr * castExpr = dynamic_cast< CastExpr * >( arg ) ) {
					// 	arg = castExpr->get_arg();
					// }
					if ( AddressExpr * addrExpr = dynamic_cast< AddressExpr * >( arg ) ) {
						Expression * ret = addrExpr->arg;
						ret->env = appExpr->env;
						addrExpr->arg = nullptr;
						appExpr->env = nullptr;
						delete appExpr;
						return ret;
					}
				}
			}
			return appExpr;
		}
	} // namespace
} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
