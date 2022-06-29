//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Validate.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 21:50:04 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue May 17 14:36:00 2022
// Update Count     : 366
//

// The "validate" phase of translation is used to take a syntax tree and convert it into a standard form that aims to be
// as regular in structure as possible.  Some assumptions can be made regarding the state of the tree after this pass is
// complete, including:
//
// - No nested structure or union definitions; any in the input are "hoisted" to the level of the containing struct or
//   union.
//
// - All enumeration constants have type EnumInstType.
//
// - The type "void" never occurs in lists of function parameter or return types.  A function
//   taking no arguments has no argument types.
//
// - No context instances exist; they are all replaced by the set of declarations signified by the context, instantiated
//   by the particular set of type arguments.
//
// - Every declaration is assigned a unique id.
//
// - No typedef declarations or instances exist; the actual type is substituted for each instance.
//
// - Each type, struct, and union definition is followed by an appropriate assignment operator.
//
// - Each use of a struct or union is connected to a complete definition of that struct or union, even if that
//   definition occurs later in the input.

#include "Validate.h"

#include <cassert>                     // for assertf, assert
#include <cstddef>                     // for size_t
#include <list>                        // for list
#include <string>                      // for string
#include <unordered_map>               // for unordered_map
#include <utility>                     // for pair

#include "AST/Chain.hpp"
#include "AST/Decl.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/Type.hpp"
#include "AST/TypeSubstitution.hpp"
#include "CodeGen/CodeGenerator.h"     // for genName
#include "CodeGen/OperatorTable.h"     // for isCtorDtor, isCtorDtorAssign
#include "ControlStruct/Mutate.h"      // for ForExprMutator
#include "Common/CodeLocation.h"       // for CodeLocation
#include "Common/Stats.h"              // for Stats::Heap
#include "Common/PassVisitor.h"        // for PassVisitor, WithDeclsToAdd
#include "Common/ScopedMap.h"          // for ScopedMap
#include "Common/SemanticError.h"      // for SemanticError
#include "Common/UniqueName.h"         // for UniqueName
#include "Common/utility.h"            // for operator+, cloneAll, deleteAll
#include "CompilationState.h"          // skip some passes in new-ast build
#include "Concurrency/Keywords.h"      // for applyKeywords
#include "FixFunction.h"               // for FixFunction
#include "Indexer.h"                   // for Indexer
#include "InitTweak/GenInit.h"         // for fixReturnStatements
#include "InitTweak/InitTweak.h"       // for isCtorDtorAssign
#include "ResolvExpr/typeops.h"        // for typesCompatible
#include "ResolvExpr/Resolver.h"       // for findSingleExpression
#include "ResolvExpr/ResolveTypeof.h"  // for resolveTypeof
#include "SymTab/Autogen.h"            // for SizeType
#include "SymTab/ValidateType.h"       // for decayEnumsAndPointers, decayFo...
#include "SynTree/LinkageSpec.h"       // for C
#include "SynTree/Attribute.h"         // for noAttributes, Attribute
#include "SynTree/Constant.h"          // for Constant
#include "SynTree/Declaration.h"       // for ObjectDecl, DeclarationWithType
#include "SynTree/Expression.h"        // for CompoundLiteralExpr, Expressio...
#include "SynTree/Initializer.h"       // for ListInit, Initializer
#include "SynTree/Label.h"             // for operator==, Label
#include "SynTree/Mutator.h"           // for Mutator
#include "SynTree/Type.h"              // for Type, TypeInstType, EnumInstType
#include "SynTree/TypeSubstitution.h"  // for TypeSubstitution
#include "SynTree/Visitor.h"           // for Visitor
#include "Validate/HandleAttributes.h" // for handleAttributes
#include "Validate/FindSpecialDecls.h" // for FindSpecialDecls

class CompoundStmt;
class ReturnStmt;
class SwitchStmt;

#define debugPrint( x ) if ( doDebug ) x

namespace SymTab {
	/// hoists declarations that are difficult to hoist while parsing
	struct HoistTypeDecls final : public WithDeclsToAdd {
		void previsit( SizeofExpr * );
		void previsit( AlignofExpr * );
		void previsit( UntypedOffsetofExpr * );
		void previsit( CompoundLiteralExpr * );
		void handleType( Type * );
	};

	struct FixQualifiedTypes final : public WithIndexer {
		FixQualifiedTypes() : WithIndexer(false) {}
		Type * postmutate( QualifiedType * );
	};

	struct HoistStruct final : public WithDeclsToAdd, public WithGuards {
		/// Flattens nested struct types
		static void hoistStruct( std::list< Declaration * > &translationUnit );

		void previsit( StructDecl * aggregateDecl );
		void previsit( UnionDecl * aggregateDecl );
		void previsit( StaticAssertDecl * assertDecl );
		void previsit( StructInstType * type );
		void previsit( UnionInstType * type );
		void previsit( EnumInstType * type );

	  private:
		template< typename AggDecl > void handleAggregate( AggDecl * aggregateDecl );

		AggregateDecl * parentAggr = nullptr;
	};

	/// Fix return types so that every function returns exactly one value
	struct ReturnTypeFixer {
		static void fix( std::list< Declaration * > &translationUnit );

		void postvisit( FunctionDecl * functionDecl );
		void postvisit( FunctionType * ftype );
	};

	/// Does early resolution on the expressions that give enumeration constants their values
	struct ResolveEnumInitializers final : public WithIndexer, public WithGuards, public WithVisitorRef<ResolveEnumInitializers>, public WithShortCircuiting {
		ResolveEnumInitializers( const Indexer * indexer );
		void postvisit( EnumDecl * enumDecl );

	  private:
		const Indexer * local_indexer;

	};

	/// Replaces array and function types in forall lists by appropriate pointer type and assigns each Object and Function declaration a unique ID.
	struct ForallPointerDecay_old final {
		void previsit( ObjectDecl * object );
		void previsit( FunctionDecl * func );
		void previsit( FunctionType * ftype );
		void previsit( StructDecl * aggrDecl );
		void previsit( UnionDecl * aggrDecl );
	};

	struct ReturnChecker : public WithGuards {
		/// Checks that return statements return nothing if their return type is void
		/// and return something if the return type is non-void.
		static void checkFunctionReturns( std::list< Declaration * > & translationUnit );

		void previsit( FunctionDecl * functionDecl );
		void previsit( ReturnStmt * returnStmt );

		typedef std::list< DeclarationWithType * > ReturnVals;
		ReturnVals returnVals;
	};

	struct ReplaceTypedef final : public WithVisitorRef<ReplaceTypedef>, public WithGuards, public WithShortCircuiting, public WithDeclsToAdd {
		ReplaceTypedef() : scopeLevel( 0 ) {}
		/// Replaces typedefs by forward declarations
		static void replaceTypedef( std::list< Declaration * > &translationUnit );

		void premutate( QualifiedType * );
		Type * postmutate( QualifiedType * qualType );
		Type * postmutate( TypeInstType * aggregateUseType );
		Declaration * postmutate( TypedefDecl * typeDecl );
		void premutate( TypeDecl * typeDecl );
		void premutate( FunctionDecl * funcDecl );
		void premutate( ObjectDecl * objDecl );
		DeclarationWithType * postmutate( ObjectDecl * objDecl );

		void premutate( CastExpr * castExpr );

		void premutate( CompoundStmt * compoundStmt );

		void premutate( StructDecl * structDecl );
		void premutate( UnionDecl * unionDecl );
		void premutate( EnumDecl * enumDecl );
		void premutate( TraitDecl * );

		void premutate( FunctionType * ftype );

	  private:
		template<typename AggDecl>
		void addImplicitTypedef( AggDecl * aggDecl );
		template< typename AggDecl >
		void handleAggregate( AggDecl * aggr );

		typedef std::unique_ptr<TypedefDecl> TypedefDeclPtr;
		typedef ScopedMap< std::string, std::pair< TypedefDeclPtr, int > > TypedefMap;
		typedef ScopedMap< std::string, TypeDecl * > TypeDeclMap;
		TypedefMap typedefNames;
		TypeDeclMap typedeclNames;
		int scopeLevel;
		bool inFunctionType = false;
	};

	struct EliminateTypedef {
		/// removes TypedefDecls from the AST
		static void eliminateTypedef( std::list< Declaration * > &translationUnit );

		template<typename AggDecl>
		void handleAggregate( AggDecl * aggregateDecl );

		void previsit( StructDecl * aggregateDecl );
		void previsit( UnionDecl * aggregateDecl );
		void previsit( CompoundStmt * compoundStmt );
	};

	struct VerifyCtorDtorAssign {
		/// ensure that constructors, destructors, and assignment have at least one
		/// parameter, the first of which must be a pointer, and that ctor/dtors have no
		/// return values.
		static void verify( std::list< Declaration * > &translationUnit );

		void previsit( FunctionDecl * funcDecl );
	};

	/// ensure that generic types have the correct number of type arguments
	struct ValidateGenericParameters {
		void previsit( StructInstType * inst );
		void previsit( UnionInstType * inst );
	};

	/// desugar declarations and uses of dimension paramaters like [N],
	/// from type-system managed values, to tunnneling via ordinary types,
	/// as char[-] in and sizeof(-) out
	struct TranslateDimensionGenericParameters : public WithIndexer, public WithGuards {
		static void translateDimensions( std::list< Declaration * > &translationUnit );
		TranslateDimensionGenericParameters();

		bool nextVisitedNodeIsChildOfSUIT = false; // SUIT = Struct or Union -Inst Type
		bool visitingChildOfSUIT = false;
		void changeState_ChildOfSUIT( bool newVal );
		void premutate( StructInstType * sit );
		void premutate( UnionInstType * uit );
		void premutate( BaseSyntaxNode * node );

		TypeDecl * postmutate( TypeDecl * td );
		Expression * postmutate( DimensionExpr * de );
		Expression * postmutate( Expression * e );
	};

	struct FixObjectType : public WithIndexer {
		/// resolves typeof type in object, function, and type declarations
		static void fix( std::list< Declaration * > & translationUnit );

		void previsit( ObjectDecl * );
		void previsit( FunctionDecl * );
		void previsit( TypeDecl * );
	};

	struct InitializerLength {
		/// for array types without an explicit length, compute the length and store it so that it
		/// is known to the rest of the phases. For example,
		///   int x[] = { 1, 2, 3 };
		///   int y[][2] = { { 1, 2, 3 }, { 1, 2, 3 } };
		/// here x and y are known at compile-time to have length 3, so change this into
		///   int x[3] = { 1, 2, 3 };
		///   int y[3][2] = { { 1, 2, 3 }, { 1, 2, 3 } };
		static void computeLength( std::list< Declaration * > & translationUnit );

		void previsit( ObjectDecl * objDecl );
	};

	struct ArrayLength : public WithIndexer {
		static void computeLength( std::list< Declaration * > & translationUnit );

		void previsit( ArrayType * arrayType );
	};

	struct CompoundLiteral final : public WithDeclsToAdd, public WithVisitorRef<CompoundLiteral> {
		Type::StorageClasses storageClasses;

		void premutate( ObjectDecl * objectDecl );
		Expression * postmutate( CompoundLiteralExpr * compLitExpr );
	};

	struct LabelAddressFixer final : public WithGuards {
		std::set< Label > labels;

		void premutate( FunctionDecl * funcDecl );
		Expression * postmutate( AddressExpr * addrExpr );
	};

	void validate_A( std::list< Declaration * > & translationUnit ) {
		PassVisitor<HoistTypeDecls> hoistDecls;
		{
			Stats::Heap::newPass("validate-A");
			Stats::Time::BlockGuard guard("validate-A");
			VerifyCtorDtorAssign::verify( translationUnit );  // must happen before autogen, because autogen examines existing ctor/dtors
			acceptAll( translationUnit, hoistDecls );
			ReplaceTypedef::replaceTypedef( translationUnit );
			ReturnTypeFixer::fix( translationUnit ); // must happen before autogen
			decayEnumsAndPointers( translationUnit ); // must happen before VerifyCtorDtorAssign, because void return objects should not exist; before LinkReferenceToTypes_old because it is an indexer and needs correct types for mangling
		}
	}

	void validate_B( std::list< Declaration * > & translationUnit ) {
		PassVisitor<FixQualifiedTypes> fixQual;
		{
			Stats::Heap::newPass("validate-B");
			Stats::Time::BlockGuard guard("validate-B");
			linkReferenceToTypes( translationUnit ); // Must happen before auto-gen, because it uses the sized flag.
			mutateAll( translationUnit, fixQual ); // must happen after LinkReferenceToTypes_old, because aggregate members are accessed
			HoistStruct::hoistStruct( translationUnit );
			EliminateTypedef::eliminateTypedef( translationUnit );
		}
	}

	void validate_C( std::list< Declaration * > & translationUnit ) {
		PassVisitor<ValidateGenericParameters> genericParams;
		PassVisitor<ResolveEnumInitializers> rei( nullptr );
		{
			Stats::Heap::newPass("validate-C");
			Stats::Time::BlockGuard guard("validate-C");
			Stats::Time::TimeBlock("Validate Generic Parameters", [&]() {
				acceptAll( translationUnit, genericParams );  // check as early as possible - can't happen before LinkReferenceToTypes_old; observed failing when attempted before eliminateTypedef
			});
			Stats::Time::TimeBlock("Translate Dimensions", [&]() {
				TranslateDimensionGenericParameters::translateDimensions( translationUnit );
			});
			if (!useNewAST) {
			Stats::Time::TimeBlock("Resolve Enum Initializers", [&]() {
				acceptAll( translationUnit, rei ); // must happen after translateDimensions because rei needs identifier lookup, which needs name mangling
			});
			}
			Stats::Time::TimeBlock("Check Function Returns", [&]() {
				ReturnChecker::checkFunctionReturns( translationUnit );
			});
			Stats::Time::TimeBlock("Fix Return Statements", [&]() {
				InitTweak::fixReturnStatements( translationUnit ); // must happen before autogen
			});
		}
	}

	void validate_D( std::list< Declaration * > & translationUnit ) {
		{
			Stats::Heap::newPass("validate-D");
			Stats::Time::BlockGuard guard("validate-D");
			Stats::Time::TimeBlock("Apply Concurrent Keywords", [&]() {
				Concurrency::applyKeywords( translationUnit );
			});
			Stats::Time::TimeBlock("Forall Pointer Decay", [&]() {
				decayForallPointers( translationUnit ); // must happen before autogenerateRoutines, after Concurrency::applyKeywords because uniqueIds must be set on declaration before resolution
			});
			Stats::Time::TimeBlock("Hoist Control Declarations", [&]() {
				ControlStruct::hoistControlDecls( translationUnit );  // hoist initialization out of for statements; must happen before autogenerateRoutines
			});
			Stats::Time::TimeBlock("Generate Autogen routines", [&]() {
				autogenerateRoutines( translationUnit ); // moved up, used to be below compoundLiteral - currently needs EnumAndPointerDecay_old
			});
		}
	}

	void validate_E( std::list< Declaration * > & translationUnit ) {
		PassVisitor<CompoundLiteral> compoundliteral;
		{
			Stats::Heap::newPass("validate-E");
			Stats::Time::BlockGuard guard("validate-E");
			Stats::Time::TimeBlock("Implement Mutex Func", [&]() {
				Concurrency::implementMutexFuncs( translationUnit );
			});
			Stats::Time::TimeBlock("Implement Thread Start", [&]() {
				Concurrency::implementThreadStarter( translationUnit );
			});
			Stats::Time::TimeBlock("Compound Literal", [&]() {
				mutateAll( translationUnit, compoundliteral );
			});
			if (!useNewAST) {
				Stats::Time::TimeBlock("Resolve With Expressions", [&]() {
					ResolvExpr::resolveWithExprs( translationUnit ); // must happen before FixObjectType because user-code is resolved and may contain with variables
				});
			}
		}
	}

	void validate_F( std::list< Declaration * > & translationUnit ) {
		PassVisitor<LabelAddressFixer> labelAddrFixer;
		{
			Stats::Heap::newPass("validate-F");
			Stats::Time::BlockGuard guard("validate-F");
			if (!useNewAST) {
				Stats::Time::TimeCall("Fix Object Type",
					FixObjectType::fix, translationUnit);
			}
			Stats::Time::TimeCall("Initializer Length",
				InitializerLength::computeLength, translationUnit);
			if (!useNewAST) {
				Stats::Time::TimeCall("Array Length",
					ArrayLength::computeLength, translationUnit);
			}
			Stats::Time::TimeCall("Find Special Declarations",
				Validate::findSpecialDecls, translationUnit);
			Stats::Time::TimeCall("Fix Label Address",
				mutateAll<LabelAddressFixer>, translationUnit, labelAddrFixer);
			if (!useNewAST) {
				Stats::Time::TimeCall("Handle Attributes",
					Validate::handleAttributes, translationUnit);
			}
		}
	}

	void validate( std::list< Declaration * > &translationUnit, __attribute__((unused)) bool doDebug ) {
		validate_A( translationUnit );
		validate_B( translationUnit );
		validate_C( translationUnit );
		validate_D( translationUnit );
		validate_E( translationUnit );
		validate_F( translationUnit );
	}

	void HoistTypeDecls::handleType( Type * type ) {
		// some type declarations are buried in expressions and not easy to hoist during parsing; hoist them here
		AggregateDecl * aggr = nullptr;
		if ( StructInstType * inst = dynamic_cast< StructInstType * >( type ) ) {
			aggr = inst->baseStruct;
		} else if ( UnionInstType * inst = dynamic_cast< UnionInstType * >( type ) ) {
			aggr = inst->baseUnion;
		} else if ( EnumInstType * inst = dynamic_cast< EnumInstType * >( type ) ) {
			aggr = inst->baseEnum;
		}
		if ( aggr && aggr->body ) {
			declsToAddBefore.push_front( aggr );
		}
	}

	void HoistTypeDecls::previsit( SizeofExpr * expr ) {
		handleType( expr->type );
	}

	void HoistTypeDecls::previsit( AlignofExpr * expr ) {
		handleType( expr->type );
	}

	void HoistTypeDecls::previsit( UntypedOffsetofExpr * expr ) {
		handleType( expr->type );
	}

	void HoistTypeDecls::previsit( CompoundLiteralExpr * expr ) {
		handleType( expr->result );
	}


	Type * FixQualifiedTypes::postmutate( QualifiedType * qualType ) {
		Type * parent = qualType->parent;
		Type * child = qualType->child;
		if ( dynamic_cast< GlobalScopeType * >( qualType->parent ) ) {
			// .T => lookup T at global scope
			if ( TypeInstType * inst = dynamic_cast< TypeInstType * >( child ) ) {
				auto td = indexer.globalLookupType( inst->name );
				if ( ! td ) {
					SemanticError( qualType->location, toString("Use of undefined global type ", inst->name) );
				}
				auto base = td->base;
				assert( base );
				Type * ret = base->clone();
				ret->get_qualifiers() = qualType->get_qualifiers();
				return ret;
			} else {
				// .T => T is not a type name
				assertf( false, "unhandled global qualified child type: %s", toCString(child) );
			}
		} else {
			// S.T => S must be an aggregate type, find the declaration for T in S.
			AggregateDecl * aggr = nullptr;
			if ( StructInstType * inst = dynamic_cast< StructInstType * >( parent ) ) {
				aggr = inst->baseStruct;
			} else if ( UnionInstType * inst = dynamic_cast< UnionInstType * > ( parent ) ) {
				aggr = inst->baseUnion;
			} else {
				SemanticError( qualType->location, toString("Qualified type requires an aggregate on the left, but has: ", parent) );
			}
			assert( aggr ); // TODO: need to handle forward declarations
			for ( Declaration * member : aggr->members ) {
				if ( TypeInstType * inst = dynamic_cast< TypeInstType * >( child ) ) {
					// name on the right is a typedef
					if ( NamedTypeDecl * aggr = dynamic_cast< NamedTypeDecl * > ( member ) ) {
						if ( aggr->name == inst->name ) {
							assert( aggr->base );
							Type * ret = aggr->base->clone();
							ret->get_qualifiers() = qualType->get_qualifiers();
							TypeSubstitution sub = parent->genericSubstitution();
							sub.apply(ret);
							return ret;
						}
					}
				} else {
					// S.T - S is not an aggregate => error
					assertf( false, "unhandled qualified child type: %s", toCString(qualType) );
				}
			}
			// failed to find a satisfying definition of type
			SemanticError( qualType->location, toString("Undefined type in qualified type: ", qualType) );
		}

		// ... may want to link canonical SUE definition to each forward decl so that it becomes easier to lookup?
	}


	void HoistStruct::hoistStruct( std::list< Declaration * > &translationUnit ) {
		PassVisitor<HoistStruct> hoister;
		acceptAll( translationUnit, hoister );
	}

	bool shouldHoist( Declaration * decl ) {
		return dynamic_cast< StructDecl * >( decl ) || dynamic_cast< UnionDecl * >( decl ) || dynamic_cast< StaticAssertDecl * >( decl );
	}

	namespace {
		void qualifiedName( AggregateDecl * aggr, std::ostringstream & ss ) {
			if ( aggr->parent ) qualifiedName( aggr->parent, ss );
			ss << "__" << aggr->name;
		}

		// mangle nested type names using entire parent chain
		std::string qualifiedName( AggregateDecl * aggr ) {
			std::ostringstream ss;
			qualifiedName( aggr, ss );
			return ss.str();
		}
	}

	template< typename AggDecl >
	void HoistStruct::handleAggregate( AggDecl * aggregateDecl ) {
		if ( parentAggr ) {
			aggregateDecl->parent = parentAggr;
			aggregateDecl->name = qualifiedName( aggregateDecl );
			// Add elements in stack order corresponding to nesting structure.
			declsToAddBefore.push_front( aggregateDecl );
		} else {
			GuardValue( parentAggr );
			parentAggr = aggregateDecl;
		} // if
		// Always remove the hoisted aggregate from the inner structure.
		GuardAction( [aggregateDecl]() { filter( aggregateDecl->members, shouldHoist, false ); } );
	}

	void HoistStruct::previsit( StaticAssertDecl * assertDecl ) {
		if ( parentAggr ) {
			declsToAddBefore.push_back( assertDecl );
		}
	}

	void HoistStruct::previsit( StructDecl * aggregateDecl ) {
		handleAggregate( aggregateDecl );
	}

	void HoistStruct::previsit( UnionDecl * aggregateDecl ) {
		handleAggregate( aggregateDecl );
	}

	void HoistStruct::previsit( StructInstType * type ) {
		// need to reset type name after expanding to qualified name
		assert( type->baseStruct );
		type->name = type->baseStruct->name;
	}

	void HoistStruct::previsit( UnionInstType * type ) {
		assert( type->baseUnion );
		type->name = type->baseUnion->name;
	}

	void HoistStruct::previsit( EnumInstType * type ) {
		assert( type->baseEnum );
		type->name = type->baseEnum->name;
	}


	bool isTypedef( Declaration * decl ) {
		return dynamic_cast< TypedefDecl * >( decl );
	}

	void EliminateTypedef::eliminateTypedef( std::list< Declaration * > &translationUnit ) {
		PassVisitor<EliminateTypedef> eliminator;
		acceptAll( translationUnit, eliminator );
		filter( translationUnit, isTypedef, true );
	}

	template< typename AggDecl >
	void EliminateTypedef::handleAggregate( AggDecl * aggregateDecl ) {
		filter( aggregateDecl->members, isTypedef, true );
	}

	void EliminateTypedef::previsit( StructDecl * aggregateDecl ) {
		handleAggregate( aggregateDecl );
	}

	void EliminateTypedef::previsit( UnionDecl * aggregateDecl ) {
		handleAggregate( aggregateDecl );
	}

	void EliminateTypedef::previsit( CompoundStmt * compoundStmt ) {
		// remove and delete decl stmts
		filter( compoundStmt->kids, [](Statement * stmt) {
			if ( DeclStmt * declStmt = dynamic_cast< DeclStmt * >( stmt ) ) {
				if ( dynamic_cast< TypedefDecl * >( declStmt->decl ) ) {
					return true;
				} // if
			} // if
			return false;
		}, true);
	}

	// expand assertions from trait instance, performing the appropriate type variable substitutions
	template< typename Iterator >
	void expandAssertions( TraitInstType * inst, Iterator out ) {
		assertf( inst->baseTrait, "Trait instance not linked to base trait: %s", toCString( inst ) );
		std::list< DeclarationWithType * > asserts;
		for ( Declaration * decl : inst->baseTrait->members ) {
			asserts.push_back( strict_dynamic_cast<DeclarationWithType *>( decl->clone() ) );
		}
		// substitute trait decl parameters for instance parameters
		applySubstitution( inst->baseTrait->parameters.begin(), inst->baseTrait->parameters.end(), inst->parameters.begin(), asserts.begin(), asserts.end(), out );
	}

	ResolveEnumInitializers::ResolveEnumInitializers( const Indexer * other_indexer ) : WithIndexer( true ) {
		if ( other_indexer ) {
			local_indexer = other_indexer;
		} else {
			local_indexer = &indexer;
		} // if
	}

	void ResolveEnumInitializers::postvisit( EnumDecl * enumDecl ) {
		if ( enumDecl->body ) {
			for ( Declaration * member : enumDecl->members ) {
				ObjectDecl * field = strict_dynamic_cast<ObjectDecl *>( member );
				if ( field->init ) {
					// need to resolve enumerator initializers early so that other passes that determine if an expression is constexpr have the appropriate information.
					SingleInit * init = strict_dynamic_cast<SingleInit *>( field->init );
					if ( !enumDecl->base || dynamic_cast<BasicType *>(enumDecl->base))
						ResolvExpr::findSingleExpression( init->value, new BasicType( Type::Qualifiers(), BasicType::SignedInt ), indexer );
					else {
						if (dynamic_cast<PointerType *>(enumDecl->base)) {
							auto typePtr = dynamic_cast<PointerType *>(enumDecl->base);
							ResolvExpr::findSingleExpression( init->value,
							 new PointerType( Type::Qualifiers(), typePtr->base ), indexer );
						} else {
							ResolvExpr::findSingleExpression( init->value, new BasicType( Type::Qualifiers(), BasicType::SignedInt ), indexer );
						}
					}
				}
			}

		} // if
	}

	/// Fix up assertions - flattens assertion lists, removing all trait instances
	void forallFixer( std::list< TypeDecl * > & forall, BaseSyntaxNode * node ) {
		for ( TypeDecl * type : forall ) {
			std::list< DeclarationWithType * > asserts;
			asserts.splice( asserts.end(), type->assertions );
			// expand trait instances into their members
			for ( DeclarationWithType * assertion : asserts ) {
				if ( TraitInstType * traitInst = dynamic_cast< TraitInstType * >( assertion->get_type() ) ) {
					// expand trait instance into all of its members
					expandAssertions( traitInst, back_inserter( type->assertions ) );
					delete traitInst;
				} else {
					// pass other assertions through
					type->assertions.push_back( assertion );
				} // if
			} // for
			// apply FixFunction to every assertion to check for invalid void type
			for ( DeclarationWithType *& assertion : type->assertions ) {
				bool isVoid = fixFunction( assertion );
				if ( isVoid ) {
					SemanticError( node, "invalid type void in assertion of function " );
				} // if
			} // for
			// normalizeAssertions( type->assertions );
		} // for
	}

	/// Replace all traits in assertion lists with their assertions.
	void expandTraits( std::list< TypeDecl * > & forall ) {
		for ( TypeDecl * type : forall ) {
			std::list< DeclarationWithType * > asserts;
			asserts.splice( asserts.end(), type->assertions );
			// expand trait instances into their members
			for ( DeclarationWithType * assertion : asserts ) {
				if ( TraitInstType * traitInst = dynamic_cast< TraitInstType * >( assertion->get_type() ) ) {
					// expand trait instance into all of its members
					expandAssertions( traitInst, back_inserter( type->assertions ) );
					delete traitInst;
				} else {
					// pass other assertions through
					type->assertions.push_back( assertion );
				} // if
			} // for
		}
	}

	/// Fix each function in the assertion list and check for invalid void type.
	void fixAssertions(
			std::list< TypeDecl * > & forall, BaseSyntaxNode * node ) {
		for ( TypeDecl * type : forall ) {
			for ( DeclarationWithType *& assertion : type->assertions ) {
				bool isVoid = fixFunction( assertion );
				if ( isVoid ) {
					SemanticError( node, "invalid type void in assertion of function " );
				} // if
			} // for
		}
	}

	void ForallPointerDecay_old::previsit( ObjectDecl * object ) {
		// ensure that operator names only apply to functions or function pointers
		if ( CodeGen::isOperator( object->name ) && ! dynamic_cast< FunctionType * >( object->type->stripDeclarator() ) ) {
			SemanticError( object->location, toCString( "operator ", object->name.c_str(), " is not a function or function pointer." )  );
		}
		object->fixUniqueId();
	}

	void ForallPointerDecay_old::previsit( FunctionDecl * func ) {
		func->fixUniqueId();
	}

	void ForallPointerDecay_old::previsit( FunctionType * ftype ) {
		forallFixer( ftype->forall, ftype );
	}

	void ForallPointerDecay_old::previsit( StructDecl * aggrDecl ) {
		forallFixer( aggrDecl->parameters, aggrDecl );
	}

	void ForallPointerDecay_old::previsit( UnionDecl * aggrDecl ) {
		forallFixer( aggrDecl->parameters, aggrDecl );
	}

	void ReturnChecker::checkFunctionReturns( std::list< Declaration * > & translationUnit ) {
		PassVisitor<ReturnChecker> checker;
		acceptAll( translationUnit, checker );
	}

	void ReturnChecker::previsit( FunctionDecl * functionDecl ) {
		GuardValue( returnVals );
		returnVals = functionDecl->get_functionType()->get_returnVals();
	}

	void ReturnChecker::previsit( ReturnStmt * returnStmt ) {
		// Previously this also checked for the existence of an expr paired with no return values on
		// the  function return type. This is incorrect, since you can have an expression attached to
		// a return statement in a void-returning function in C. The expression is treated as if it
		// were cast to void.
		if ( ! returnStmt->get_expr() && returnVals.size() != 0 ) {
			SemanticError( returnStmt, "Non-void function returns no values: " );
		}
	}


	void ReplaceTypedef::replaceTypedef( std::list< Declaration * > &translationUnit ) {
		PassVisitor<ReplaceTypedef> eliminator;
		mutateAll( translationUnit, eliminator );
		if ( eliminator.pass.typedefNames.count( "size_t" ) ) {
			// grab and remember declaration of size_t
			Validate::SizeType = eliminator.pass.typedefNames["size_t"].first->base->clone();
		} else {
			// xxx - missing global typedef for size_t - default to long unsigned int, even though that may be wrong
			// eventually should have a warning for this case.
			Validate::SizeType = new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt );
		}
	}

	void ReplaceTypedef::premutate( QualifiedType * ) {
		visit_children = false;
	}

	Type * ReplaceTypedef::postmutate( QualifiedType * qualType ) {
		// replacing typedefs only makes sense for the 'oldest ancestor' of the qualified type
		qualType->parent = qualType->parent->acceptMutator( * visitor );
		return qualType;
	}

	static bool isNonParameterAttribute( Attribute * attr ) {
		static const std::vector<std::string> bad_names = {
			"aligned", "__aligned__",
		};
		for ( auto name : bad_names ) {
			if ( name == attr->name ) {
				return true;
			}
		}
		return false;
	}

	Type * ReplaceTypedef::postmutate( TypeInstType * typeInst ) {
		// instances of typedef types will come here. If it is an instance
		// of a typdef type, link the instance to its actual type.
		TypedefMap::const_iterator def = typedefNames.find( typeInst->name );
		if ( def != typedefNames.end() ) {
			Type * ret = def->second.first->base->clone();
			ret->location = typeInst->location;
			ret->get_qualifiers() |= typeInst->get_qualifiers();
			// GCC ignores certain attributes if they arrive by typedef, this mimics that.
			if ( inFunctionType ) {
				ret->attributes.remove_if( isNonParameterAttribute );
			}
			ret->attributes.splice( ret->attributes.end(), typeInst->attributes );
			// place instance parameters on the typedef'd type
			if ( ! typeInst->parameters.empty() ) {
				ReferenceToType * rtt = dynamic_cast<ReferenceToType *>(ret);
				if ( ! rtt ) {
					SemanticError( typeInst->location, "Cannot apply type parameters to base type of " + typeInst->name );
				}
				rtt->parameters.clear();
				cloneAll( typeInst->parameters, rtt->parameters );
				mutateAll( rtt->parameters, * visitor );  // recursively fix typedefs on parameters
			} // if
			delete typeInst;
			return ret;
		} else {
			TypeDeclMap::const_iterator base = typedeclNames.find( typeInst->name );
			if ( base == typedeclNames.end() ) {
				SemanticError( typeInst->location, toString("Use of undefined type ", typeInst->name) );
			}
			typeInst->set_baseType( base->second );
			return typeInst;
		} // if
		assert( false );
	}

	struct VarLenChecker : WithShortCircuiting {
		void previsit( FunctionType * ) { visit_children = false; }
		void previsit( ArrayType * at ) {
			isVarLen |= at->isVarLen;
		}
		bool isVarLen = false;
	};

	bool isVariableLength( Type * t ) {
		PassVisitor<VarLenChecker> varLenChecker;
		maybeAccept( t, varLenChecker );
		return varLenChecker.pass.isVarLen;
	}

	Declaration * ReplaceTypedef::postmutate( TypedefDecl * tyDecl ) {
		if ( typedefNames.count( tyDecl->name ) == 1 && typedefNames[ tyDecl->name ].second == scopeLevel ) {
			// typedef to the same name from the same scope
			// must be from the same type

			Type * t1 = tyDecl->base;
			Type * t2 = typedefNames[ tyDecl->name ].first->base;
			if ( ! ResolvExpr::typesCompatible( t1, t2, Indexer() ) ) {
				SemanticError( tyDecl->location, "Cannot redefine typedef: " + tyDecl->name );
			}
			// Cannot redefine VLA typedefs. Note: this is slightly incorrect, because our notion of VLAs
			// at this point in the translator is imprecise. In particular, this will disallow redefining typedefs
			// with arrays whose dimension is an enumerator or a cast of a constant/enumerator. The effort required
			// to fix this corner case likely outweighs the utility of allowing it.
			if ( isVariableLength( t1 ) || isVariableLength( t2 ) ) {
				SemanticError( tyDecl->location, "Cannot redefine typedef: " + tyDecl->name );
			}
		} else {
			typedefNames[ tyDecl->name ] = std::make_pair( TypedefDeclPtr( tyDecl ), scopeLevel );
		} // if

		// When a typedef is a forward declaration:
		//    typedef struct screen SCREEN;
		// the declaration portion must be retained:
		//    struct screen;
		// because the expansion of the typedef is:
		//    void rtn( SCREEN * p ) => void rtn( struct screen * p )
		// hence the type-name "screen" must be defined.
		// Note, qualifiers on the typedef are superfluous for the forward declaration.

		Type * designatorType = tyDecl->base->stripDeclarator();
		if ( StructInstType * aggDecl = dynamic_cast< StructInstType * >( designatorType ) ) {
			declsToAddBefore.push_back( new StructDecl( aggDecl->name, AggregateDecl::Struct, noAttributes, tyDecl->linkage ) );
		} else if ( UnionInstType * aggDecl = dynamic_cast< UnionInstType * >( designatorType ) ) {
			declsToAddBefore.push_back( new UnionDecl( aggDecl->name, noAttributes, tyDecl->linkage ) );
		} else if ( EnumInstType * enumDecl = dynamic_cast< EnumInstType * >( designatorType ) ) {
			// declsToAddBefore.push_back( new EnumDecl( enumDecl->name, noAttributes, tyDecl->linkage, enumDecl->baseEnum->base ) );
			if (enumDecl->baseEnum) {
				declsToAddBefore.push_back( new EnumDecl( enumDecl->name, noAttributes, tyDecl->linkage, enumDecl->baseEnum->base ) );
			} else {
				declsToAddBefore.push_back( new EnumDecl( enumDecl->name, noAttributes, tyDecl->linkage ) );
			}
		} // if
		return tyDecl->clone();
	}

	void ReplaceTypedef::premutate( TypeDecl * typeDecl ) {
		TypedefMap::iterator i = typedefNames.find( typeDecl->name );
		if ( i != typedefNames.end() ) {
			typedefNames.erase( i ) ;
		} // if

		typedeclNames.insert( typeDecl->name, typeDecl );
	}

	void ReplaceTypedef::premutate( FunctionDecl * ) {
		GuardScope( typedefNames );
		GuardScope( typedeclNames );
	}

	void ReplaceTypedef::premutate( ObjectDecl * ) {
		GuardScope( typedefNames );
		GuardScope( typedeclNames );
	}

	DeclarationWithType * ReplaceTypedef::postmutate( ObjectDecl * objDecl ) {
		if ( FunctionType * funtype = dynamic_cast<FunctionType *>( objDecl->type ) ) { // function type?
			// replace the current object declaration with a function declaration
			FunctionDecl * newDecl = new FunctionDecl( objDecl->name, objDecl->get_storageClasses(), objDecl->linkage, funtype, 0, objDecl->attributes, objDecl->get_funcSpec() );
			objDecl->attributes.clear();
			objDecl->set_type( nullptr );
			delete objDecl;
			return newDecl;
		} // if
		return objDecl;
	}

	void ReplaceTypedef::premutate( CastExpr * ) {
		GuardScope( typedefNames );
		GuardScope( typedeclNames );
	}

	void ReplaceTypedef::premutate( CompoundStmt * ) {
		GuardScope( typedefNames );
		GuardScope( typedeclNames );
		scopeLevel += 1;
		GuardAction( [this](){ scopeLevel -= 1; } );
	}

	template<typename AggDecl>
	void ReplaceTypedef::addImplicitTypedef( AggDecl * aggDecl ) {
		if ( typedefNames.count( aggDecl->get_name() ) == 0 ) {
			Type * type = nullptr;
			if ( StructDecl * newDeclStructDecl = dynamic_cast< StructDecl * >( aggDecl ) ) {
				type = new StructInstType( Type::Qualifiers(), newDeclStructDecl->get_name() );
			} else if ( UnionDecl * newDeclUnionDecl = dynamic_cast< UnionDecl * >( aggDecl ) ) {
				type = new UnionInstType( Type::Qualifiers(), newDeclUnionDecl->get_name() );
			} else if ( EnumDecl * newDeclEnumDecl = dynamic_cast< EnumDecl * >( aggDecl )  ) {
				type = new EnumInstType( Type::Qualifiers(), newDeclEnumDecl->get_name() );
			} // if
			TypedefDeclPtr tyDecl( new TypedefDecl( aggDecl->get_name(), aggDecl->location, Type::StorageClasses(), type, aggDecl->get_linkage() ) );
			typedefNames[ aggDecl->get_name() ] = std::make_pair( std::move( tyDecl ), scopeLevel );
			// add the implicit typedef to the AST
			declsToAddBefore.push_back( new TypedefDecl( aggDecl->get_name(), aggDecl->location, Type::StorageClasses(), type->clone(), aggDecl->get_linkage() ) );
		} // if
	}

	template< typename AggDecl >
	void ReplaceTypedef::handleAggregate( AggDecl * aggr ) {
		SemanticErrorException errors;

		ValueGuard< std::list<Declaration * > > oldBeforeDecls( declsToAddBefore );
		ValueGuard< std::list<Declaration * > > oldAfterDecls ( declsToAddAfter  );
		declsToAddBefore.clear();
		declsToAddAfter.clear();

		GuardScope( typedefNames );
		GuardScope( typedeclNames );
		mutateAll( aggr->parameters, * visitor );
		mutateAll( aggr->attributes, * visitor );

		// unroll mutateAll for aggr->members so that implicit typedefs for nested types are added to the aggregate body.
		for ( std::list< Declaration * >::iterator i = aggr->members.begin(); i != aggr->members.end(); ++i ) {
			if ( !declsToAddAfter.empty() ) { aggr->members.splice( i, declsToAddAfter ); }

			try {
				* i = maybeMutate( * i, * visitor );
			} catch ( SemanticErrorException &e ) {
				errors.append( e );
			}

			if ( !declsToAddBefore.empty() ) { aggr->members.splice( i, declsToAddBefore ); }
		}

		if ( !declsToAddAfter.empty() ) { aggr->members.splice( aggr->members.end(), declsToAddAfter ); }
		if ( !errors.isEmpty() ) { throw errors; }
	}

	void ReplaceTypedef::premutate( StructDecl * structDecl ) {
		visit_children = false;
		addImplicitTypedef( structDecl );
		handleAggregate( structDecl );
	}

	void ReplaceTypedef::premutate( UnionDecl * unionDecl ) {
		visit_children = false;
		addImplicitTypedef( unionDecl );
		handleAggregate( unionDecl );
	}

	void ReplaceTypedef::premutate( EnumDecl * enumDecl ) {
		addImplicitTypedef( enumDecl );
	}

	void ReplaceTypedef::premutate( FunctionType * ) {
		GuardValue( inFunctionType );
		inFunctionType = true;
	}

	void ReplaceTypedef::premutate( TraitDecl * ) {
		GuardScope( typedefNames );
		GuardScope( typedeclNames);
	}

	void VerifyCtorDtorAssign::verify( std::list< Declaration * > & translationUnit ) {
		PassVisitor<VerifyCtorDtorAssign> verifier;
		acceptAll( translationUnit, verifier );
	}

	void VerifyCtorDtorAssign::previsit( FunctionDecl * funcDecl ) {
		FunctionType * funcType = funcDecl->get_functionType();
		std::list< DeclarationWithType * > &returnVals = funcType->get_returnVals();
		std::list< DeclarationWithType * > &params = funcType->get_parameters();

		if ( CodeGen::isCtorDtorAssign( funcDecl->get_name() ) ) { // TODO: also check /=, etc.
			if ( params.size() == 0 ) {
				SemanticError( funcDecl->location, "Constructors, destructors, and assignment functions require at least one parameter." );
			}
			ReferenceType * refType = dynamic_cast< ReferenceType * >( params.front()->get_type() );
			if ( ! refType ) {
				SemanticError( funcDecl->location, "First parameter of a constructor, destructor, or assignment function must be a reference." );
			}
			if ( CodeGen::isCtorDtor( funcDecl->get_name() ) && returnVals.size() != 0 ) {
				if(!returnVals.front()->get_type()->isVoid()) {
					SemanticError( funcDecl->location, "Constructors and destructors cannot have explicit return values." );
				}
			}
		}
	}

	// Test for special name on a generic parameter.  Special treatment for the
	// special name is a bootstrapping hack.  In most cases, the worlds of T's
	// and of N's don't overlap (normal treamtemt).  The foundations in
	// array.hfa use tagging for both types and dimensions.  Tagging treats
	// its subject parameter even more opaquely than T&, which assumes it is
	// possible to have a pointer/reference to such an object.  Tagging only
	// seeks to identify the type-system resident at compile time.  Both N's
	// and T's can make tags.  The tag definition uses the special name, which
	// is treated as "an N or a T."  This feature is not inteded to be used
	// outside of the definition and immediate uses of a tag.
	static inline bool isReservedTysysIdOnlyName( const std::string & name ) {
		// name's prefix was __CFA_tysys_id_only, before it got wrapped in __..._generic
		int foundAt = name.find("__CFA_tysys_id_only");
		if (foundAt == 0) return true;
		if (foundAt == 2 && name[0] == '_' && name[1] == '_') return true;
		return false;
	}

	template< typename Aggr >
	void validateGeneric( Aggr * inst ) {
		std::list< TypeDecl * > * params = inst->get_baseParameters();
		if ( params ) {
			std::list< Expression * > & args = inst->get_parameters();

			// insert defaults arguments when a type argument is missing (currently only supports missing arguments at the end of the list).
			// A substitution is used to ensure that defaults are replaced correctly, e.g.,
			//   forall(otype T, otype alloc = heap_allocator(T)) struct vector;
			//   vector(int) v;
			// after insertion of default values becomes
			//   vector(int, heap_allocator(T))
			// and the substitution is built with T=int so that after substitution, the result is
			//   vector(int, heap_allocator(int))
			TypeSubstitution sub;
			auto paramIter = params->begin();
			auto argIter = args.begin();
			for ( ; paramIter != params->end(); ++paramIter, ++argIter ) {
				if ( argIter != args.end() ) {
					TypeExpr * expr = dynamic_cast< TypeExpr * >( * argIter );
					if ( expr ) {
						sub.add( (* paramIter)->get_name(), expr->get_type()->clone() );
					}
				} else {
					Type * defaultType = (* paramIter)->get_init();
					if ( defaultType ) {
						args.push_back( new TypeExpr( defaultType->clone() ) );
						sub.add( (* paramIter)->get_name(), defaultType->clone() );
						argIter = std::prev(args.end());
					} else {
						SemanticError( inst, "Too few type arguments in generic type " );
					}
				}
				assert( argIter != args.end() );
				bool typeParamDeclared = (*paramIter)->kind != TypeDecl::Kind::Dimension;
				bool typeArgGiven;
				if ( isReservedTysysIdOnlyName( (*paramIter)->name ) ) {
					// coerce a match when declaration is reserved name, which means "either"
					typeArgGiven = typeParamDeclared;
				} else {
					typeArgGiven = dynamic_cast< TypeExpr * >( * argIter );
				}
				if ( ! typeParamDeclared &&   typeArgGiven ) SemanticError( inst, "Type argument given for value parameter: " );
				if (   typeParamDeclared && ! typeArgGiven ) SemanticError( inst, "Expression argument given for type parameter: " );
			}

			sub.apply( inst );
			if ( args.size() > params->size() ) SemanticError( inst, "Too many type arguments in generic type " );
		}
	}

	void ValidateGenericParameters::previsit( StructInstType * inst ) {
		validateGeneric( inst );
	}

	void ValidateGenericParameters::previsit( UnionInstType * inst ) {
		validateGeneric( inst );
	}

	void TranslateDimensionGenericParameters::translateDimensions( std::list< Declaration * > &translationUnit ) {
		PassVisitor<TranslateDimensionGenericParameters> translator;
		mutateAll( translationUnit, translator );
	}

	TranslateDimensionGenericParameters::TranslateDimensionGenericParameters() : WithIndexer( false ) {}

	// Declaration of type variable:           forall( [N] )          ->  forall( N & | sized( N ) )
	TypeDecl * TranslateDimensionGenericParameters::postmutate( TypeDecl * td ) {
		if ( td->kind == TypeDecl::Dimension ) {
			td->kind = TypeDecl::Dtype;
			if ( ! isReservedTysysIdOnlyName( td->name ) ) {
				td->sized = true;
			}
		}
		return td;
	}

	// Situational awareness:
	// array( float, [[currentExpr]]     )  has  visitingChildOfSUIT == true
	// array( float, [[currentExpr]] - 1 )  has  visitingChildOfSUIT == false
	// size_t x =    [[currentExpr]]        has  visitingChildOfSUIT == false
	void TranslateDimensionGenericParameters::changeState_ChildOfSUIT( bool newVal ) {
		GuardValue( nextVisitedNodeIsChildOfSUIT );
		GuardValue( visitingChildOfSUIT );
		visitingChildOfSUIT = nextVisitedNodeIsChildOfSUIT;
		nextVisitedNodeIsChildOfSUIT = newVal;
	}
	void TranslateDimensionGenericParameters::premutate( StructInstType * sit ) {
		(void) sit;
		changeState_ChildOfSUIT(true);
	}
	void TranslateDimensionGenericParameters::premutate( UnionInstType * uit ) {
		(void) uit;
		changeState_ChildOfSUIT(true);
	}
	void TranslateDimensionGenericParameters::premutate( BaseSyntaxNode * node ) {
		(void) node;
		changeState_ChildOfSUIT(false);
	}

	// Passing values as dimension arguments:  array( float,     7 )  -> array( float, char[             7 ] )
	// Consuming dimension parameters:         size_t x =    N - 1 ;  -> size_t x =          sizeof(N) - 1   ;
	// Intertwined reality:                    array( float, N     )  -> array( float,              N        )
	//                                         array( float, N - 1 )  -> array( float, char[ sizeof(N) - 1 ] )
	// Intertwined case 1 is not just an optimization.
	// Avoiding char[sizeof(-)] is necessary to enable the call of f to bind the value of N, in:
	//   forall([N]) void f( array(float, N) & );
	//   array(float, 7) a;
	//   f(a);

	Expression * TranslateDimensionGenericParameters::postmutate( DimensionExpr * de ) {
		// Expression de is an occurrence of N in LHS of above examples.
		// Look up the name that de references.
		// If we are in a struct body, then this reference can be to an entry of the stuct's forall list.
		// Whether or not we are in a struct body, this reference can be to an entry of a containing function's forall list.
		// If we are in a struct body, then the stuct's forall declarations are innermost (functions don't occur in structs).
		// Thus, a potential struct's declaration is highest priority.
		// A struct's forall declarations are already renamed with _generic_ suffix.  Try that name variant first.

		std::string useName = "__" + de->name + "_generic_";
		TypeDecl * namedParamDecl = const_cast<TypeDecl *>( strict_dynamic_cast<const TypeDecl *, nullptr >( indexer.lookupType( useName ) ) );

		if ( ! namedParamDecl ) {
			useName = de->name;
			namedParamDecl = const_cast<TypeDecl *>( strict_dynamic_cast<const TypeDecl *, nullptr >( indexer.lookupType( useName ) ) );
		}

		// Expect to find it always.  A misspelled name would have been parsed as an identifier.
		assert( namedParamDecl && "Type-system-managed value name not found in symbol table" );

		delete de;

		TypeInstType * refToDecl = new TypeInstType( 0, useName, namedParamDecl );

		if ( visitingChildOfSUIT ) {
			// As in postmutate( Expression * ), topmost expression needs a TypeExpr wrapper
			// But avoid ArrayType-Sizeof
			return new TypeExpr( refToDecl );
		} else {
			// the N occurrence is being used directly as a runtime value,
			// if we are in a type instantiation, then the N is within a bigger value computation
			return new SizeofExpr( refToDecl );
		}
	}

	Expression * TranslateDimensionGenericParameters::postmutate( Expression * e ) {
		if ( visitingChildOfSUIT ) {
			// e is an expression used as an argument to instantiate a type
			if (! dynamic_cast< TypeExpr * >( e ) ) {
				// e is a value expression
				// but not a DimensionExpr, which has a distinct postmutate
				Type * typeExprContent = new ArrayType( 0, new BasicType( 0, BasicType::Char ), e, true, false );
				TypeExpr * result = new TypeExpr( typeExprContent );
				return result;
			}
		}
		return e;
	}

	void CompoundLiteral::premutate( ObjectDecl * objectDecl ) {
		storageClasses = objectDecl->get_storageClasses();
	}

	Expression * CompoundLiteral::postmutate( CompoundLiteralExpr * compLitExpr ) {
		// transform [storage_class] ... (struct S){ 3, ... };
		// into [storage_class] struct S temp =  { 3, ... };
		static UniqueName indexName( "_compLit" );

		ObjectDecl * tempvar = new ObjectDecl( indexName.newName(), storageClasses, LinkageSpec::C, nullptr, compLitExpr->get_result(), compLitExpr->get_initializer() );
		compLitExpr->set_result( nullptr );
		compLitExpr->set_initializer( nullptr );
		delete compLitExpr;
		declsToAddBefore.push_back( tempvar );					// add modified temporary to current block
		return new VariableExpr( tempvar );
	}

	void ReturnTypeFixer::fix( std::list< Declaration * > &translationUnit ) {
		PassVisitor<ReturnTypeFixer> fixer;
		acceptAll( translationUnit, fixer );
	}

	void ReturnTypeFixer::postvisit( FunctionDecl * functionDecl ) {
		FunctionType * ftype = functionDecl->get_functionType();
		std::list< DeclarationWithType * > & retVals = ftype->get_returnVals();
		assertf( retVals.size() == 0 || retVals.size() == 1, "Function %s has too many return values: %zu", functionDecl->get_name().c_str(), retVals.size() );
		if ( retVals.size() == 1 ) {
			// ensure all function return values have a name - use the name of the function to disambiguate (this also provides a nice bit of help for debugging).
			// ensure other return values have a name.
			DeclarationWithType * ret = retVals.front();
			if ( ret->get_name() == "" ) {
				ret->set_name( toString( "_retval_", CodeGen::genName( functionDecl ) ) );
			}
			ret->get_attributes().push_back( new Attribute( "unused" ) );
		}
	}

	void ReturnTypeFixer::postvisit( FunctionType * ftype ) {
		// xxx - need to handle named return values - this information needs to be saved somehow
		// so that resolution has access to the names.
		// Note that this pass needs to happen early so that other passes which look for tuple types
		// find them in all of the right places, including function return types.
		std::list< DeclarationWithType * > & retVals = ftype->get_returnVals();
		if ( retVals.size() > 1 ) {
			// generate a single return parameter which is the tuple of all of the return values
			TupleType * tupleType = strict_dynamic_cast< TupleType * >( ResolvExpr::extractResultType( ftype ) );
			// ensure return value is not destructed by explicitly creating an empty ListInit node wherein maybeConstruct is false.
			ObjectDecl * newRet = new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, 0, tupleType, new ListInit( std::list<Initializer *>(), noDesignators, false ) );
			deleteAll( retVals );
			retVals.clear();
			retVals.push_back( newRet );
		}
	}

	void FixObjectType::fix( std::list< Declaration * > & translationUnit ) {
		PassVisitor<FixObjectType> fixer;
		acceptAll( translationUnit, fixer );
	}

	void FixObjectType::previsit( ObjectDecl * objDecl ) {
		Type * new_type = ResolvExpr::resolveTypeof( objDecl->get_type(), indexer );
		objDecl->set_type( new_type );
	}

	void FixObjectType::previsit( FunctionDecl * funcDecl ) {
		Type * new_type = ResolvExpr::resolveTypeof( funcDecl->type, indexer );
		funcDecl->set_type( new_type );
	}

	void FixObjectType::previsit( TypeDecl * typeDecl ) {
		if ( typeDecl->get_base() ) {
			Type * new_type = ResolvExpr::resolveTypeof( typeDecl->get_base(), indexer );
			typeDecl->set_base( new_type );
		} // if
	}

	void InitializerLength::computeLength( std::list< Declaration * > & translationUnit ) {
		PassVisitor<InitializerLength> len;
		acceptAll( translationUnit, len );
	}

	void ArrayLength::computeLength( std::list< Declaration * > & translationUnit ) {
		PassVisitor<ArrayLength> len;
		acceptAll( translationUnit, len );
	}

	void InitializerLength::previsit( ObjectDecl * objDecl ) {
		if ( ArrayType * at = dynamic_cast< ArrayType * >( objDecl->type ) ) {
			if ( at->dimension ) return;
			if ( ListInit * init = dynamic_cast< ListInit * >( objDecl->init ) ) {
				at->dimension = new ConstantExpr( Constant::from_ulong( init->initializers.size() ) );
			}
		}
	}

	void ArrayLength::previsit( ArrayType * type ) {
		if ( type->dimension ) {
			// need to resolve array dimensions early so that constructor code can correctly determine
			// if a type is a VLA (and hence whether its elements need to be constructed)
			ResolvExpr::findSingleExpression( type->dimension, Validate::SizeType->clone(), indexer );

			// must re-evaluate whether a type is a VLA, now that more information is available
			// (e.g. the dimension may have been an enumerator, which was unknown prior to this step)
			type->isVarLen = ! InitTweak::isConstExpr( type->dimension );
		}
	}

	struct LabelFinder {
		std::set< Label > & labels;
		LabelFinder( std::set< Label > & labels ) : labels( labels ) {}
		void previsit( Statement * stmt ) {
			for ( Label & l : stmt->labels ) {
				labels.insert( l );
			}
		}
	};

	void LabelAddressFixer::premutate( FunctionDecl * funcDecl ) {
		GuardValue( labels );
		PassVisitor<LabelFinder> finder( labels );
		funcDecl->accept( finder );
	}

	Expression * LabelAddressFixer::postmutate( AddressExpr * addrExpr ) {
		// convert &&label into label address
		if ( AddressExpr * inner = dynamic_cast< AddressExpr * >( addrExpr->arg ) ) {
			if ( NameExpr * nameExpr = dynamic_cast< NameExpr * >( inner->arg ) ) {
				if ( labels.count( nameExpr->name ) ) {
					Label name = nameExpr->name;
					delete addrExpr;
					return new LabelAddressExpr( name );
				}
			}
		}
		return addrExpr;
	}

namespace {
	/// Replaces enum types by int, and function/array types in function parameter and return
	/// lists by appropriate pointers
	/*
	struct EnumAndPointerDecay_new {
		const ast::EnumDecl * previsit( const ast::EnumDecl * enumDecl ) {
			// set the type of each member of the enumeration to be EnumConstant
			for ( unsigned i = 0; i < enumDecl->members.size(); ++i ) {
				// build new version of object with EnumConstant
				ast::ptr< ast::ObjectDecl > obj =
					enumDecl->members[i].strict_as< ast::ObjectDecl >();
				obj.get_and_mutate()->type =
					new ast::EnumInstType{ enumDecl->name, ast::CV::Const };

				// set into decl
				ast::EnumDecl * mut = mutate( enumDecl );
				mut->members[i] = obj.get();
				enumDecl = mut;
			}
			return enumDecl;
		}

		static const ast::FunctionType * fixFunctionList(
			const ast::FunctionType * func,
			std::vector< ast::ptr< ast::DeclWithType > > ast::FunctionType::* field,
			ast::ArgumentFlag isVarArgs = ast::FixedArgs
		) {
			const auto & dwts = func->* field;
			unsigned nvals = dwts.size();
			bool hasVoid = false;
			for ( unsigned i = 0; i < nvals; ++i ) {
				func = ast::mutate_field_index( func, field, i, fixFunction( dwts[i], hasVoid ) );
			}

			// the only case in which "void" is valid is where it is the only one in the list
			if ( hasVoid && ( nvals > 1 || isVarArgs ) ) {
				SemanticError(
					dwts.front()->location, func, "invalid type void in function type" );
			}

			// one void is the only thing in the list, remove it
			if ( hasVoid ) {
				func = ast::mutate_field(
					func, field, std::vector< ast::ptr< ast::DeclWithType > >{} );
			}

			return func;
		}

		const ast::FunctionType * previsit( const ast::FunctionType * func ) {
			func = fixFunctionList( func, &ast::FunctionType::params, func->isVarArgs );
			return fixFunctionList( func, &ast::FunctionType::returns );
		}
	};

	/// expand assertions from a trait instance, performing appropriate type variable substitutions
	void expandAssertions(
		const ast::TraitInstType * inst, std::vector< ast::ptr< ast::DeclWithType > > & out
	) {
		assertf( inst->base, "Trait instance not linked to base trait: %s", toCString( inst ) );

		// build list of trait members, substituting trait decl parameters for instance parameters
		ast::TypeSubstitution sub{
			inst->base->params.begin(), inst->base->params.end(), inst->params.begin() };
		// deliberately take ast::ptr by-value to ensure this does not mutate inst->base
		for ( ast::ptr< ast::Decl > decl : inst->base->members ) {
			auto member = decl.strict_as< ast::DeclWithType >();
			sub.apply( member );
			out.emplace_back( member );
		}
	}

	/// Associates forward declarations of aggregates with their definitions
	class LinkReferenceToTypes_new final
	: public ast::WithSymbolTable, public ast::WithGuards, public
	  ast::WithVisitorRef<LinkReferenceToTypes_new>, public ast::WithShortCircuiting {

		// these maps of uses of forward declarations of types need to have the actual type
		// declaration switched in * after * they have been traversed. To enable this in the
		// ast::Pass framework, any node that needs to be so mutated has mutate() called on it
		// before it is placed in the map, properly updating its parents in the usual traversal,
		// then can have the actual mutation applied later
		using ForwardEnumsType = std::unordered_multimap< std::string, ast::EnumInstType * >;
		using ForwardStructsType = std::unordered_multimap< std::string, ast::StructInstType * >;
		using ForwardUnionsType = std::unordered_multimap< std::string, ast::UnionInstType * >;

		const CodeLocation & location;
		const ast::SymbolTable * localSymtab;

		ForwardEnumsType forwardEnums;
		ForwardStructsType forwardStructs;
		ForwardUnionsType forwardUnions;

		/// true if currently in a generic type body, so that type parameter instances can be
		/// renamed appropriately
		bool inGeneric = false;

	public:
		/// contstruct using running symbol table
		LinkReferenceToTypes_new( const CodeLocation & loc )
		: location( loc ), localSymtab( &symtab ) {}

		/// construct using provided symbol table
		LinkReferenceToTypes_new( const CodeLocation & loc, const ast::SymbolTable & syms )
		: location( loc ), localSymtab( &syms ) {}

		const ast::Type * postvisit( const ast::TypeInstType * typeInst ) {
			// ensure generic parameter instances are renamed like the base type
			if ( inGeneric && typeInst->base ) {
				typeInst = ast::mutate_field(
					typeInst, &ast::TypeInstType::name, typeInst->base->name );
			}

			if (
				auto typeDecl = dynamic_cast< const ast::TypeDecl * >(
					localSymtab->lookupType( typeInst->name ) )
			) {
				typeInst = ast::mutate_field( typeInst, &ast::TypeInstType::kind, typeDecl->kind );
			}

			return typeInst;
		}

		const ast::Type * postvisit( const ast::EnumInstType * inst ) {
			const ast::EnumDecl * decl = localSymtab->lookupEnum( inst->name );
			// not a semantic error if the enum is not found, just an implicit forward declaration
			if ( decl ) {
				inst = ast::mutate_field( inst, &ast::EnumInstType::base, decl );
			}
			if ( ! decl || ! decl->body ) {
				// forward declaration
				auto mut = mutate( inst );
				forwardEnums.emplace( inst->name, mut );
				inst = mut;
			}
			return inst;
		}

		void checkGenericParameters( const ast::BaseInstType * inst ) {
			for ( const ast::Expr * param : inst->params ) {
				if ( ! dynamic_cast< const ast::TypeExpr * >( param ) ) {
					SemanticError(
						location, inst, "Expression parameters for generic types are currently "
						"unsupported: " );
				}
			}
		}

		const ast::StructInstType * postvisit( const ast::StructInstType * inst ) {
			const ast::StructDecl * decl = localSymtab->lookupStruct( inst->name );
			// not a semantic error if the struct is not found, just an implicit forward declaration
			if ( decl ) {
				inst = ast::mutate_field( inst, &ast::StructInstType::base, decl );
			}
			if ( ! decl || ! decl->body ) {
				// forward declaration
				auto mut = mutate( inst );
				forwardStructs.emplace( inst->name, mut );
				inst = mut;
			}
			checkGenericParameters( inst );
			return inst;
		}

		const ast::UnionInstType * postvisit( const ast::UnionInstType * inst ) {
			const ast::UnionDecl * decl = localSymtab->lookupUnion( inst->name );
			// not a semantic error if the struct is not found, just an implicit forward declaration
			if ( decl ) {
				inst = ast::mutate_field( inst, &ast::UnionInstType::base, decl );
			}
			if ( ! decl || ! decl->body ) {
				// forward declaration
				auto mut = mutate( inst );
				forwardUnions.emplace( inst->name, mut );
				inst = mut;
			}
			checkGenericParameters( inst );
			return inst;
		}

		const ast::Type * postvisit( const ast::TraitInstType * traitInst ) {
			// handle other traits
			const ast::TraitDecl * traitDecl = localSymtab->lookupTrait( traitInst->name );
			if ( ! traitDecl )	 {
				SemanticError( location, "use of undeclared trait " + traitInst->name );
			}
			if ( traitDecl->params.size() != traitInst->params.size() ) {
				SemanticError( location, traitInst, "incorrect number of trait parameters: " );
			}
			traitInst = ast::mutate_field( traitInst, &ast::TraitInstType::base, traitDecl );

			// need to carry over the "sized" status of each decl in the instance
			for ( unsigned i = 0; i < traitDecl->params.size(); ++i ) {
				auto expr = traitInst->params[i].as< ast::TypeExpr >();
				if ( ! expr ) {
					SemanticError(
						traitInst->params[i].get(), "Expression parameters for trait instances "
						"are currently unsupported: " );
				}

				if ( auto inst = expr->type.as< ast::TypeInstType >() ) {
					if ( traitDecl->params[i]->sized && ! inst->base->sized ) {
						// traitInst = ast::mutate_field_index(
						// 	traitInst, &ast::TraitInstType::params, i,
						// 	...
						// );
						ast::TraitInstType * mut = ast::mutate( traitInst );
						ast::chain_mutate( mut->params[i] )
							( &ast::TypeExpr::type )
								( &ast::TypeInstType::base )->sized = true;
						traitInst = mut;
					}
				}
			}

			return traitInst;
		}

		void previsit( const ast::QualifiedType * ) { visit_children = false; }

		const ast::Type * postvisit( const ast::QualifiedType * qualType ) {
			// linking only makes sense for the "oldest ancestor" of the qualified type
			return ast::mutate_field(
				qualType, &ast::QualifiedType::parent, qualType->parent->accept( * visitor ) );
		}

		const ast::Decl * postvisit( const ast::EnumDecl * enumDecl ) {
			// visit enum members first so that the types of self-referencing members are updated
			// properly
			if ( ! enumDecl->body ) return enumDecl;

			// update forward declarations to point here
			auto fwds = forwardEnums.equal_range( enumDecl->name );
			if ( fwds.first != fwds.second ) {
				auto inst = fwds.first;
				do {
					// forward decl is stored * mutably * in map, can thus be updated
					inst->second->base = enumDecl;
				} while ( ++inst != fwds.second );
				forwardEnums.erase( fwds.first, fwds.second );
			}

			// ensure that enumerator initializers are properly set
			for ( unsigned i = 0; i < enumDecl->members.size(); ++i ) {
				auto field = enumDecl->members[i].strict_as< ast::ObjectDecl >();
				if ( field->init ) {
					// need to resolve enumerator initializers early so that other passes that
					// determine if an expression is constexpr have appropriate information
					auto init = field->init.strict_as< ast::SingleInit >();

					enumDecl = ast::mutate_field_index(
						enumDecl, &ast::EnumDecl::members, i,
						ast::mutate_field( field, &ast::ObjectDecl::init,
							ast::mutate_field( init, &ast::SingleInit::value,
								ResolvExpr::findSingleExpression(
									init->value, new ast::BasicType{ ast::BasicType::SignedInt },
									symtab ) ) ) );
				}
			}

			return enumDecl;
		}

		/// rename generic type parameters uniquely so that they do not conflict with user defined
		/// function forall parameters, e.g. the T in Box and the T in f, below
		///   forall(otype T)
		///   struct Box {
		///     T x;
		///   };
		///   forall(otype T)
		///   void f(Box(T) b) {
		///     ...
		///   }
		template< typename AggrDecl >
		const AggrDecl * renameGenericParams( const AggrDecl * aggr ) {
			GuardValue( inGeneric );
			inGeneric = ! aggr->params.empty();

			for ( unsigned i = 0; i < aggr->params.size(); ++i ) {
				const ast::TypeDecl * td = aggr->params[i];

				aggr = ast::mutate_field_index(
					aggr, &AggrDecl::params, i,
					ast::mutate_field( td, &ast::TypeDecl::name, "__" + td->name + "_generic_" ) );
			}
			return aggr;
		}

		const ast::StructDecl * previsit( const ast::StructDecl * structDecl ) {
			return renameGenericParams( structDecl );
		}

		void postvisit( const ast::StructDecl * structDecl ) {
			// visit struct members first so that the types of self-referencing members are
			// updated properly
			if ( ! structDecl->body ) return;

			// update forward declarations to point here
			auto fwds = forwardStructs.equal_range( structDecl->name );
			if ( fwds.first != fwds.second ) {
				auto inst = fwds.first;
				do {
					// forward decl is stored * mutably * in map, can thus be updated
					inst->second->base = structDecl;
				} while ( ++inst != fwds.second );
				forwardStructs.erase( fwds.first, fwds.second );
			}
		}

		const ast::UnionDecl * previsit( const ast::UnionDecl * unionDecl ) {
			return renameGenericParams( unionDecl );
		}

		void postvisit( const ast::UnionDecl * unionDecl ) {
			// visit union members first so that the types of self-referencing members are updated
			// properly
			if ( ! unionDecl->body ) return;

			// update forward declarations to point here
			auto fwds = forwardUnions.equal_range( unionDecl->name );
			if ( fwds.first != fwds.second ) {
				auto inst = fwds.first;
				do {
					// forward decl is stored * mutably * in map, can thus be updated
					inst->second->base = unionDecl;
				} while ( ++inst != fwds.second );
				forwardUnions.erase( fwds.first, fwds.second );
			}
		}

		const ast::Decl * postvisit( const ast::TraitDecl * traitDecl ) {
			// set the "sized" status for the special "sized" trait
			if ( traitDecl->name == "sized" ) {
				assertf( traitDecl->params.size() == 1, "Built-in trait 'sized' has incorrect "
					"number of parameters: %zd", traitDecl->params.size() );

				traitDecl = ast::mutate_field_index(
					traitDecl, &ast::TraitDecl::params, 0,
					ast::mutate_field(
						traitDecl->params.front().get(), &ast::TypeDecl::sized, true ) );
			}

			// move assertions from type parameters into the body of the trait
			std::vector< ast::ptr< ast::DeclWithType > > added;
			for ( const ast::TypeDecl * td : traitDecl->params ) {
				for ( const ast::DeclWithType * assn : td->assertions ) {
					auto inst = dynamic_cast< const ast::TraitInstType * >( assn->get_type() );
					if ( inst ) {
						expandAssertions( inst, added );
					} else {
						added.emplace_back( assn );
					}
				}
			}
			if ( ! added.empty() ) {
				auto mut = mutate( traitDecl );
				for ( const ast::DeclWithType * decl : added ) {
					mut->members.emplace_back( decl );
				}
				traitDecl = mut;
			}

			return traitDecl;
		}
	};

	/// Replaces array and function types in forall lists by appropriate pointer type and assigns
	/// each object and function declaration a unique ID
	class ForallPointerDecay_new {
		const CodeLocation & location;
	public:
		ForallPointerDecay_new( const CodeLocation & loc ) : location( loc ) {}

		const ast::ObjectDecl * previsit( const ast::ObjectDecl * obj ) {
			// ensure that operator names only apply to functions or function pointers
			if (
				CodeGen::isOperator( obj->name )
				&& ! dynamic_cast< const ast::FunctionType * >( obj->type->stripDeclarator() )
			) {
				SemanticError( obj->location, toCString( "operator ", obj->name.c_str(), " is not "
					"a function or function pointer." )  );
			}

			// ensure object has unique ID
			if ( obj->uniqueId ) return obj;
			auto mut = mutate( obj );
			mut->fixUniqueId();
			return mut;
		}

		const ast::FunctionDecl * previsit( const ast::FunctionDecl * func ) {
			// ensure function has unique ID
			if ( func->uniqueId ) return func;
			auto mut = mutate( func );
			mut->fixUniqueId();
			return mut;
		}

		/// Fix up assertions -- flattens assertion lists, removing all trait instances
		template< typename node_t, typename parent_t >
		static const node_t * forallFixer(
			const CodeLocation & loc, const node_t * node,
			ast::FunctionType::ForallList parent_t::* forallField
		) {
			for ( unsigned i = 0; i < (node->* forallField).size(); ++i ) {
				const ast::TypeDecl * type = (node->* forallField)[i];
				if ( type->assertions.empty() ) continue;

				std::vector< ast::ptr< ast::DeclWithType > > asserts;
				asserts.reserve( type->assertions.size() );

				// expand trait instances into their members
				for ( const ast::DeclWithType * assn : type->assertions ) {
					auto traitInst =
						dynamic_cast< const ast::TraitInstType * >( assn->get_type() );
					if ( traitInst ) {
						// expand trait instance to all its members
						expandAssertions( traitInst, asserts );
					} else {
						// pass other assertions through
						asserts.emplace_back( assn );
					}
				}

				// apply FixFunction to every assertion to check for invalid void type
				for ( ast::ptr< ast::DeclWithType > & assn : asserts ) {
					bool isVoid = false;
					assn = fixFunction( assn, isVoid );
					if ( isVoid ) {
						SemanticError( loc, node, "invalid type void in assertion of function " );
					}
				}

				// place mutated assertion list in node
				auto mut = mutate( type );
				mut->assertions = move( asserts );
				node = ast::mutate_field_index( node, forallField, i, mut );
			}
			return node;
		}

		const ast::FunctionType * previsit( const ast::FunctionType * ftype ) {
			return forallFixer( location, ftype, &ast::FunctionType::forall );
		}

		const ast::StructDecl * previsit( const ast::StructDecl * aggrDecl ) {
			return forallFixer( aggrDecl->location, aggrDecl, &ast::StructDecl::params );
		}

		const ast::UnionDecl * previsit( const ast::UnionDecl * aggrDecl ) {
			return forallFixer( aggrDecl->location, aggrDecl, &ast::UnionDecl::params );
		}
	};
	*/
} // anonymous namespace

/*
const ast::Type * validateType(
		const CodeLocation & loc, const ast::Type * type, const ast::SymbolTable & symtab ) {
	// ast::Pass< EnumAndPointerDecay_new > epc;
	ast::Pass< LinkReferenceToTypes_new > lrt{ loc, symtab };
	ast::Pass< ForallPointerDecay_new > fpd{ loc };

	return type->accept( lrt )->accept( fpd );
}
*/

} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
