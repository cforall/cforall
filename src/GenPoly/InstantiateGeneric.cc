//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// InstantiateGeneric.cc --
//
// Author           : Aaron B. Moss
// Created On       : Thu Aug 04 18:33:00 2016
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jul 16 10:17:00 2020
// Update Count     : 2
//
#include "InstantiateGeneric.h"

#include <cassert>                     // for assertf, assert
#include <iterator>                    // for back_inserter, inserter
#include <list>                        // for list, _List_const_iterator
#include <utility>                     // for move, pair
#include <vector>                      // for vector

#include "CodeGen/OperatorTable.h"
#include "Common/PassVisitor.h"        // for PassVisitor, WithDeclsToAdd
#include "Common/ScopedMap.h"          // for ScopedMap
#include "Common/UniqueName.h"         // for UniqueName
#include "Common/utility.h"            // for deleteAll, cloneAll
#include "GenPoly.h"                   // for isPolyType, typesPolyCompatible
#include "InitTweak/InitTweak.h"
#include "ResolvExpr/typeops.h"        // for adjustExprType
#include "ResolvExpr/Unify.h"          // for typesCompatible
#include "ScopedSet.h"                 // for ScopedSet, ScopedSet<>::iterator
#include "ScrubTyVars.h"               // for ScrubTyVars
#include "SynTree/Declaration.h"       // for StructDecl, UnionDecl, TypeDecl
#include "SynTree/Expression.h"        // for TypeExpr, Expression
#include "SynTree/Mutator.h"           // for mutateAll
#include "SynTree/Type.h"              // for StructInstType, UnionInstType
#include "SynTree/TypeSubstitution.h"  // for TypeSubstitution
#include "SynTree/Visitor.h"           // for acceptAll


namespace GenPoly {

	/// Abstracts type equality for a list of parameter types
	struct TypeList {
		TypeList() : params() {}
		TypeList( const std::list< Type* > &_params ) : params() { cloneAll(_params, params); }
		TypeList( std::list< Type* > &&_params ) : params( _params ) {}

		TypeList( const TypeList &that ) : params() { cloneAll(that.params, params); }
		TypeList( TypeList &&that ) : params( std::move( that.params ) ) {}

		/// Extracts types from a list of TypeExpr*
		TypeList( const std::list< TypeExpr* >& _params ) : params() {
			for ( std::list< TypeExpr* >::const_iterator param = _params.begin(); param != _params.end(); ++param ) {
				params.push_back( (*param)->get_type()->clone() );
			}
		}

		TypeList& operator= ( const TypeList &that ) {
			deleteAll( params );

			params.clear();
			cloneAll( that.params, params );

			return *this;
		}

		TypeList& operator= ( TypeList &&that ) {
			deleteAll( params );

			params = std::move( that.params );

			return *this;
		}

		~TypeList() { deleteAll( params ); }

		bool operator== ( const TypeList& that ) const {
			if ( params.size() != that.params.size() ) return false;

			for ( std::list< Type* >::const_iterator it = params.begin(), jt = that.params.begin(); it != params.end(); ++it, ++jt ) {
				if ( ! typesPolyCompatible( *it, *jt ) ) return false;
			}
			return true;
		}

		std::list< Type* > params;  ///< Instantiation parameters
	};

	/// Maps a key and a TypeList to the some value, accounting for scope
	template< typename Key, typename Value >
	class InstantiationMap {
		/// Wraps value for a specific (Key, TypeList) combination
		typedef std::pair< TypeList, Value* > Instantiation;
		/// List of TypeLists paired with their appropriate values
		typedef std::vector< Instantiation > ValueList;
		/// Underlying map type; maps keys to a linear list of corresponding TypeLists and values
		typedef ScopedMap< Key*, ValueList > InnerMap;

		InnerMap instantiations;  ///< instantiations

	public:
		/// Starts a new scope
		void beginScope() { instantiations.beginScope(); }

		/// Ends a scope
		void endScope() { instantiations.endScope(); }

		/// Gets the value for the (key, typeList) pair, returns NULL on none such.
		Value *lookup( Key *key, const std::list< TypeExpr* >& params ) const {
			TypeList typeList( params );

			// scan scopes for matches to the key
			for ( typename InnerMap::const_iterator insts = instantiations.find( key ); insts != instantiations.end(); insts = instantiations.findNext( insts, key ) ) {
				for ( typename ValueList::const_reverse_iterator inst = insts->second.rbegin(); inst != insts->second.rend(); ++inst ) {
					if ( inst->first == typeList ) return inst->second;
				}
			}
			// no matching instantiations found
			return 0;
		}

		/// Adds a value for a (key, typeList) pair to the current scope
		void insert( Key *key, const std::list< TypeExpr* > &params, Value *value ) {
			auto it = instantiations.findAt( instantiations.currentScope(), key );
			if ( it == instantiations.end() ) {
				instantiations.insert( key, ValueList{ Instantiation{ TypeList( params ), value } } );
			} else {
				it->second.push_back( Instantiation{ TypeList( params ), value } );
			}
		}
	};

	/// Possible options for a given specialization of a generic type
	enum class genericType {
		dtypeStatic,  ///< Concrete instantiation based solely on {d,f}type-to-void conversions
		concrete,     ///< Concrete instantiation requiring at least one parameter type
		dynamic       ///< No concrete instantiation
	};

	genericType& operator |= ( genericType& gt, const genericType& ht ) {
		switch ( gt ) {
		case genericType::dtypeStatic:
			gt = ht;
			break;
		case genericType::concrete:
			if ( ht == genericType::dynamic ) { gt = genericType::dynamic; }
			break;
		case genericType::dynamic:
			// nothing possible
			break;
		}
		return gt;
	}

	/// Add cast to dtype-static member expressions so that type information is not lost in GenericInstantiator
	struct FixDtypeStatic final : public WithGuards, public WithVisitorRef<FixDtypeStatic>, public WithShortCircuiting, public WithStmtsToAdd {
		Expression * postmutate( MemberExpr * memberExpr );

		void premutate( ApplicationExpr * appExpr );
		void premutate( AddressExpr * addrExpr );

		template<typename AggrInst>
		Expression * fixMemberExpr( AggrInst * inst, MemberExpr * memberExpr );

		bool isLvalueArg = false;
	};

	/// Mutator pass that replaces concrete instantiations of generic types with actual struct declarations, scoped appropriately
	struct GenericInstantiator final : public WithConstTypeSubstitution, public WithDeclsToAdd, public WithVisitorRef<GenericInstantiator>, public WithGuards {
		/// Map of (generic type, parameter list) pairs to concrete type instantiations
		InstantiationMap< AggregateDecl, AggregateDecl > instantiations;
		/// Set of types which are dtype-only generic (and therefore have static layout)
		std::set<AggregateDecl *> dtypeStatics;
		/// Namer for concrete types
		UniqueName typeNamer;
		/// Should not make use of type environment to replace types of function parameter and return values.
		bool inFunctionType = false;
		/// Index of current member, used to recreate MemberExprs with the member from an instantiation
		int memberIndex = -1;
		GenericInstantiator() : instantiations(), dtypeStatics(), typeNamer("_conc_") {}

		Type* postmutate( StructInstType *inst );
		Type* postmutate( UnionInstType *inst );

		// fix MemberExprs to use the member from the instantiation
		void premutate( MemberExpr * memberExpr );
		Expression * postmutate( MemberExpr * memberExpr );

		void premutate( FunctionType * ) {
			GuardValue( inFunctionType );
			inFunctionType = true;
		}

		void beginScope();
		void endScope();
	private:
		/// Wrap instantiation lookup for structs
		StructDecl* lookup( StructInstType *inst, const std::list< TypeExpr* > &typeSubs ) { return (StructDecl*)instantiations.lookup( inst->get_baseStruct(), typeSubs ); }
		/// Wrap instantiation lookup for unions
		UnionDecl* lookup( UnionInstType *inst, const std::list< TypeExpr* > &typeSubs ) { return (UnionDecl*)instantiations.lookup( inst->get_baseUnion(), typeSubs ); }
		/// Wrap instantiation insertion for structs
		void insert( StructInstType *inst, const std::list< TypeExpr* > &typeSubs, StructDecl *decl ) { instantiations.insert( inst->get_baseStruct(), typeSubs, decl ); }
		/// Wrap instantiation insertion for unions
		void insert( UnionInstType *inst, const std::list< TypeExpr* > &typeSubs, UnionDecl *decl ) { instantiations.insert( inst->get_baseUnion(), typeSubs, decl ); }

		void replaceParametersWithConcrete( std::list< Expression* >& params );
		Type *replaceWithConcrete( Type *type, bool doClone );

		/// Strips a dtype-static aggregate decl of its type parameters, marks it as stripped
		void stripDtypeParams( AggregateDecl *base, std::list< TypeDecl* >& baseParams, const std::list< TypeExpr* >& typeSubs );
	};

	void instantiateGeneric( std::list< Declaration* > &translationUnit ) {
		PassVisitor<FixDtypeStatic> fixer;
		PassVisitor<GenericInstantiator> instantiator;

		mutateAll( translationUnit, fixer );
		mutateAll( translationUnit, instantiator );
	}

	bool isDtypeStatic( const std::list< TypeDecl* >& baseParams ) {
		return std::all_of( baseParams.begin(), baseParams.end(), []( TypeDecl * td ) { return ! td->isComplete(); } );
	}

	/// Makes substitutions of params into baseParams; returns dtypeStatic if there is a concrete instantiation based only on {d,f}type-to-void conversions,
	/// concrete if there is a concrete instantiation requiring at least one parameter type, and dynamic if there is no concrete instantiation
	genericType makeSubstitutions( const std::list< TypeDecl* >& baseParams, const std::list< Expression* >& params, std::list< TypeExpr* >& out ) {
		genericType gt = genericType::dtypeStatic;

		// substitute concrete types for given parameters, and incomplete types for placeholders
		std::list< TypeDecl* >::const_iterator baseParam = baseParams.begin();
		std::list< Expression* >::const_iterator param = params.begin();
		for ( ; baseParam != baseParams.end() && param != params.end(); ++baseParam, ++param ) {
			TypeExpr *paramType = dynamic_cast< TypeExpr* >( *param );
			assert(paramType && "Aggregate parameters should be type expressions");

			if ( (*baseParam)->isComplete() ) {
				// substitute parameter for complete (otype or sized dtype) type
				if ( isPolyType( paramType->get_type() ) ) {
					// substitute polymorphic parameter type in to generic type
					out.push_back( paramType->clone() );
					gt = genericType::dynamic;
				} else {
					// normalize possibly dtype-static parameter type
					out.push_back( new TypeExpr{
						ScrubTyVars::scrubAll( paramType->get_type()->clone() ) } );
					gt |= genericType::concrete;
				}
			} else switch ( (*baseParam)->get_kind() ) {
				case TypeDecl::Dtype:
					// can pretend that any incomplete dtype is `void`
					out.push_back( new TypeExpr( new VoidType( Type::Qualifiers() ) ) );
					break;
				case TypeDecl::Ftype:
					// can pretend that any ftype is `void (*)(void)`
					out.push_back( new TypeExpr( new FunctionType( Type::Qualifiers(), false ) ) );
					break;
				case TypeDecl::Ttype:
					assertf( false, "Ttype parameters are not currently allowed as parameters to generic types." );
					break;
				default:
					assertf( false, "Unhandled type parameter kind" );
					break;
			}
		}

		assertf( baseParam == baseParams.end() && param == params.end(), "Type parameters should match type variables" );
		return gt;
	}

	/// Substitutes types of members of in according to baseParams => typeSubs, appending the result to out
	void substituteMembers( const std::list< Declaration* >& in, const std::list< TypeDecl* >& baseParams, const std::list< TypeExpr* >& typeSubs,
							std::list< Declaration* >& out ) {
		// substitute types into new members
		TypeSubstitution subs( baseParams.begin(), baseParams.end(), typeSubs.begin() );
		for ( std::list< Declaration* >::const_iterator member = in.begin(); member != in.end(); ++member ) {
			Declaration *newMember = (*member)->clone();
			subs.apply(newMember);
			out.push_back( newMember );
		}
	}

	/// Substitutes types of members according to baseParams => typeSubs, working in-place
	void substituteMembers( std::list< Declaration* >& members, const std::list< TypeDecl* >& baseParams, const std::list< TypeExpr* >& typeSubs ) {
		// substitute types into new members
		TypeSubstitution subs( baseParams.begin(), baseParams.end(), typeSubs.begin() );
		for ( std::list< Declaration* >::iterator member = members.begin(); member != members.end(); ++member ) {
			subs.apply(*member);
		}
	}

	/// Strips the instances's type parameters
	void stripInstParams( ReferenceToType *inst ) {
		deleteAll( inst->get_parameters() );
		inst->get_parameters().clear();
	}

	template< typename AggrInst >
	static AggrInst * asForward( AggrInst * decl ) {
		if ( !decl->body ) {
			return nullptr;
		}
		decl = decl->clone();
		decl->body = false;
		deleteAll( decl->members );
		decl->members.clear();
		return decl;
	}

	void GenericInstantiator::stripDtypeParams( AggregateDecl *base, std::list< TypeDecl* >& baseParams, const std::list< TypeExpr* >& typeSubs ) {
		substituteMembers( base->get_members(), baseParams, typeSubs );

		// xxx - can't delete type parameters because they may have assertions that are used
		// deleteAll( baseParams );
		baseParams.clear();

		dtypeStatics.insert( base );
	}

	/// xxx - more or less copied from box -- these should be merged with those somehow...
	void GenericInstantiator::replaceParametersWithConcrete( std::list< Expression* >& params ) {
		for ( std::list< Expression* >::iterator param = params.begin(); param != params.end(); ++param ) {
			TypeExpr *paramType = dynamic_cast< TypeExpr* >( *param );
			assertf(paramType, "Aggregate parameters should be type expressions");
			paramType->set_type( replaceWithConcrete( paramType->get_type(), false ) );
		}
	}

	Type *GenericInstantiator::replaceWithConcrete( Type *type, bool doClone ) {
		if ( TypeInstType *typeInst = dynamic_cast< TypeInstType * >( type ) ) {
			if ( env && ! inFunctionType ) {
				Type *concrete = env->lookup( typeInst->get_name() );
				if ( concrete ) {
					return concrete->clone();
				}
				else return typeInst->clone();
			}
		} else if ( StructInstType *structType = dynamic_cast< StructInstType* >( type ) ) {
			if ( doClone ) {
				structType = structType->clone();
			}
			replaceParametersWithConcrete( structType->get_parameters() );
			return structType;
		} else if ( UnionInstType *unionType = dynamic_cast< UnionInstType* >( type ) ) {
			if ( doClone ) {
				unionType = unionType->clone();
			}
			replaceParametersWithConcrete( unionType->get_parameters() );
			return unionType;
		}
		return type;
	}


	Type* GenericInstantiator::postmutate( StructInstType *inst ) {
		// exit early if no need for further mutation
		if ( inst->get_parameters().empty() ) return inst;

		// need to replace type variables to ensure that generic types are instantiated for the return values of polymorphic functions (in particular, for thunks, because they are not [currently] copy constructed).
		replaceWithConcrete( inst, false );

		// check for an already-instantiatiated dtype-static type
		if ( dtypeStatics.find( inst->get_baseStruct() ) != dtypeStatics.end() ) {
			stripInstParams( inst );
			return inst;
		}

		// check if type can be concretely instantiated; put substitutions into typeSubs
		assertf( inst->get_baseParameters(), "Base struct has parameters" );
		std::list< TypeExpr* > typeSubs;
		genericType gt = makeSubstitutions( *inst->get_baseParameters(), inst->get_parameters(), typeSubs );
		switch ( gt ) {
		case genericType::dtypeStatic:
			stripDtypeParams( inst->get_baseStruct(), *inst->get_baseParameters(), typeSubs );
			stripInstParams( inst );
			break;

		case genericType::concrete: {
			// make concrete instantiation of generic type
			StructDecl *concDecl = lookup( inst, typeSubs );
			if ( ! concDecl ) {
				// set concDecl to new type, insert type declaration into statements to add
				concDecl = new StructDecl( typeNamer.newName( inst->get_name() ) );
				concDecl->set_body( inst->get_baseStruct()->has_body() );
				substituteMembers( inst->get_baseStruct()->get_members(), *inst->get_baseParameters(), typeSubs, concDecl->get_members() );
				// Forward declare before recursion. (TODO: Only when needed, #199.)
				insert( inst, typeSubs, concDecl );
				if ( StructDecl *forwardDecl = asForward( concDecl ) ) {
					declsToAddBefore.push_back( forwardDecl );
				}
				concDecl->acceptMutator( *visitor ); // recursively instantiate members
				declsToAddBefore.push_back( concDecl ); // must occur before declaration is added so that member instantiations appear first
			}
			StructInstType *newInst = new StructInstType( inst->get_qualifiers(), concDecl->get_name() );
			newInst->set_baseStruct( concDecl );

			delete inst;
			inst = newInst;
			break;
		}

		case genericType::dynamic:
			// do nothing
			break;
		}

		deleteAll( typeSubs );
		return inst;
	}

	Type* GenericInstantiator::postmutate( UnionInstType *inst ) {
		// exit early if no need for further mutation
		if ( inst->get_parameters().empty() ) return inst;

		// check for an already-instantiatiated dtype-static type
		if ( dtypeStatics.find( inst->get_baseUnion() ) != dtypeStatics.end() ) {
			stripInstParams( inst );
			return inst;
		}

		// check if type can be concretely instantiated; put substitutions into typeSubs
		assert( inst->get_baseParameters() && "Base union has parameters" );
		std::list< TypeExpr* > typeSubs;
		genericType gt = makeSubstitutions( *inst->get_baseParameters(), inst->get_parameters(), typeSubs );
		switch ( gt ) {
		case genericType::dtypeStatic:
			stripDtypeParams( inst->get_baseUnion(), *inst->get_baseParameters(), typeSubs );
			stripInstParams( inst );
			break;

		case genericType::concrete:
		{
			// make concrete instantiation of generic type
			UnionDecl *concDecl = lookup( inst, typeSubs );
			if ( ! concDecl ) {
				// set concDecl to new type, insert type declaration into statements to add
				concDecl = new UnionDecl( typeNamer.newName( inst->get_name() ) );
				concDecl->set_body( inst->get_baseUnion()->has_body() );
				substituteMembers( inst->get_baseUnion()->get_members(), *inst->get_baseParameters(), typeSubs, concDecl->get_members() );
				// Forward declare before recursion. (TODO: Only when needed, #199.)
				insert( inst, typeSubs, concDecl );
				if ( UnionDecl *forwardDecl = asForward( concDecl ) ) {
					declsToAddBefore.push_back( forwardDecl );
				}
				concDecl->acceptMutator( *visitor ); // recursively instantiate members
				declsToAddBefore.push_back( concDecl ); // must occur before declaration is added so that member instantiations appear first
			}
			UnionInstType *newInst = new UnionInstType( inst->get_qualifiers(), concDecl->get_name() );
			newInst->set_baseUnion( concDecl );

			delete inst;
			inst = newInst;
			break;
		}
		case genericType::dynamic:
			// do nothing
			break;
		}

		deleteAll( typeSubs );
		return inst;
	}

	namespace {
		bool isGenericType( Type * t ) {
			if ( StructInstType * inst = dynamic_cast< StructInstType * >( t ) ) {
				return ! inst->parameters.empty();
			} else if ( UnionInstType * inst = dynamic_cast< UnionInstType * >( t ) ) {
				return ! inst->parameters.empty();
			}
			return false;
		}
	}

	void GenericInstantiator::premutate( MemberExpr * memberExpr ) {
		GuardValue( memberIndex );
		memberIndex = -1;
		if ( isGenericType( memberExpr->aggregate->result ) ) {
			// find the location of the member
			AggregateDecl * aggr = memberExpr->aggregate->result->getAggr();
			std::list< Declaration * > & members = aggr->members;
			memberIndex = std::distance( members.begin(), std::find( members.begin(), members.end(), memberExpr->member ) );
			assertf( memberIndex < (int)members.size(), "Could not find member %s in generic type %s", toString( memberExpr->member ).c_str(), toString( memberExpr->aggregate ).c_str() );
		}
	}

	Expression * GenericInstantiator::postmutate( MemberExpr * memberExpr ) {
		if ( memberIndex != -1 ) {
			// using the location from the generic type, find the member in the instantiation and rebuild the member expression
			AggregateDecl * aggr = memberExpr->aggregate->result->getAggr();
			assertf( memberIndex < (int)aggr->members.size(), "Instantiation somehow has fewer members than the generic type." );
			Declaration * member = *std::next( aggr->members.begin(), memberIndex );
			assertf( member->name == memberExpr->member->name, "Instantiation has different member order than the generic type. %s / %s", toString( member ).c_str(), toString( memberExpr->member ).c_str() );
			DeclarationWithType * field = strict_dynamic_cast< DeclarationWithType * >( member );
			MemberExpr * ret = new MemberExpr( field, memberExpr->aggregate->clone() );
			ResolvExpr::adjustExprType( ret->result ); // pointer decay
			std::swap( ret->env, memberExpr->env );
			delete memberExpr;
			return ret;
		}
		return memberExpr;
	}

	void GenericInstantiator::beginScope() {
		instantiations.beginScope();
	}

	void GenericInstantiator::endScope() {
		instantiations.endScope();
	}

	template< typename AggrInst >
	Expression * FixDtypeStatic::fixMemberExpr( AggrInst * inst, MemberExpr * memberExpr ) {
		// need to cast dtype-static member expressions to their actual type before that type is erased.
		// NOTE: the casts here have the third argument (isGenerated) set to false so that these casts persist until Box, where they are needed.
		auto & baseParams = *inst->get_baseParameters();
		if ( isDtypeStatic( baseParams ) ) {
			if ( ! ResolvExpr::typesCompatible( memberExpr->result, memberExpr->member->get_type(), SymTab::Indexer() ) ) {
				// type of member and type of expression differ
				Type * concType = memberExpr->result->clone();
				if ( isLvalueArg ) {
					// result must be C lvalue, so make a new reference variable with the correct actual type to replace the member expression
					//   forall(dtype T)
					//   struct Ptr {
					//     T * x
					//   };
					//   Ptr(int) p;
					//   int i;
					//   p.x = &i;
					// becomes
					//   int *& _dtype_static_member_0 = (int **)&p.x;
					//   _dtype_static_member_0 = &i;
					// Note: this currently creates more temporaries than is strictly necessary, since it does not check for duplicate uses of the same member expression.
					static UniqueName tmpNamer( "_dtype_static_member_" );
					Expression * init = new CastExpr( new AddressExpr( memberExpr ), new PointerType( Type::Qualifiers(), concType->clone() ), false );
					ObjectDecl * tmp = ObjectDecl::newObject( tmpNamer.newName(), new ReferenceType( Type::Qualifiers(), concType ), new SingleInit( init ) );
					stmtsToAddBefore.push_back( new DeclStmt( tmp ) );
					return new VariableExpr( tmp );
				} else {
					// can simply add a cast to actual type
					return new CastExpr( memberExpr, concType, false );
				}
			}
		}
		return memberExpr;
	}

	Expression * FixDtypeStatic::postmutate( MemberExpr * memberExpr ) {
		Type * aggrType = memberExpr->aggregate->result;
		if ( isGenericType( aggrType ) ) {
			if ( StructInstType * inst = dynamic_cast< StructInstType * >( aggrType ) ) {
				return fixMemberExpr( inst, memberExpr );
			} else if ( UnionInstType * inst = dynamic_cast< UnionInstType * >( aggrType ) ) {
				return fixMemberExpr( inst, memberExpr );
			}
		}
		return memberExpr;
	}

	void FixDtypeStatic::premutate( ApplicationExpr * appExpr ) {
		GuardValue( isLvalueArg );
		isLvalueArg = false;
		DeclarationWithType * function = InitTweak::getFunction( appExpr );
		if ( function->linkage == LinkageSpec::Intrinsic && CodeGen::isAssignment( function->name ) ) {
			// explicitly visit children because only the first argument must be a C lvalue.
			visit_children = false;
			appExpr->env = maybeMutate( appExpr->env, *visitor );
			appExpr->result = maybeMutate( appExpr->result, *visitor );
			appExpr->function = maybeMutate( appExpr->function, *visitor );
			isLvalueArg = true;
			for ( Expression * arg : appExpr->args ) {
				arg = maybeMutate( arg, *visitor );
				isLvalueArg = false;
			}
		}
	}

	void FixDtypeStatic::premutate( AddressExpr * ) {
		// argument of & must be C lvalue
		GuardValue( isLvalueArg );
		isLvalueArg = true;
	}
} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
