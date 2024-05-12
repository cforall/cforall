//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// InstantiateGeneric.cpp -- Create concrete instances of generic types.
//
// Author           : Andrew Beach
// Created On       : Tue Aug 16 10:51:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Oct 31 16:48:00 2022
// Update Count     : 1
//

#include "InstantiateGeneric.hpp"

#include <cassert>                     // for assertf, assert
#include <set>                         // for set
#include <utility>                     // for move, pair
#include <vector>                      // for vector

#include "AST/Copy.hpp"                // for deepCopy
#include "AST/Create.hpp"              // for asForward
#include "AST/Inspect.hpp"             // for getFunction
#include "AST/Pass.hpp"                // for Pass, WithGuard, WithShortCi...
#include "AST/TranslationUnit.hpp"     // for TranslationUnit
#include "AST/Vector.hpp"              // for vector
#include "CodeGen/OperatorTable.hpp"   // for isAssignment
#include "Common/ScopedMap.hpp"        // for ScopedMap
#include "Common/UniqueName.hpp"       // for UniqueName
#include "GenPoly/GenPoly.hpp"         // for isPolyType, typesPolyCompatible
#include "GenPoly/ScrubTypeVars.hpp"   // for scrubAllTypeVars
#include "ResolvExpr/AdjustExprType.hpp"  // for adjustExprType
#include "ResolvExpr/Unify.hpp"        // for typesCompatible

namespace GenPoly {

namespace {

// Utilities:

using type_vector = ast::vector< ast::TypeExpr >;

/// Abstracts type equality for a list of parameter types.
struct TypeList {
	TypeList() : params() {}
	TypeList( ast::vector< ast::Type > const & params ) :
		params( params ) {}
	TypeList( ast::vector< ast::Type > && params ) :
		params( std::move( params ) ) {}
	TypeList( TypeList const & that ) : params( that.params ) {}
	TypeList( TypeList && that ) : params( std::move( that.params ) ) {}

	TypeList( ast::vector< ast::TypeExpr > const & exprs ) :
			params() {
		for ( auto expr : exprs ) {
			params.emplace_back( ast::deepCopy( expr->type ) );
		}
	}

	TypeList & operator=( TypeList const & that ) {
		params = that.params;
		return *this;
	}

	TypeList & operator=( TypeList && that ) {
		params = std::move( that.params );
		return *this;
	}

	bool operator==( TypeList const & that ) const {
		if ( params.size() != that.params.size() ) {
			return false;
		}

		for ( auto it = params.begin(), jt = that.params.begin() ;
				it != params.end() ; ++it, ++jt ) {
			if ( !typesPolyCompatible( it->get(), jt->get() ) ) {
				return false;
			}
		}
		return true;
	}

	ast::vector<ast::Type> params;
};

/// Maps a key and a TypeList to a valuue. Also supports scoping.
class InstantiationMap final {
	/// Wraps value for a specific (AggregateDecl, TypeList) combination.
	using Instantiation = std::pair<TypeList, ast::ptr<ast::AggregateDecl>>;
	/// List of TypeLists paired with the appropriate values.
	using ValueList = std::vector<Instantiation>;
	/// Underlying map type; maps keys to a linear list of corresponding
	/// TypeLists and values.
	using InnerMap = ScopedMap<ast::ptr<ast::AggregateDecl>, ValueList>;

	InnerMap data;
public:
	void beginScope() { data.beginScope(); }
	void endScope() { data.endScope(); }

	/// Gets the value for the (declaration, type list) pair,
	/// returns null if no such value exists.
	ast::AggregateDecl const * lookup(
			ast::AggregateDecl const * key,
			type_vector const & params ) const {
		// This type repackaging is used for the helpers.
		ast::ptr<ast::AggregateDecl> ptr = key;
		TypeList typeList( params );

		// Scan scopes for matches to the key.
		for ( auto insts = data.find( key ) ;
				insts != data.end() ; insts = data.findNext( insts, ptr )) {
			for ( auto inst = insts->second.rbegin() ;
					inst != insts->second.rend() ; ++inst ) {
				if ( inst->first == typeList ) {
					return inst->second;
				}
			}
		}
		return nullptr;
	}

	/// Adds a value for a (key, type list) pair to the current scope.
	void insert( ast::AggregateDecl const * key, type_vector const & params,
			ast::AggregateDecl const * value ) {
		auto it = data.findAt( data.currentScope(), key );
		if ( it == data.end() ) {
			data.insert( key, ValueList( 1,
				Instantiation( TypeList( params ), value ) ) );
		} else {
			it->second.emplace_back(
				Instantiation( TypeList( params ), value ) );
		}
	}
};

/// Possible options for a given specialization of a generic type.
enum class GenericType {
	/// Concrete instatiation based solely on {d,f}type-to-void conversions.
	dtypeStatic,
	/// Concrete instatiation requiring at least one parameters type.
	concrete,
	/// No concrete instantiation.
	dynamic
};

GenericType & operator|=( GenericType & gt, const GenericType & ht ) {
	if ( gt < ht ) gt = ht;
	return gt;
}

bool isDtypeStatic( ast::vector<ast::TypeDecl> const & baseParams ) {
	return std::all_of( baseParams.begin(), baseParams.end(),
		[]( ast::TypeDecl const * td ){ return !td->isComplete(); }
	);
}

/// Makes substitutions of params into baseParams; returns dtypeStatic if
/// there is a concrete instantiation based only on {d,f}type-to-void
/// conversions, concrete if there is a concrete instantiation requiring at
/// least one parameter type, and dynamic if there is no concrete instantiation.
GenericType makeSubstitutions(
		ast::vector<ast::TypeExpr> & out,
		ast::vector<ast::TypeDecl> const & baseParams,
		ast::vector<ast::Expr> const & params ) {
	GenericType gt = GenericType::dtypeStatic;

	// Substitute concrete types for given parameters,
	// using incomplete types for placeholders.
	auto baseParam = baseParams.begin();
	auto param = params.begin();
	for ( ; baseParam != baseParams.end() && param != params.end() ;
			++baseParam, ++param ) {
		ast::TypeExpr const * paramExpr = param->as<ast::TypeExpr>();
		assertf( paramExpr, "Aggregate parameters should be type expressions." );

		if ( (*baseParam)->isComplete() ) {
			// Substitute parameter for complete (otype or sized dtype) type.
			if ( isPolyType( paramExpr->type ) ) {
				// Substitute polymorphic parameter type in to generic type.
				out.push_back( ast::deepCopy( paramExpr ) );
				gt = GenericType::dynamic;
			} else {
				// Normalize possibly dtype-static parameter type.
				out.emplace_back( new ast::TypeExpr( paramExpr->location,
					scrubAllTypeVars( ast::deepCopy( paramExpr->type ) ) ) );
				gt |= GenericType::concrete;
			}
		} else switch ( (*baseParam)->kind ) {
		case ast::TypeDecl::Dtype:
			// Here, pretend that any incomplete dtype is `void`.
			out.emplace_back( new ast::TypeExpr( paramExpr->location,
				new ast::VoidType() ) );
			break;
		case ast::TypeDecl::Ftype:
			// Here, pretend that any ftype is `void (*)(void)`.
			out.emplace_back( new ast::TypeExpr( paramExpr->location,
				new ast::FunctionType() ) );
			break;
		case ast::TypeDecl::Ttype:
			assertf( false, "Ttype parameters are not currently allowed as parameters to generic types." );
			break;
		default:
			assertf( false, "Unhandled type parameter kind" );
			break;
		}
	}

	assertf( baseParam == baseParams.end(), "Base Parameters not exausted." );
	assertf( param == params.end(), "Type parameters not exausted." );
	return gt;
}

/// Substitutes types of members according to baseParams => typeSubs,
/// returning the result in a new vector.
ast::vector<ast::Decl> substituteMembers(
		ast::vector<ast::Decl> const & members,
		ast::vector<ast::TypeDecl> const & baseParams,
		ast::vector<ast::TypeExpr> const & typeSubs ) {
	ast::vector<ast::Decl> out;
	ast::TypeSubstitution subs( baseParams, typeSubs );
	for ( ast::ptr<ast::Decl> const & member : members ) {
		// Create a manual copy to avoid in-place mutation.
		// If being a PureVisitor is decided to be part of apply's interface,
		// then we can actually skip this step as it will never mutate in-
		// place. (Then we don't need the extra guard to free temp value.)
		ast::ptr<ast::Decl> copy = ast::deepCopy( member.get() );
		auto result = subs.apply( copy.get() );
		out.emplace_back( result.node );
	}
	return out;
}

/// Substitutes types of members according to baseParams => typeSubs,
/// modifying them in-place.
void substituteMembersHere(
		ast::vector<ast::Decl> & members,
		ast::vector<ast::TypeDecl> const & baseParams,
		ast::vector<ast::TypeExpr> const & typeSubs ) {
	ast::TypeSubstitution subs( baseParams, typeSubs );
	for ( ast::ptr<ast::Decl> & member : members ) {
		// The member must be mutated in place to avoid breaking
		assert( member->unique() );

		auto field = member.strict_as<ast::ObjectDecl>();
		auto result = subs.apply( field->type.get() );
		auto copy = ast::mutate_field(
			field, &ast::ObjectDecl::type, result.node );

		// I'm not kidding, it is very important.
		assert( copy == member.get() );
	}
}

/// Strips the instances' type parameters.
void stripInstParams( ast::BaseInstType * inst ) {
	inst->params.clear();
}

bool isGenericType( ast::Type const * type ) {
	if ( auto inst = dynamic_cast<ast::StructInstType const *>( type ) ) {
		return !inst->params.empty();
	} else if ( auto inst = dynamic_cast<ast::UnionInstType const *>( type ) ) {
		return !inst->params.empty();
	} else {
		return false;
	}
}

// The Passes:

struct FixDtypeStatic final :
		public ast::WithGuards,
		public ast::WithVisitorRef<FixDtypeStatic>,
		public ast::WithShortCircuiting,
		public ast::WithStmtsToAdd<> {
	ast::ApplicationExpr const * previsit( ast::ApplicationExpr const * expr );
	void previsit( ast::AddressExpr const * expr );

	ast::Expr const * postvisit( ast::MemberExpr const * expr );
private:
	template<typename Aggr>
	ast::Expr const * fixMemberExpr(
		Aggr const * inst, ast::MemberExpr const * memberExpr );

	ast::Expr const * fixMemberExpr(
		ast::vector<ast::TypeDecl> const & baseParams,
		ast::MemberExpr const * memberExpr );

	bool isLValueArg = false;
};

ast::ApplicationExpr const * FixDtypeStatic::previsit(
		ast::ApplicationExpr const * expr ) {
	GuardValue( isLValueArg ) = false;
	ast::Decl const * function = ast::getFunction( expr );
	if ( ast::Linkage::Intrinsic != function->linkage
			|| !CodeGen::isAssignment( function->name ) ) {
		return expr;
	}

	// Explicity visit children because only the first element must be a
	// C lvalue (normally, it can only send info to both or neither).
	visit_children = false;
	expr = mutate_field( expr, &ast::ApplicationExpr::env,
		maybe_accept( expr->env.get(), *visitor ) );
	expr = mutate_field( expr, &ast::ApplicationExpr::result,
		maybe_accept( expr->result.get(), *visitor ) );
	expr = mutate_field( expr, &ast::ApplicationExpr::func,
		maybe_accept( expr->func.get(), *visitor ) );
	isLValueArg = true;
	for ( unsigned i = 0; i < expr->args.size(); ++i ) {
		ast::Expr const * newExpr = expr->args[i]->accept( *visitor );
		// This is declared here for lifetime reasons.
		ast::ptr<ast::CastExpr> cast;
		if ( newExpr != expr->args[i].get() &&
				(cast = dynamic_cast<ast::CastExpr const *>( newExpr )) ) {
			newExpr = cast->arg.get();
		}
		expr = mutate_field_index( expr, &ast::ApplicationExpr::args,
			i, newExpr );
		isLValueArg = false;
	}
	return expr;
}

void FixDtypeStatic::previsit( ast::AddressExpr const * ) {
	// The argument of an address expression (`&`) must be a C lvalue.
	GuardValue( isLValueArg ) = true;
}

ast::Expr const * FixDtypeStatic::postvisit( ast::MemberExpr const * expr ) {
	ast::ptr<ast::Type> const & type = expr->aggregate->result;
	if ( auto inst = type.as<ast::StructInstType>() ) {
		if ( !inst->params.empty() ) return fixMemberExpr( inst, expr );
	} else if ( auto inst = type.as<ast::UnionInstType>() ) {
		if ( !inst->params.empty() ) return fixMemberExpr( inst, expr );
	}
	return expr;
}

template<typename Aggr>
ast::Expr const * FixDtypeStatic::fixMemberExpr(
		Aggr const * inst, ast::MemberExpr const * memberExpr ) {
	return fixMemberExpr( inst->base->params, memberExpr );
}

ast::Expr const * FixDtypeStatic::fixMemberExpr(
		ast::vector<ast::TypeDecl> const & baseParams,
		ast::MemberExpr const * memberExpr ) {
	// Need to cast dtype-static member expressions to their actual type
	// before the actual type type is erased.
	// NOTE: The casts here have the third argument (isGenerated) set to
	// ExplicitCast so that they casts persist until Box, where they are needed.

	if ( !isDtypeStatic( baseParams ) ||
			ResolvExpr::typesCompatible(
				memberExpr->result,
				memberExpr->member->get_type() ) ) {
		return memberExpr;
	}

	// Type of member and type of expression differ.
	ast::Type const * concType = ast::deepCopy( memberExpr->result );
	CodeLocation const & location = memberExpr->location;
	if ( isLValueArg ) {
		// The result must be a C lvalue expression. So make a new reference
		// variable with the correct actual type to replace the
		// member expression.
		//   forall(T &)
		//   struct Ptr {
		//     T * x;
		//   };
		//   Ptr(int) p;
		//   int i;
		// The original expression:
		//   p.x = &i;
		// Becomes the expression/declaration:
		//   int *& _dtype_static_member_0;
		//   (_dtype_static_member_0 = (int**)&p.x,
		//    _dtype_static_member_0) = &i;

		// The declaration is simple:
		static UniqueName tmpNamer( "_dtype_static_member_" );
		ast::ObjectDecl * tmp = new ast::ObjectDecl( location,
			tmpNamer.newName(),
			new ast::ReferenceType( concType ),
			nullptr,
			ast::Storage::Classes(),
			ast::Linkage::C
		);
		stmtsToAddBefore.push_back( new ast::DeclStmt( location, tmp ) );

		// The expression is more complex, uses references and reference /
		// pointer parity. But breaking it up risks reordering.
		return new ast::CommaExpr( location,
			ast::UntypedExpr::createAssign( location,
				new ast::VariableExpr( location, tmp ),
				new ast::CastExpr( location,
					new ast::AddressExpr( location, memberExpr ),
					new ast::PointerType( ast::deepCopy( concType ) ),
					ast::ExplicitCast
				)
			),
			new ast::VariableExpr( location, tmp )
		);
	} else {
		// Here, it can simply add a cast to actual types.
		return new ast::CastExpr( location,
			memberExpr,
			concType,
			ast::ExplicitCast
		);
	}
}

struct GenericInstantiator final :
		public ast::WithCodeLocation,
		public ast::WithConstTypeSubstitution,
		public ast::WithDeclsToAdd<>,
		public ast::WithGuards,
		public ast::WithVisitorRef<GenericInstantiator>
{
	/// Map of (generic type, parameter list) pairs
	/// to concrete type instantiations.
	InstantiationMap instantiations;
	/// Set of types which are dtype-only generic
	/// (and therefore have static layout).
	std::set<ast::AggregateDecl const *> dtypeStatics;
	/// Namer for concrete types.
	UniqueName typeNamer;
	/// Should not make use of type environment to replace types of function
	/// parameter and return values.
	bool inFunctionType = false;
	/// Index of current member, used to recreate MemberExprs with the
	/// member from an instantiation.
	int memberIndex = -1;

	GenericInstantiator() :
		instantiations(), dtypeStatics(), typeNamer("_conc_") {}

	ast::Type const * postvisit( ast::StructInstType const * inst );
	ast::Type const * postvisit( ast::UnionInstType const * inst );

	void previsit( ast::MemberExpr const * expr );
	ast::Expr const * postvisit( ast::MemberExpr const * expr );
	ast::Expr const * postvisit( ast::Expr const * expr );
	ast::Designation const * postvisit( ast::Designation const * );

	void previsit( ast::ParseNode const * node ) {
		GuardValue( location ) = &node->location;
	}
	void previsit( ast::FunctionType const * ) {
		GuardValue( inFunctionType ) = true;
	}
	void beginScope() {
		instantiations.beginScope();
	}
	void endScope() {
		instantiations.endScope();
	}
private:
	/// Wrap instantiation lookup for structures.
	ast::StructDecl const * lookup(
		ast::StructInstType const * inst, type_vector const & typeSubs ) const;
	/// Wrap instantiation lookup for unions.
	ast::UnionDecl const * lookup(
		ast::UnionInstType const * inst, type_vector const & typeSubs ) const;
	/// Wrap instantiation insertion for structures.
	void insert( ast::StructInstType const * inst,
		type_vector const & typeSubs, ast::StructDecl const * decl );
	/// Wrap instantiation insertion for unions.
	void insert( ast::UnionInstType const * inst,
		type_vector const & typeSubs, ast::UnionDecl const * decl );

	void replaceParametersWithConcrete( ast::vector<ast::Expr> & params );
	ast::Type const * replaceWithConcrete( ast::Type const * type, bool doClone );

	template<typename AggrDecl>
	ast::Type const * fixInstType( ast::SueInstType<AggrDecl> const * inst );

	/// Strips a dtype-static aggregate decl of its type parameters,
	/// marks it as stripped.
	void stripDtypeParams( ast::AggregateDecl * base,
		ast::vector<ast::TypeDecl> & baseParams,
		ast::vector<ast::TypeExpr> const & typeSubs );
};

ast::Type const * GenericInstantiator::postvisit(
		ast::StructInstType const * inst ) {
	return fixInstType( inst );
}

ast::Type const * GenericInstantiator::postvisit(
		ast::UnionInstType const * inst ) {
	return fixInstType( inst );
}

template<typename AggrDecl>
ast::Type const * GenericInstantiator::fixInstType(
		ast::SueInstType<AggrDecl> const * inst ) {
	assert( location );

	// There is nothing to mutate if the params are empty.
	if ( inst->params.empty() ) return inst;

	// Need to replace type variables to ensure that generic types are
	// instantiated for the return values of polymorphic functions (in
	// particular, for thunks, because they are not [currently] copy
	// constructed).
	// (This used to be run only on structures, but I believe both need it.)
	inst = strict_dynamic_cast<ast::SueInstType<AggrDecl> const *>(
		replaceWithConcrete( inst, false ) );

	// Check for an already-instantiatiated dtype-static type.
	if ( dtypeStatics.find( inst->base ) != dtypeStatics.end() ) {
		auto mutInst = ast::mutate( inst );
		stripInstParams( mutInst );
		return mutInst;
	}

	// Check if the type can be concretely instantiated;
	// and put substitutions in typeSubs.
	assertf( inst->base, "Base data-type has parameters." );
	ast::vector<ast::TypeExpr> typeSubs;
	GenericType gt = makeSubstitutions( typeSubs, inst->base->params, inst->params );
	switch ( gt ) {
	case GenericType::dtypeStatic:
	{
		auto mutInst = ast::mutate( inst );
		assert( mutInst->base->unique() );
		auto mutBase = mutInst->base.get_and_mutate();
		stripDtypeParams( mutBase, mutBase->params, typeSubs );
		stripInstParams( mutInst );
		return mutInst;
	}
	case GenericType::concrete:
	{
		// Make concrete instantiation of generic type.
		AggrDecl const * concDecl = lookup( inst, typeSubs );
		if ( !concDecl ) {
			// Set concDecl to new type, insert type declaration
			// into statements to add.
			AggrDecl * newDecl = new AggrDecl( *location,
				typeNamer.newName( inst->name )
			);
			newDecl->body = inst->base->body;
			newDecl->members = substituteMembers(
				inst->base->members,
				inst->base->params,
				typeSubs
			);

			// Forward declare before recursion. (TODO: Only when needed, #199.)
			insert( inst, typeSubs, newDecl );
			if ( AggrDecl const * forwardDecl = ast::asForward( newDecl ) ) {
				declsToAddBefore.push_back( forwardDecl );
			}
			// Recursively instantiate members:
			concDecl = strict_dynamic_cast<AggrDecl const *>(
				newDecl->accept( *visitor ) );
			// Must occur before declaration is added so
			// that member instantiation appear first.
			declsToAddBefore.push_back( concDecl );
		}
		return new ast::SueInstType<AggrDecl>( concDecl, inst->qualifiers );
	}
	case GenericType::dynamic:
		// Do nothing.
	default:
		// Should never happen.
		return inst;
	}
}

void GenericInstantiator::previsit( ast::MemberExpr const * expr ) {
	GuardValue( location ) = &expr->location;
	GuardValue( memberIndex ) = -1;
	// Only run on expressions where the field being accessed is generic.
	if ( isGenericType( expr->aggregate->result ) ) {
		// Find the location of the member:
		ast::AggregateDecl const * aggr =
			expr->aggregate->result.strict_as<ast::BaseInstType>()->aggr();
		ast::vector<ast::Decl> const & members = aggr->members;
		auto it = std::find( members.begin(), members.end(), expr->member );
		memberIndex = std::distance( members.begin(), it );
		assertf( memberIndex < (int)members.size(), "Could not find member %s in generic type %s.", toString( expr->member ).c_str(), toString( expr->aggregate ).c_str() );
	}
}

ast::Expr const * GenericInstantiator::postvisit(
		ast::MemberExpr const * expr ) {
	if ( memberIndex == -1 ) {
		return expr;
	}

	// Using the location from the generic type, find the member
	// in the instantiation and rebuild the member expression.
	ast::AggregateDecl const * aggr =
		expr->aggregate->result.strict_as<ast::BaseInstType>()->aggr();
	assertf( memberIndex < (int)aggr->members.size(), "Instantiation somehow has fewer members than the generic type." );
	ast::Decl const * member = *std::next( aggr->members.begin(), memberIndex );
	assertf( member->name == expr->member->name, "Instantiation has different member order than the generic type. %s / %s", toString( member ).c_str(), toString( expr->member ).c_str() );
	auto field = strict_dynamic_cast< ast::DeclWithType const * >( member );
	ast::MemberExpr * ret = new ast::MemberExpr( expr->location,
		field,
		ast::deepCopy( expr->aggregate )
	);
	// For pointer decay:
	ret->result = ResolvExpr::adjustExprType(
		ret->result,
		ast::TypeEnvironment(),
		ast::SymbolTable()
	);
	ret->env = expr->env;
	return ret;
}

ast::Expr const * GenericInstantiator::postvisit( ast::Expr const * expr ) {
	// We are not modifying env on MemberExpr, but that seems to work.
	if ( expr->env ) {
		auto newEnv = expr->env->accept( *visitor );
		expr = ast::mutate_field( expr, &ast::Expr::env, newEnv );
	}
	return expr;
}

// This attempts to figure out what the final name of the field will be.
// Pretty printing can cause this to become incorrect.
std::string getPrintName( ast::DeclWithType const * decl ) {
	return ( decl->linkage.is_mangled )
		? decl->scopedMangleName() : decl->name;
}

ast::Designation const * GenericInstantiator::postvisit(
		ast::Designation const * designation ) {
	// Disconnect designator names from their fields.
	// It is now incorrect to point at the generic definition where the used
	// type now is replaced with a concrete instance. Ideally, new variable
	// expressions would point at fields in the concrete instances, but that
	// is work and that information should not be needed this late in
	// compilation.

	// Modify all designations, even if not needed.
	auto mutNode = mutate( designation );
	for ( ast::ptr<ast::Expr> & designator : mutNode->designators ) {
		if ( auto var = designator.as<ast::VariableExpr>() ) {
			designator = new ast::NameExpr(
				var->location, getPrintName( var->var ) );
		}
	}
	return mutNode;
}

ast::StructDecl const * GenericInstantiator::lookup(
		ast::StructInstType const * inst,
		type_vector const & typeSubs ) const {
	auto ret = instantiations.lookup( inst->base, typeSubs );
	return strict_dynamic_cast<ast::StructDecl const *, nullptr>( ret );
}

ast::UnionDecl const * GenericInstantiator::lookup(
		ast::UnionInstType const * inst,
		type_vector const & typeSubs ) const {
	auto ret = instantiations.lookup( inst->base, typeSubs );
	return strict_dynamic_cast<ast::UnionDecl const *, nullptr>( ret );
}

void GenericInstantiator::insert( ast::StructInstType const * inst,
		type_vector const & typeSubs, ast::StructDecl const * decl ) {
	instantiations.insert( inst->base, typeSubs, decl );
}

void GenericInstantiator::insert( ast::UnionInstType const * inst,
		type_vector const & typeSubs, ast::UnionDecl const * decl ) {
	instantiations.insert( inst->base, typeSubs, decl );
}

void GenericInstantiator::replaceParametersWithConcrete(
		ast::vector<ast::Expr> & params ) {
	for ( ast::ptr<ast::Expr> & param : params ) {
		auto paramType = param.as<ast::TypeExpr>();
		assertf( paramType, "Aggregate parameters should be type expressions." );
		auto type = replaceWithConcrete( paramType->type, false );
		param = ast::mutate_field( paramType, &ast::TypeExpr::type, type );
	}
}

ast::Type const * GenericInstantiator::replaceWithConcrete(
		ast::Type const * type, bool doClone ) {
	if ( auto inst = dynamic_cast<ast::TypeInstType const *>( type ) ) {
		if ( typeSubs && !inFunctionType ) {
			ast::Type const * concType = typeSubs->lookup( inst );
			return ast::deepCopy( ( concType ) ? concType : inst );
		}
	} else if ( auto inst = dynamic_cast<ast::StructInstType const *>( type ) ) {
		auto mut = ( doClone ) ? ast::deepCopy( inst ) : ast::mutate( inst );
		replaceParametersWithConcrete( mut->params );
		return mut;
	} else if ( auto inst = dynamic_cast<ast::UnionInstType const *>( type ) ) {
		auto mut = ( doClone ) ? ast::deepCopy( inst ) : ast::mutate( inst );
		replaceParametersWithConcrete( mut->params );
		return mut;
	}
	return type;
}

void GenericInstantiator::stripDtypeParams(
		ast::AggregateDecl * base,
		ast::vector<ast::TypeDecl> & baseParams,
		ast::vector<ast::TypeExpr> const & typeSubs ) {
	substituteMembersHere( base->members, baseParams, typeSubs );

	baseParams.clear();

	dtypeStatics.insert( base );
}

} // namespace

void instantiateGeneric( ast::TranslationUnit & translationUnit ) {
	ast::Pass<FixDtypeStatic>::run( translationUnit );
	ast::Pass<GenericInstantiator>::run( translationUnit );
}

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
