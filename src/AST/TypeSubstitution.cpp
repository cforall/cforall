//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeSubstitution.cpp --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Thr May 25 11:24:00 2023
// Update Count     : 6
//

#include "TypeSubstitution.hpp"

#include "Type.hpp"   // for TypeInstType, Type, StructInstType, UnionInstType
#include "Pass.hpp"   // for Pass, PureVisitor, WithGuards, WithVisitorRef

namespace ast {

TypeSubstitution::TypeSubstitution() {
}

TypeSubstitution::TypeSubstitution( const TypeSubstitution &other ) : Node() {
	initialize( other, *this );
}

TypeSubstitution::~TypeSubstitution() {}

TypeSubstitution &TypeSubstitution::operator=( const TypeSubstitution &other ) {
	if ( this == &other ) return *this;
	initialize( other, *this );
	return *this;
}

void TypeSubstitution::initialize( const TypeSubstitution &src, TypeSubstitution &dest ) {
	dest.typeMap.clear();
	dest.add( src );
}

void TypeSubstitution::add( const TypeSubstitution &other ) {
	for ( TypeMap::const_iterator i = other.typeMap.begin(); i != other.typeMap.end(); ++i ) {
		typeMap[ i->first ] = i->second;
	} // for
}

void TypeSubstitution::add( const TypeInstType * formalType, const Type *actualType ) {
	typeMap[ *formalType ] = actualType;
}

void TypeSubstitution::add( const TypeEnvKey & key, const Type * actualType) {
	typeMap[ key ] = actualType;
}

void TypeSubstitution::remove( const TypeInstType * formalType ) {
	TypeMap::iterator i = typeMap.find( *formalType );
	if ( i != typeMap.end() ) {
		typeMap.erase( *formalType );
	} // if
}

const Type *TypeSubstitution::lookup(
		const TypeEnvKey & formalType ) const {
	TypeMap::const_iterator i = typeMap.find( formalType );

	// break on not in substitution set
	if ( i == typeMap.end() ) return 0;

	// attempt to transitively follow TypeInstType links.
	while ( const TypeInstType *actualType = i->second.as<TypeInstType>()) {
		// break cycles in the transitive follow
		if ( formalType == *actualType ) break;

		// Look for the type this maps to, returning previous mapping if none-such
		i = typeMap.find( *actualType );
		if ( i == typeMap.end() ) return actualType;
	}

	// return type from substitution set
	return i->second;
}

const Type *TypeSubstitution::lookup( const TypeInstType * formalType ) const {
	return lookup( ast::TypeEnvKey( *formalType ) );
}

bool TypeSubstitution::empty() const {
	return typeMap.empty();
}

namespace {
	struct EnvTrimmer {
		const TypeSubstitution * env;
		TypeSubstitution * newEnv;
		EnvTrimmer( const TypeSubstitution * env, TypeSubstitution * newEnv ) : env( env ), newEnv( newEnv ){}
		void previsit( const FunctionType * ftype ) {
			// transfer known bindings for seen type variables
			for (auto & formal : ftype->forall) {
				if ( const Type * t = env->lookup( formal ) ) {
					newEnv->add( formal, t );
				}
			}
		}
	};
} // namespace

/// reduce environment to just the parts that are referenced in a given expression
TypeSubstitution * TypeSubstitution::newFromExpr( const Expr * expr, const TypeSubstitution * env ) {
	if ( env ) {
		TypeSubstitution * newEnv = new TypeSubstitution();
		Pass<EnvTrimmer> trimmer( env, newEnv );
		expr->accept( trimmer );
		return newEnv;
	}
	return nullptr;
}

// definitition must happen after PassVisitor is included so that WithGuards can be used
struct TypeSubstitution::Substituter : public WithGuards, public WithVisitorRef<Substituter>, public PureVisitor {
	//static size_t traceId;

	Substituter( const TypeSubstitution & sub, bool freeOnly ) : sub( sub ), freeOnly( freeOnly ) {}

	const Type * postvisit( const TypeInstType * aggregateUseType );

	/// Records type variable bindings from forall-statements
	void previsit( const FunctionType * type );
	/// Records type variable bindings from forall-statements and instantiations of generic types
	// void handleAggregateType( const BaseInstType * type );

	// void previsit( const StructInstType * aggregateUseType );
	// void previsit( const UnionInstType * aggregateUseType );

	const TypeSubstitution & sub;
	int subCount = 0;
	bool freeOnly;
	typedef std::unordered_set< TypeEnvKey > BoundVarsType;
	BoundVarsType boundVars;
};

// size_t TypeSubstitution::Substituter::traceId = Stats::Heap::new_stacktrace_id("TypeSubstitution");

void TypeSubstitution::normalize() {
	Pass<Substituter> sub( *this, true );
	do {
		sub.core.subCount = 0;
		sub.core.freeOnly = true;
		for ( TypeMap::iterator i = typeMap.begin(); i != typeMap.end(); ++i ) {
			i->second = i->second->accept( sub );
		}
	} while ( sub.core.subCount );
}

TypeSubstitution::ApplyResult<Node> TypeSubstitution::applyBase(
		const Node * input, bool isFree ) const {
	assert( input );
	Pass<Substituter> sub( *this, isFree );
	const Node * output = input->accept( sub );
	return { output, sub.core.subCount };
}

const Type * TypeSubstitution::Substituter::postvisit( const TypeInstType *inst ) {
	BoundVarsType::const_iterator bound = boundVars.find( *inst );
	if ( bound != boundVars.end() ) return inst;

	TypeMap::const_iterator i = sub.typeMap.find( *inst );
	if ( i == sub.typeMap.end() ) {
		return inst;
	} else {
		// cut off infinite loop for the case where a type is bound to itself.
		// Note: this does not prevent cycles in the general case, so it may be necessary to do something more sophisticated here.
		// TODO: investigate preventing type variables from being bound to themselves in the first place.
		if ( const TypeInstType * replacement = i->second.as<TypeInstType>() ) {
			if ( *inst == *replacement ) {
				return inst;
			}
		}
		// std::cerr << "found " << inst->name << ", replacing with " << i->second << std::endl;
		subCount++;
		ptr<Type> newType = i->second; // force clone if needed
		add_qualifiers( newType, inst->qualifiers );
		// Note: need to recursively apply substitution to the new type because normalize does not
		// substitute bound vars, but bound vars must be substituted when not in freeOnly mode.
		newType = newType->accept( *visitor );
		return newType.release();
	} // if
}

void TypeSubstitution::Substituter::previsit( const FunctionType * ptype ) {
	GuardValue( boundVars );
	// bind type variables from forall-qualifiers
	if ( freeOnly ) {
		for ( auto & tyvar : ptype->forall ) {
				boundVars.insert( *tyvar );
		} // for
	} // if
}

/*
void TypeSubstitution::Substituter::handleAggregateType( const BaseInstType * type ) {
	GuardValue( boundVars );
	// bind type variables from forall-qualifiers
	if ( freeOnly ) {
		// bind type variables from generic type instantiations
		if ( auto decl = type->aggr() ) {
			if ( ! type->params.empty() ) {
				for ( const TypeDecl * tyvar : decl->params ) {
					boundVars.insert( *tyvar );
				} // for
			} // if
		}
	} // if
}

void TypeSubstitution::Substituter::previsit( const StructInstType * aggregateUseType ) {
	handleAggregateType( aggregateUseType );
}

void TypeSubstitution::Substituter::previsit( const UnionInstType *aggregateUseType ) {
	handleAggregateType( aggregateUseType );
}
*/

} // namespace ast

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
