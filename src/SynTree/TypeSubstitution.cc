//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeSubstitution.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Mar 16 15:54:35 2017
// Update Count     : 4
//

#include <ostream>  // for ostream, basic_ostream, operator<<, endl

#include "Type.h"   // for TypeInstType, Type, StructInstType, UnionInstType
#include "TypeSubstitution.h"

TypeSubstitution::TypeSubstitution() {
}

TypeSubstitution::TypeSubstitution( const TypeSubstitution &other ) {
	initialize( other, *this );
}

TypeSubstitution::~TypeSubstitution() {
	for ( TypeEnvType::iterator i = typeEnv.begin(); i != typeEnv.end(); ++i ) {
		delete( i->second );
	}
	for ( VarEnvType::iterator i = varEnv.begin(); i != varEnv.end(); ++i ) {
		delete( i->second );
	}
}

TypeSubstitution &TypeSubstitution::operator=( const TypeSubstitution &other ) {
	if ( this == &other ) return *this;
	initialize( other, *this );
	return *this;
}

void TypeSubstitution::initialize( const TypeSubstitution &src, TypeSubstitution &dest ) {
	dest.typeEnv.clear();
	dest.varEnv.clear();
	dest.add( src );
}

void TypeSubstitution::add( const TypeSubstitution &other ) {
	for ( TypeEnvType::const_iterator i = other.typeEnv.begin(); i != other.typeEnv.end(); ++i ) {
		typeEnv[ i->first ] = i->second->clone();
	} // for
	for ( VarEnvType::const_iterator i = other.varEnv.begin(); i != other.varEnv.end(); ++i ) {
		varEnv[ i->first ] = i->second->clone();
	} // for
}

void TypeSubstitution::add( std::string formalType, Type *actualType ) {
	TypeEnvType::iterator i = typeEnv.find( formalType );
	if ( i != typeEnv.end() ) {
		delete i->second;
	} // if
	typeEnv[ formalType ] = actualType->clone();
}

void TypeSubstitution::addVar( std::string formalExpr, Expression *actualExpr ) {
	varEnv[ formalExpr ] = actualExpr;
}

void TypeSubstitution::remove( std::string formalType ) {
	TypeEnvType::iterator i = typeEnv.find( formalType );
	if ( i != typeEnv.end() ) {
		delete i->second;
		typeEnv.erase( formalType );
	} // if
}

Type *TypeSubstitution::lookup( std::string formalType ) const {
	TypeEnvType::const_iterator i = typeEnv.find( formalType );

	// break on not in substitution set
	if ( i == typeEnv.end() ) return 0;

	// attempt to transitively follow TypeInstType links.
	while ( TypeInstType *actualType = dynamic_cast< TypeInstType* >( i->second ) ) {
		const std::string& typeName = actualType->get_name();

		// break cycles in the transitive follow
		if ( formalType == typeName ) break;

		// Look for the type this maps to, returning previous mapping if none-such
		i = typeEnv.find( typeName );
		if ( i == typeEnv.end() ) return actualType;
	}

	// return type from substitution set
	return i->second;

#if 0
	if ( i == typeEnv.end() ) {
		return 0;
	} else {
		return i->second;
	} // if
#endif
}

bool TypeSubstitution::empty() const {
	return typeEnv.empty() && varEnv.empty();
}

namespace {
	struct EnvTrimmer {
		const TypeSubstitution * env;
		TypeSubstitution * newEnv;
		EnvTrimmer( const TypeSubstitution * env, TypeSubstitution * newEnv ) : env( env ), newEnv( newEnv ){}
		void previsit( TypeDecl * tyDecl ) {
			// transfer known bindings for seen type variables
			if ( Type * t = env->lookup( tyDecl->name ) ) {
				newEnv->add( tyDecl->name, t );
			}
		}
	};
} // namespace

/// reduce environment to just the parts that are referenced in a given expression
TypeSubstitution * TypeSubstitution::newFromExpr( Expression * expr, const TypeSubstitution * env ) {
	if ( env ) {
		TypeSubstitution * newEnv = new TypeSubstitution();
		PassVisitor<EnvTrimmer> trimmer( env, newEnv );
		expr->accept( trimmer );
		return newEnv;
	}
	return nullptr;
}

void TypeSubstitution::normalize() {
	PassVisitor<Substituter> sub( *this, true );
	do {
		sub.pass.subCount = 0;
		sub.pass.freeOnly = true;
		for ( TypeEnvType::iterator i = typeEnv.begin(); i != typeEnv.end(); ++i ) {
			i->second = i->second->acceptMutator( sub );
		}
	} while ( sub.pass.subCount );
}

Type * TypeSubstitution::Substituter::postmutate( TypeInstType *inst ) {
	BoundVarsType::const_iterator bound = boundVars.find( inst->name );
	if ( bound != boundVars.end() ) return inst;

	TypeEnvType::const_iterator i = sub.typeEnv.find( inst->get_name() );
	if ( i == sub.typeEnv.end() ) {
		return inst;
	} else {
		// cut off infinite loop for the case where a type is bound to itself.
		// Note: this does not prevent cycles in the general case, so it may be necessary to do something more sophisticated here.
		// TODO: investigate preventing type variables from being bound to themselves in the first place.
		if ( TypeInstType * replacement = dynamic_cast< TypeInstType * >( i->second ) ) {
			if ( inst->name == replacement->name ) {
				return inst;
			}
		}
		// std::cerr << "found " << inst->name << ", replacing with " << i->second << std::endl;
		subCount++;
		Type * newtype = i->second->clone();
		newtype->get_qualifiers() |= inst->get_qualifiers();
		delete inst;
		// Note: need to recursively apply substitution to the new type because normalize does not substitute bound vars, but bound vars must be substituted when not in freeOnly mode.
		return newtype->acceptMutator( *visitor );
	} // if
}

Expression * TypeSubstitution::Substituter::postmutate( NameExpr * nameExpr ) {
	VarEnvType::const_iterator i = sub.varEnv.find( nameExpr->name );
	if ( i == sub.varEnv.end() ) {
		return nameExpr;
	} else {
		subCount++;
		delete nameExpr;
		return i->second->clone();
	} // if
}

void TypeSubstitution::Substituter::premutate( Type * type ) {
	GuardValue( boundVars );
	// bind type variables from forall-qualifiers
	if ( freeOnly ) {
		for ( Type::ForallList::const_iterator tyvar = type->forall.begin(); tyvar != type->forall.end(); ++tyvar ) {
			boundVars.insert( (*tyvar)->name );
		} // for
	} // if
}

template< typename TypeClass >
void TypeSubstitution::Substituter::handleAggregateType( TypeClass * type ) {
	GuardValue( boundVars );
	// bind type variables from forall-qualifiers
	if ( freeOnly ) {
		for ( Type::ForallList::const_iterator tyvar = type->forall.begin(); tyvar != type->forall.end(); ++tyvar ) {
			boundVars.insert( (*tyvar)->name );
		} // for
		// bind type variables from generic type instantiations
		std::list< TypeDecl* > *baseParameters = type->get_baseParameters();
		if ( baseParameters && ! type->parameters.empty() ) {
			for ( std::list< TypeDecl* >::const_iterator tyvar = baseParameters->begin(); tyvar != baseParameters->end(); ++tyvar ) {
				boundVars.insert( (*tyvar)->name );
			} // for
		} // if
	} // if
}

void TypeSubstitution::Substituter::premutate( StructInstType * aggregateUseType ) {
	handleAggregateType( aggregateUseType );
}

void TypeSubstitution::Substituter::premutate( UnionInstType *aggregateUseType ) {
	handleAggregateType( aggregateUseType );
}

void TypeSubstitution::print( std::ostream &os, Indenter indent ) const {
	os << indent << "Types:" << std::endl;
	for ( TypeEnvType::const_iterator i = typeEnv.begin(); i != typeEnv.end(); ++i ) {
		os << indent+1 << i->first << " -> ";
		i->second->print( os, indent+2 );
		os << std::endl;
	} // for
	os << indent << "Non-types:" << std::endl;
	for ( VarEnvType::const_iterator i = varEnv.begin(); i != varEnv.end(); ++i ) {
		os << indent+1 << i->first << " -> ";
		i->second->print( os, indent+2 );
		os << std::endl;
	} // for
}

std::ostream & operator<<( std::ostream & out, const TypeSubstitution & sub ) {
	sub.print( out );
	return out;
}


// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
