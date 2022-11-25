//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeEnvironment.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 29 11:00:00 2019
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Dec 11 21:49:13 2019
// Update Count     : 4
//

#include "TypeEnvironment.hpp"

#include <algorithm>  // for copy
#include <cassert>
#include <iterator>   // for ostream_iterator
#include <iostream>
#include <string>
#include <utility>    // for move
#include <vector>

#include "Decl.hpp"
#include "Node.hpp"
#include "Pass.hpp"
#include "Print.hpp"
#include "Type.hpp"
#include "Common/Indenter.h"
#include "ResolvExpr/typeops.h"    // for occurs
#include "ResolvExpr/WidenMode.h"
#include "ResolvExpr/Unify.h"      // for unifyInexact
#include "Tuples/Tuples.h"         // for isTtype
#include "CompilationState.h"

using ResolvExpr::WidenMode;

namespace ast {

void print( std::ostream & out, const AssertionSet & assns, Indenter indent ) {
	for ( const auto & i : assns ) {
		print( out, i.first, indent );
		out << ( i.second.isUsed ? " (used)" : " (not used)" );
	}
}

void print( std::ostream & out, const OpenVarSet & open, Indenter indent ) {
	out << indent;
	bool first = true;
	for ( const auto & i : open ) {
		if ( first ) { first = false; } else { out << ' '; }
		out << i.first.typeString() << "(" << i.second << ")";
	}
}

void print( std::ostream & out, const EqvClass & clz, Indenter indent ) {
	out << "(";
	bool first = true;
	for(const auto & var : clz.vars) {
		if(first) first = false;
		else out << " ";

		if( deterministic_output ) out << "[unbound]";
		else out << "_" << var.formal_usage << "_" << var.expr_id << "_";

		out << var.base->name;
	}
	out << ")";

	if ( clz.bound ) {
		out << " -> ";
		print( out, clz.bound, indent+1 );
	}

	if ( ! clz.allowWidening ) {
		out << " (no widening)";
	}

	out << std::endl;
}

const EqvClass * TypeEnvironment::lookup( const TypeEnvKey & var ) const {
	for ( ClassList::const_iterator i = env.begin(); i != env.end(); ++i ) {
		if ( i->vars.find( var ) != i->vars.end() ) return &*i;
	}
	return nullptr;
}

namespace {
	/// Removes any class from env that intersects eqvClass
	void filterOverlappingClasses( std::list<EqvClass> & env, const EqvClass & eqvClass ) {
		auto i = env.begin();
		while ( i != env.end() ) {
			auto next = i; ++next;  // save next node in case of erasure

			for ( const auto & v : eqvClass.vars ) {
				if ( i->vars.count( v ) ) {
					env.erase( i );  // remove overlapping class
					break;           // done with this class
				}
			}

			i = next;  // go to next node even if this removed
		}
	}
}

void TypeEnvironment::add( const FunctionType::ForallList & tyDecls ) {
	for ( auto & tyDecl : tyDecls ) {
		env.emplace_back( tyDecl );
	}
}

void TypeEnvironment::add( const TypeSubstitution & sub ) {
	for ( const auto & p : sub ) {
		add( EqvClass{ p.first, p.second } );
	}
}

void TypeEnvironment::writeToSubstitution( TypeSubstitution & sub ) const {
	for ( const auto & clz : env ) {
		TypeEnvKey clzRep;
		bool first = true;
		for ( const auto & var : clz.vars ) {
			if ( clz.bound ) {
				sub.add( var, clz.bound );
			} else if ( first ) {
				clzRep = var;
				first = false;
			} else {
				sub.add( var, new TypeInstType{ clzRep } );
			}
		}
	}
	sub.normalize();
}

void TypeEnvironment::simpleCombine( const TypeEnvironment & o ) {
	env.insert( env.end(), o.env.begin(), o.env.end() );
}

namespace {
	/// Implements occurs check by traversing type
	struct Occurs : public ast::WithVisitorRef<Occurs> {
		bool result;
		std::unordered_set< TypeEnvKey > vars;
		const TypeEnvironment & tenv;

		Occurs( const TypeEnvKey & var, const TypeEnvironment & env )
		: result( false ), vars(), tenv( env ) {
			if ( const EqvClass * clz = tenv.lookup( var ) ) {
				vars = clz->vars;
			} else {
				vars.emplace( var );
			}
		}

		void previsit( const TypeInstType * typeInst ) {
			if ( vars.count( *typeInst ) ) {
				result = true;
			} else if ( const EqvClass * clz = tenv.lookup( *typeInst ) ) {
				if ( clz->bound ) {
					clz->bound->accept( *visitor );
				}
			}
		}
	};

	/// true if `var` occurs in `ty` under `env`
	bool occurs( const Type * ty, const TypeEnvKey & var, const TypeEnvironment & env ) {
		Pass<Occurs> occur{ var, env };
		maybe_accept( ty, occur );
		return occur.core.result;
	}
}

bool TypeEnvironment::combine(
		const TypeEnvironment & o, OpenVarSet & open, const SymbolTable & symtab ) {
	// short-circuit easy cases
	if ( o.empty() ) return true;
	if ( empty() ) {
		simpleCombine( o );
		return true;
	}

	// merge classes
	for ( const EqvClass & c : o.env ) {
		// index of typeclass in local environment bound to c
		auto rt = env.end();

		// look for first existing bound variable
		auto vt = c.vars.begin();
		for ( ; vt != c.vars.end(); ++vt ) {
			rt = internal_lookup( *vt );
			if ( rt != env.end() ) break;
		}

		if ( rt != env.end() ) {  // c needs to be merged into *rt
			EqvClass & r = *rt;
			// merge bindings
			if ( ! mergeBound( r, c, open, symtab ) ) return false;
			// merge previous unbound variables into this class, checking occurs if needed
			if ( r.bound ) for ( const auto & u : c.vars ) {
				if ( occurs( r.bound, u, *this ) ) return false;
				r.vars.emplace( u );
			} else { r.vars.insert( c.vars.begin(), vt ); }
			// merge subsequent variables into this class (skipping *vt, already there)
			while ( ++vt != c.vars.end() ) {
				auto st = internal_lookup( *vt );
				if ( st == env.end() ) {
					// unbound, safe to add if occurs
					if ( r.bound && occurs( r.bound, *vt, *this ) ) return false;
					r.vars.emplace( *vt );
				} else if ( st != rt ) {
					// bound, but not to the same class
					if ( ! mergeClasses( rt, st, open, symtab ) ) return false;
				}	// ignore bound into the same class
			}
		} else {  // no variables in c bound; just copy up
			env.emplace_back( c );
		}
	}

	// merged all classes
	return true;
}

void TypeEnvironment::extractOpenVars( OpenVarSet & open ) const {
	for ( const auto & clz : env ) {
		for ( const auto & var : clz.vars ) {
			open[ var ] = clz.data;
		}
	}
}

void TypeEnvironment::addActual( const TypeEnvironment & actualEnv, OpenVarSet & open ) {
	for ( const auto & clz : actualEnv ) {
		EqvClass c = clz;
		c.allowWidening = false;
		for ( const auto & var : c.vars ) {
			open[ var ] = c.data;
		}
		env.emplace_back( std::move(c) );
	}
}

/// true if a type is a function type
bool isFtype( const Type * type ) {
	if ( dynamic_cast< const FunctionType * >( type ) ) {
		return true;
	} else if ( auto typeInst = dynamic_cast< const TypeInstType * >( type ) ) {
		return typeInst->kind == TypeDecl::Ftype;
	} else return false;
}

namespace {
	/// true if the given type can be bound to the given type variable
	bool tyVarCompatible( const TypeData & data, const Type * type ) {
		switch ( data.kind ) {
		  case TypeDecl::Dtype:
			// to bind to an object type variable, the type must not be a function type.
			// if the type variable is specified to be a complete type then the incoming
			// type must also be complete
			// xxx - should this also check that type is not a tuple type and that it's not a ttype?
			return ! isFtype( type ) && ( ! data.isComplete || type->isComplete() );
		  case TypeDecl::Ftype:
			return isFtype( type );
		  case TypeDecl::Ttype:
			// ttype unifies with any tuple type
			return dynamic_cast< const TupleType * >( type ) || Tuples::isTtype( type );
		  default:
			assertf(false, "Unhandled tyvar kind: %d", data.kind);
			return false;
		}
	}
}

bool TypeEnvironment::bindVar(
		const TypeInstType * typeInst, const Type * bindTo, const TypeData & data,
		AssertionSet & need, AssertionSet & have, const OpenVarSet & open, WidenMode widen,
		const SymbolTable & symtab
) {
	// remove references from bound type, so that type variables can only bind to value types
	ptr<Type> target = bindTo->stripReferences();
	auto tyvar = open.find( *typeInst );
	assert( tyvar != open.end() );
	if ( ! tyVarCompatible( tyvar->second, target ) ) return false;
	if ( occurs( target, *typeInst, *this ) ) return false;

	auto it = internal_lookup( *typeInst );
	if ( it != env.end() ) {
		if ( it->bound ) {
			// attempt to unify equivalence class type with type to bind to.
			// equivalence class type has stripped qualifiers which must be restored
			ptr<Type> common;
			ptr<Type> newType = it->bound;
			reset_qualifiers( newType, typeInst->qualifiers );
			if ( unifyInexact(
					newType, target, *this, need, have, open,
					widen & WidenMode{ it->allowWidening, true }, symtab, common ) ) {
				if ( common ) {
					it->bound = std::move(common);
					reset_qualifiers( it->bound );
				}
			} else return false;
		} else {
			it->bound = std::move(target);
			reset_qualifiers( it->bound );
			it->allowWidening = widen.first && widen.second;
		}
	} else {
		env.emplace_back(
			*typeInst, target, widen.first && widen.second, data );
	}
	return true;
}

bool TypeEnvironment::bindVarToVar(
		const TypeInstType * var1, const TypeInstType * var2, TypeData && data,
		AssertionSet & need, AssertionSet & have, const OpenVarSet & open,
		WidenMode widen, const SymbolTable & symtab
) {
	auto c1 = internal_lookup( *var1 );
	auto c2 = internal_lookup( *var2 );

	// exit early if variables already bound together
	if ( c1 != env.end() && c1 == c2 ) {
		c1->allowWidening &= widen;
		return true;
	}

	bool widen1 = false, widen2 = false;
	const Type * type1 = nullptr, * type2 = nullptr;

	// check for existing bindings, perform occurs check
	if ( c1 != env.end() ) {
		if ( c1->bound ) {
			if ( occurs( c1->bound, *var2, *this ) ) return false;
			type1 = c1->bound;
		}
		widen1 = widen.first && c1->allowWidening;
	}
	if ( c2 != env.end() ) {
		if ( c2->bound ) {
			if ( occurs( c2->bound, *var1, *this ) ) return false;
			type2 = c2->bound;
		}
		widen2 = widen.second && c2->allowWidening;
	}

	if ( type1 && type2 ) {
		// both classes bound, merge if bound types can be unified
		ptr<Type> newType1{ type1 }, newType2{ type2 };
		WidenMode newWidenMode{ widen1, widen2 };
		ptr<Type> common;

		if ( unifyInexact(
				newType1, newType2, *this, need, have, open, newWidenMode, symtab, common ) ) {
			c1->vars.insert( c2->vars.begin(), c2->vars.end() );
			c1->allowWidening = widen1 && widen2;
			if ( common ) {
				c1->bound = std::move(common);
				reset_qualifiers( c1->bound );
			}
			c1->data.isComplete |= data.isComplete;
			env.erase( c2 );
		} else return false;
	} else if ( c1 != env.end() && c2 != env.end() ) {
		// both classes exist, at least one unbound, merge unconditionally
		if ( type1 ) {
			c1->vars.insert( c2->vars.begin(), c2->vars.end() );
			c1->allowWidening = widen1;
			c1->data.isComplete |= data.isComplete;
			env.erase( c2 );
		} else {
			c2->vars.insert( c1->vars.begin(), c1->vars.end() );
			c2->allowWidening = widen2;
			c2->data.isComplete |= data.isComplete;
			env.erase( c1 );
		}
	} else if ( c1 != env.end() ) {
		// var2 unbound, add to env[c1]
		c1->vars.emplace( *var2 );
		c1->allowWidening = widen1;
		c1->data.isComplete |= data.isComplete;
	} else if ( c2 != env.end() ) {
		// var1 unbound, add to env[c2]
		c2->vars.emplace( *var1 );
		c2->allowWidening = widen2;
		c2->data.isComplete |= data.isComplete;
	} else {
		// neither var bound, create new class
		env.emplace_back( *var1, *var2, widen1 && widen2, data );
	}

	return true;
}

void TypeEnvironment::forbidWidening() {
	for ( EqvClass& c : env ) c.allowWidening = false;
}

void TypeEnvironment::add( EqvClass && eqvClass ) {
	filterOverlappingClasses( env, eqvClass );
	env.emplace_back( std::move(eqvClass) );
}

bool TypeEnvironment::mergeBound(
		EqvClass & to, const EqvClass & from, OpenVarSet & open, const SymbolTable & symtab ) {
	if ( from.bound ) {
		if ( to.bound ) {
			// attempt to unify bound types
			ptr<Type> toType{ to.bound }, fromType{ from.bound };
			WidenMode widen{ to.allowWidening, from.allowWidening };
			ptr<Type> common;
			AssertionSet need, have;

			if ( unifyInexact(
					toType, fromType, *this, need, have, open, widen, symtab, common ) ) {
				// unifies, set common type if necessary
				if ( common ) {
					to.bound = std::move(common);
					reset_qualifiers( to.bound );
				}
			} else return false; // cannot unify
		} else {
			to.bound = from.bound;
		}
	}

	// unify widening if matches
	to.allowWidening &= from.allowWidening;
	return true;
}

bool TypeEnvironment::mergeClasses(
	ClassList::iterator to, ClassList::iterator from, OpenVarSet & open, const SymbolTable & symtab
) {
	EqvClass & r = *to, & s = *from;

	// ensure bounds match
	if ( ! mergeBound( r, s, open, symtab ) ) return false;

	// check safely bindable
	if ( r.bound ) {
		for ( const auto & v : s.vars ) if ( occurs( r.bound, v, *this ) ) return false;
	}

	// merge classes
	r.vars.insert( s.vars.begin(), s.vars.end() );
	r.allowWidening &= s.allowWidening;
	env.erase( from );

	return true;
}

TypeEnvironment::ClassList::iterator TypeEnvironment::internal_lookup( const TypeEnvKey & var ) {
	for ( ClassList::iterator i = env.begin(); i != env.end(); ++i ) {
		if ( i->vars.count( var ) ) return i;
	}
	return env.end();
}

void print( std::ostream & out, const TypeEnvironment & env, Indenter indent ) {
	for ( const auto & clz : env ) {
		print( out, clz, indent );
	}
}

std::ostream & operator<<( std::ostream & out, const TypeEnvironment & env ) {
	print( out, env );
	return out;
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
