//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeEnvironment.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 12:19:47 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fri Jun 18 14:27:00 2019
// Update Count     : 5
//

#include <cassert>                     // for assert
#include <algorithm>                   // for copy, set_intersection
#include <iterator>                    // for ostream_iterator, insert_iterator
#include <memory>                      // for unique_ptr
#include <utility>                     // for pair, move

#include "CompilationState.h"          // for deterministic_output
#include "Common/utility.h"            // for maybeClone
#include "SynTree/Type.h"              // for Type, FunctionType, Type::Fora...
#include "SynTree/TypeSubstitution.h"  // for TypeSubstitution
#include "Tuples/Tuples.h"             // for isTtype
#include "TypeEnvironment.h"
#include "typeops.h"                   // for occurs
#include "Unify.h"                     // for unifyInexact

namespace ResolvExpr {
	void printAssertionSet( const AssertionSet &assertions, std::ostream &os, int indent ) {
		for ( AssertionSet::const_iterator i = assertions.begin(); i != assertions.end(); ++i ) {
			i->first->print( os, indent );
			if ( i->second.isUsed ) {
				os << "(used)";
			} else {
				os << "(not used)";
			} // if
		} // for
	}

	void printOpenVarSet( const OpenVarSet &openVars, std::ostream &os, int indent ) {
		os << std::string( indent, ' ' );
		for ( OpenVarSet::const_iterator i = openVars.begin(); i != openVars.end(); ++i ) {
			os << i->first << "(" << i->second << ") ";
		} // for
	}

	void EqvClass::initialize( const EqvClass &src, EqvClass &dest ) {
		initialize( src, dest, src.type );
	}

	void EqvClass::initialize( const EqvClass &src, EqvClass &dest, const Type *ty ) {
		dest.vars = src.vars;
		dest.type = maybeClone( ty );
		dest.allowWidening = src.allowWidening;
		dest.data = src.data;
	}

	EqvClass::EqvClass() : type( nullptr ), allowWidening( true ) {
	}

	EqvClass::EqvClass( const EqvClass &other ) {
		initialize( other, *this );
	}

	EqvClass::EqvClass( const EqvClass &other, const Type *ty ) {
		initialize( other, *this, ty );
	}

	EqvClass::EqvClass( EqvClass &&other )
	: vars{std::move(other.vars)}, type{other.type},
	  allowWidening{std::move(other.allowWidening)}, data{std::move(other.data)} {
		  other.type = nullptr;
	}

	EqvClass &EqvClass::operator=( const EqvClass &other ) {
		if ( this == &other ) return *this;
		delete type;
		initialize( other, *this );
		return *this;
	}

	EqvClass &EqvClass::operator=( EqvClass &&other ) {
		if ( this == &other ) return *this;
		delete type;

		vars = std::move(other.vars);
		type = other.type;
		other.type = nullptr;
		allowWidening = std::move(other.allowWidening);
		data = std::move(other.data);

		return *this;
	}

	EqvClass::~EqvClass() {
		delete type;
	}

	void EqvClass::set_type( Type* ty ) {
		if ( ty == type ) return;
		delete type;
		type = ty;
	}

	void EqvClass::print( std::ostream &os, Indenter indent ) const {
		os << "(";
		bool first = true;
		for(const auto & var : vars) {
			if(first) first = false;
			else os << " ";
			if( deterministic_output && isUnboundType(var) ) os << "[unbound]";
			else os << var;
		}
		os << ")";
		if ( type ) {
			os << " -> ";
			type->print( os, indent+1 );
		} // if
		if ( ! allowWidening ) {
			os << " (no widening)";
		} // if
		os << std::endl;
	}

	const EqvClass* TypeEnvironment::lookup( const std::string &var ) const {
		for ( ClassList::const_iterator i = env.begin(); i != env.end(); ++i ) {
			if ( i->vars.find( var ) != i->vars.end() ) return &*i;
		} // for
		return nullptr;
	}

	/// Removes any class from env that intersects eqvClass
	void filterOverlappingClasses( std::list<EqvClass> &env, const EqvClass &eqvClass ) {
		for ( auto i = env.begin(); i != env.end(); ) {
			auto next = i;
			++next;
			std::set<std::string> intersection;
			std::set_intersection( i->vars.begin(), i->vars.end(), eqvClass.vars.begin(), eqvClass.vars.end(),
				std::inserter( intersection, intersection.begin() ) );
			if ( ! intersection.empty() ) { env.erase( i ); }
			i = next;
		}
	}

	void TypeEnvironment::add( EqvClass &&eqvClass ) {
		filterOverlappingClasses( env, eqvClass );
		env.push_back( std::move(eqvClass) );
	}

	void TypeEnvironment::add( const Type::ForallList &tyDecls ) {
		for ( Type::ForallList::const_iterator i = tyDecls.begin(); i != tyDecls.end(); ++i ) {
			EqvClass newClass;
			newClass.vars.insert( (*i)->get_name() );
			newClass.data = TypeDecl::Data{ (*i) };
			env.push_back( std::move(newClass) );
		} // for
	}

	void TypeEnvironment::add( const TypeSubstitution & sub ) {
		EqvClass newClass;
		for ( auto p : sub ) {
			newClass.vars.insert( p.first );
			newClass.type = p.second->clone();
			newClass.allowWidening = false;
			// Minimal assumptions. Not technically correct, but might be good enough, and
			// is the best we can do at the moment since information is lost in the
			// transition to TypeSubstitution
			newClass.data = TypeDecl::Data{ TypeDecl::Dtype, false };
			add( std::move(newClass) );
		}
	}

	void TypeEnvironment::makeSubstitution( TypeSubstitution &sub ) const {
		for ( ClassList::const_iterator theClass = env.begin(); theClass != env.end(); ++theClass ) {
			for ( std::set< std::string >::const_iterator theVar = theClass->vars.begin(); theVar != theClass->vars.end(); ++theVar ) {
				if ( theClass->type ) {
					sub.add( *theVar, theClass->type );
				} else if ( theVar != theClass->vars.begin() ) {
					TypeInstType *newTypeInst = new TypeInstType( Type::Qualifiers(), *theClass->vars.begin(), theClass->data.kind == TypeDecl::Ftype );
					sub.add( *theVar, newTypeInst );
					delete newTypeInst;
				} // if
			} // for
		} // for
		sub.normalize();
	}

	void TypeEnvironment::print( std::ostream &os, Indenter indent ) const {
		for ( const EqvClass & theClass : env ) {
			theClass.print( os, indent );
		} // for
	}

	TypeEnvironment::ClassList::iterator TypeEnvironment::internal_lookup( const std::string &var ) {
		for ( ClassList::iterator i = env.begin(); i != env.end(); ++i ) {
			if ( i->vars.count( var ) ) return i;
		} // for
		return env.end();
	}

	void TypeEnvironment::simpleCombine( const TypeEnvironment &second ) {
		env.insert( env.end(), second.env.begin(), second.env.end() );
	}

	// xxx -- this should maybe be worrying about iterator invalidation (see resolv-proto)
	bool TypeEnvironment::mergeBound( EqvClass& to, const EqvClass& from, OpenVarSet& openVars, const SymTab::Indexer& indexer ) {
		if ( from.type ) {
			if ( to.type ) {
				// attempt to unify bound types
				std::unique_ptr<Type> toType{ to.type->clone() }, fromType{ from.type->clone() };
				WidenMode widen{ to.allowWidening, from.allowWidening };
				Type* common = nullptr;
				AssertionSet need, have;
				if ( unifyInexact( toType.get(), fromType.get(), *this, need, have, openVars, widen, indexer, common ) ) {
					// unifies, set common type if necessary
					if ( common ) {
						common->get_qualifiers() = Type::Qualifiers{};
						to.set_type( common );
					}
				} else return false; // cannot unify
			} else {
				to.type = from.type->clone();
			}
		}

		// unify widening if matches
		to.allowWidening &= from.allowWidening;
		return true;
	}

	// xxx -- this should maybe be worrying about iterator invalidation (see resolv-proto)
	bool TypeEnvironment::mergeClasses( TypeEnvironment::ClassList::iterator to, TypeEnvironment::ClassList::iterator from, OpenVarSet& openVars, const SymTab::Indexer& indexer ) {
		EqvClass& r = *to;
		EqvClass& s = *from;

		// ensure bounds match
		if ( ! mergeBound( r, s, openVars, indexer ) ) return false;

		// check safely bindable
		if ( r.type && occursIn( r.type, s.vars.begin(), s.vars.end(), *this ) ) return false;

		// merge classes in
		r.vars.insert( s.vars.begin(), s.vars.end() );
		r.allowWidening &= s.allowWidening;
		env.erase( from );

		return true;
	}

	bool TypeEnvironment::combine( const TypeEnvironment& o, OpenVarSet& openVars, const SymTab::Indexer& indexer ) {
		// short-circuit easy cases
		if ( o.isEmpty() ) return true;
		if ( isEmpty() ) {
			simpleCombine( o );
			return true;
		}

		// merge classes
		for ( auto ct = o.env.begin(); ct != o.env.end(); ++ct ) {
			const EqvClass& c = *ct;

			// typeclass in local environment bound to c
			auto rt = env.end();

			// look for first existing bound variable
			auto vt = c.vars.begin();
			for ( ; vt != c.vars.end(); ++vt ) {
				rt = internal_lookup( *vt );
				if ( rt != env.end() ) break;
			}

			if ( rt != env.end() ) {  // c needs to be merged into *rt
				EqvClass& r = *rt;
				// merge bindings
				if ( ! mergeBound( r, c, openVars, indexer ) ) return false;
				// merge previous unbound variables into this class, checking occurs if needed
				if ( r.type ) for ( auto ut = c.vars.begin(); ut != vt; ++ut ) {
					if ( occurs( r.type, *ut, *this ) ) return false;
					r.vars.insert( *ut );
				} else { r.vars.insert( c.vars.begin(), vt ); }
				// merge subsequent variables into this class (skipping *vt, already there)
				while ( ++vt != c.vars.end() ) {
					auto st = internal_lookup( *vt );
					if ( st == env.end() ) {
						// unbound, safe to add if passes occurs
						if ( r.type && occurs( r.type, *vt, *this ) ) return false;
						r.vars.insert( *vt );
					} else if ( st != rt ) {
						// bound, but not to the same class
						if ( ! mergeClasses( rt, st, openVars, indexer ) ) return false;
					}   // ignore bound into the same class
				}
			} else {  // no variables in c bound; just copy up
				env.push_back( c );
			}
		}

		// merged all classes
		return true;
	}

	void TypeEnvironment::extractOpenVars( OpenVarSet &openVars ) const {
		for ( ClassList::const_iterator eqvClass = env.begin(); eqvClass != env.end(); ++eqvClass ) {
			for ( std::set< std::string >::const_iterator var = eqvClass->vars.begin(); var != eqvClass->vars.end(); ++var ) {
				openVars[ *var ] = eqvClass->data;
			} // for
		} // for
	}

	void TypeEnvironment::addActual( const TypeEnvironment& actualEnv, OpenVarSet& openVars ) {
		for ( const EqvClass& c : actualEnv ) {
			EqvClass c2 = c;
			c2.allowWidening = false;
			for ( const std::string& var : c2.vars ) {
				openVars[ var ] = c2.data;
			}
			env.push_back( std::move(c2) );
		}
	}

	bool isFtype( const Type * type ) {
		if ( dynamic_cast< const FunctionType * >( type ) ) {
			return true;
		} else if ( const TypeInstType *typeInst = dynamic_cast< const TypeInstType * >( type ) ) {
			return typeInst->get_isFtype();
		} // if
		return false;
	}

	bool tyVarCompatible( const TypeDecl::Data & data, const Type * type ) {
		switch ( data.kind ) {
		  case TypeDecl::Dtype:
			// to bind to an object type variable, the type must not be a function type.
			// if the type variable is specified to be a complete type then the incoming
			// type must also be complete
			// xxx - should this also check that type is not a tuple type and that it's not a ttype?
			return ! isFtype( type ) && (! data.isComplete || type->isComplete() );
		  case TypeDecl::Ftype:
			return isFtype( type );
		  case TypeDecl::Ttype:
			// ttype unifies with any tuple type
			return dynamic_cast< const TupleType * >( type ) || Tuples::isTtype( type );
		  default:
			assertf(false, "Unhandled tyvar kind: %d", data.kind);
		} // switch
		return false;
	}

	bool TypeEnvironment::bindVar( const TypeInstType *typeInst, Type *bindTo, const TypeDecl::Data & data, AssertionSet &need, AssertionSet &have, const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer ) {

		// remove references from other, so that type variables can only bind to value types
		bindTo = bindTo->stripReferences();
		OpenVarSet::const_iterator tyvar = openVars.find( typeInst->get_name() );
		assert( tyvar != openVars.end() );
		if ( ! tyVarCompatible( tyvar->second, bindTo ) ) {
			return false;
		} // if
		if ( occurs( bindTo, typeInst->get_name(), *this ) ) {
			return false;
		} // if
		auto curClass = internal_lookup( typeInst->get_name() );
		if ( curClass != env.end() ) {
			if ( curClass->type ) {
				Type *common = 0;
				// attempt to unify equivalence class type (which has qualifiers stripped, so they must be restored) with the type to bind to
				std::unique_ptr< Type > newType( curClass->type->clone() );
				newType->tq = typeInst->tq;
				if ( unifyInexact( newType.get(), bindTo, *this, need, have, openVars, widen & WidenMode( curClass->allowWidening, true ), indexer, common ) ) {
					if ( common ) {
						common->get_qualifiers() = Type::Qualifiers{};
						curClass->set_type( common );
					} // if
				} else return false;
			} else {
				Type* newType = bindTo->clone();
				newType->get_qualifiers() = Type::Qualifiers{};
				curClass->set_type( newType );
				curClass->allowWidening = widen.first && widen.second;
			} // if
		} else {
			EqvClass newClass;
			newClass.vars.insert( typeInst->get_name() );
			newClass.type = bindTo->clone();
			newClass.type->get_qualifiers() = Type::Qualifiers();
			newClass.allowWidening = widen.first && widen.second;
			newClass.data = data;
			env.push_back( std::move(newClass) );
		} // if
		return true;
	}

	bool TypeEnvironment::bindVarToVar( const TypeInstType * var1, const TypeInstType * var2,
			TypeDecl::Data && data, AssertionSet &need, AssertionSet &have,
			const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer ) {

		auto class1 = internal_lookup( var1->get_name() );
		auto class2 = internal_lookup( var2->get_name() );

		// exit early if variables already bound together
		if ( class1 != env.end() && class1 == class2 ) {
			class1->allowWidening &= widen;
			return true;
		}

		bool widen1 = false, widen2 = false;
		const Type *type1 = nullptr, *type2 = nullptr;

		// check for existing bindings, perform occurs check
		if ( class1 != env.end() ) {
			if ( class1->type ) {
				if ( occurs( class1->type, var2->get_name(), *this ) ) return false;
				type1 = class1->type;
			} // if
			widen1 = widen.first && class1->allowWidening;
		} // if
		if ( class2 != env.end() ) {
			if ( class2->type ) {
				if ( occurs( class2->type, var1->get_name(), *this ) ) return false;
				type2 = class2->type;
			} // if
			widen2 = widen.second && class2->allowWidening;
		} // if

		if ( type1 && type2 ) {
			// both classes bound, merge if bound types can be unified
			std::unique_ptr<Type> newType1{ type1->clone() }, newType2{ type2->clone() };
			WidenMode newWidenMode{ widen1, widen2 };
			Type *common = 0;
			if ( unifyInexact( newType1.get(), newType2.get(), *this, need, have, openVars, newWidenMode, indexer, common ) ) {
				class1->vars.insert( class2->vars.begin(), class2->vars.end() );
				class1->allowWidening = widen1 && widen2;
				if ( common ) {
					common->get_qualifiers() = Type::Qualifiers{};
					class1->set_type( common );
				}
				class1->data.isComplete |= data.isComplete;
				env.erase( class2 );
			} else return false;
		} else if ( class1 != env.end() && class2 != env.end() ) {
			// both classes exist, at least one unbound, merge unconditionally
			if ( type1 ) {
				class1->vars.insert( class2->vars.begin(), class2->vars.end() );
				class1->allowWidening = widen1;
				class1->data.isComplete |= data.isComplete;
				env.erase( class2 );
			} else {
				class2->vars.insert( class1->vars.begin(), class1->vars.end() );
				class2->allowWidening = widen2;
				class2->data.isComplete |= data.isComplete;
				env.erase( class1 );
			} // if
		} else if ( class1 != env.end() ) {
			// var2 unbound, add to class1
			class1->vars.insert( var2->get_name() );
			class1->allowWidening = widen1;
			class1->data.isComplete |= data.isComplete;
		} else if ( class2 != env.end() ) {
			// var1 unbound, add to class2
			class2->vars.insert( var1->get_name() );
			class2->allowWidening = widen2;
			class2->data.isComplete |= data.isComplete;
		} else {
			// neither var bound, create new class
			EqvClass newClass;
			newClass.vars.insert( var1->get_name() );
			newClass.vars.insert( var2->get_name() );
			newClass.allowWidening = widen1 && widen2;
			newClass.data = data;
			env.push_back( std::move(newClass) );
		} // if
		return true;
	}

	void TypeEnvironment::forbidWidening() {
		for ( EqvClass& c : env ) c.allowWidening = false;
	}

	std::ostream & operator<<( std::ostream & out, const TypeEnvironment & env ) {
		env.print( out );
		return out;
	}
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
