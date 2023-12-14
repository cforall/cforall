//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CurrentObject.h --
//
// Author           : Rob Schluntz
// Created On       : Tue Jun 13 15:28:32 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Dec  9 17:49:51 2023
// Update Count     : 20
//

#include <stddef.h>                    // for size_t
#include <cassert>                     // for assertf, assert, safe_dynamic_...
#include <deque>
#include <iostream>                    // for ostream, operator<<, basic_ost...
#include <stack>                       // for stack
#include <string>                      // for string, operator<<, allocator

#include "AST/Copy.hpp"                // for shallowCopy
#include "AST/Expr.hpp"                // for InitAlternative
#include "AST/GenericSubstitution.hpp" // for genericSubstitution
#include "AST/Init.hpp"                // for Designation
#include "AST/Node.hpp"                // for readonly
#include "AST/Print.hpp"               // for readonly
#include "AST/Type.hpp"
#include "Common/Eval.h"               // for eval
#include "Common/Indenter.h"           // for Indenter, operator<<
#include "Common/SemanticError.h"      // for SemanticError
#include "Common/utility.h"            // for toString
#include "CurrentObject.h"

#if 0
#define PRINT(x) x
#else
#define PRINT(x)
#endif

namespace ast {

/// Iterates members of a type by initializer.
class MemberIterator {
public:
	virtual ~MemberIterator() {}

	/// Internal set position based on iterator ranges.
	virtual void setPosition(
		std::deque< ptr< Expr > >::const_iterator it,
		std::deque< ptr< Expr > >::const_iterator end ) = 0;

	/// Walks the current object using the given designators as a guide.
	void setPosition( const std::deque< ptr< Expr > > & designators ) {
		setPosition( designators.begin(), designators.end() );
	}

	/// Retrieve the list of possible (Type,Designation) pairs for the
	/// current position in the current object.
	virtual std::deque< InitAlternative > operator* () const = 0;

	/// True if the iterator is not currently at the end.
	virtual operator bool() const = 0;

	/// Moves the iterator by one member in the current object.
	virtual MemberIterator & bigStep() = 0;

	/// Moves the iterator by one member in the current subobject.
	virtual MemberIterator & smallStep() = 0;

	/// The type of the current object.
	virtual const Type * getType() = 0;

	/// The type of the current subobject.
	virtual const Type * getNext() = 0;

	/// Helper for operator*; aggregates must add designator to each init
	/// alternative, but adding designators in operator* creates duplicates.
	virtual std::deque< InitAlternative > first() const = 0;
};

namespace {

/// create a new MemberIterator that traverses a type correctly
MemberIterator * createMemberIterator( const CodeLocation & loc, const Type * type );

/// Iterates "other" types (e.g. basic, pointer) which do not change at list initializer entry
class SimpleIterator final : public MemberIterator {
	CodeLocation location;
	const Type * type = nullptr;
public:
	SimpleIterator( const CodeLocation & loc, const Type * t ) : location( loc ), type( t ) {}

	void setPosition(
		std::deque< ptr< Expr > >::const_iterator begin,
		std::deque< ptr< Expr > >::const_iterator end
	) override {
		if ( begin != end ) {
			SemanticError( location, "Un-designated initializer given non-empty designator" );
		}
	}

	std::deque< InitAlternative > operator* () const override { return first(); }

	operator bool() const override { return type; }

	SimpleIterator & bigStep() override { return smallStep(); }
	SimpleIterator & smallStep() override {
		type = nullptr;  // empty on increment because no members
		return *this;
	}

	const Type * getType() override { return type; }

	const Type * getNext() override { return type; }

	std::deque< InitAlternative > first() const override {
		if ( type ) return { InitAlternative{ type, new Designation{ location } } };
		return {};
	}
};

/// Iterates over an indexed type:
class IndexIterator : public MemberIterator {
protected:
	CodeLocation location;
	size_t index = 0;
	size_t size = 0;
	std::unique_ptr<MemberIterator> memberIter;
public:
	IndexIterator( const CodeLocation & loc, size_t size ) :
		location( loc ), size( size )
	{}

	void setPosition( const Expr * expr ) {
		// need to permit integer-constant-expressions, including: integer constants,
		// enumeration constants, character constants, sizeof expressions, alignof expressions,
		// cast expressions

		auto arg = eval( expr );
		assertf( arg.hasKnownValue, "Non-evaluable expression made it to IndexIterator" );
		index = arg.knownValue;

		// if ( auto constExpr = dynamic_cast< const ConstantExpr * >( expr ) ) {
		// 	try {
		// 		index = constExpr->intValue();
		// 	} catch ( SemanticErrorException & ) {
		// 		SemanticError( expr, "Constant expression of non-integral type in array designator: " );
		// 	}
		// } else if ( auto castExpr = dynamic_cast< const CastExpr * >( expr ) ) {
		// 	setPosition( castExpr->arg );
		// } else if ( dynamic_cast< const SizeofExpr * >( expr ) || dynamic_cast< const AlignofExpr * >( expr ) ) {
		// 	index = 0;
		// } else {
		// 	assertf( false,	"2 bad designator given to ArrayIterator: %s", toString( expr ).c_str() );
		// }
	}

	void setPosition(
		std::deque<ast::ptr<ast::Expr>>::const_iterator begin,
		std::deque<ast::ptr<ast::Expr>>::const_iterator end
	) override {
		if ( begin == end ) return;

		setPosition( *begin );
		memberIter->setPosition( ++begin, end );
	}

	std::deque< InitAlternative > operator* () const override { return first(); }

	operator bool() const override { return index < size; }
};

/// Iterates over the members of array types:
class ArrayIterator final : public IndexIterator {
	const ArrayType * array = nullptr;
	const Type * base = nullptr;

	size_t getSize( const Expr * expr ) {
		auto res = eval( expr );
		if ( !res.hasKnownValue ) {
			SemanticError( location, "Array designator must be a constant expression %s", toString( expr ).c_str() );
		}
		return res.knownValue;
	}

public:
	ArrayIterator( const CodeLocation & loc, const ArrayType * at ) :
			IndexIterator( loc, getSize( at->dimension) ),
			array( at ), base( at->base ) {
		PRINT( std::cerr << "Creating array iterator: " << at << std::endl; )
		memberIter.reset( createMemberIterator( loc, base ) );
		if ( at->isVarLen ) {
			SemanticError( location, at, "VLA initialization does not support @=: " );
		}
	}

	ArrayIterator & bigStep() override {
		PRINT( std::cerr << "bigStep in ArrayIterator (" << index << "/" << size << ")" << std::endl; )
		++index;
		memberIter.reset( index < size ? createMemberIterator( location, base ) : nullptr );
		return *this;
	}

	ArrayIterator & smallStep() override {
		PRINT( std::cerr << "smallStep in ArrayIterator (" << index << "/" << size << ")" << std::endl; )
		if ( memberIter ) {
			PRINT( std::cerr << "has member iter: " << *memberIter << std::endl; )
			memberIter->smallStep();
			if ( *memberIter ) {
				PRINT( std::cerr << "has valid member iter" << std::endl; )
				return *this;
			}
		}
		return bigStep();
	}

	const Type * getType() override { return array; }

	const Type * getNext() override { return base; }

	std::deque< InitAlternative > first() const override {
		PRINT( std::cerr << "first in ArrayIterator (" << index << "/" << size << ")" << std::endl; )
		if ( memberIter && *memberIter ) {
			std::deque< InitAlternative > ret = memberIter->first();
			for ( InitAlternative & alt : ret ) {
				alt.designation.get_and_mutate()->designators.emplace_front( ConstantExpr::from_ulong( location, index ) );
			}
			return ret;
		}
		return {};
	}
};

class AggregateIterator : public MemberIterator {
protected:
	using MemberList = std::vector< ptr< Decl > >;

	CodeLocation location;
	std::string kind;  // for debug
	std::string name;
	const Type * inst;
	const MemberList & members;
	MemberList::const_iterator curMember;
	bool atbegin = true;  // false at first {small,big}Step
	const Type * curType = nullptr;
	std::unique_ptr< MemberIterator > memberIter = nullptr;
	TypeSubstitution sub;

	bool init() {
		PRINT( std::cerr << "--init()--" << members.size() << std::endl; )
		if ( curMember != members.end() ) {
			if ( auto field = curMember->as< ObjectDecl >() ) {
				PRINT( std::cerr << "incremented to field: " << field << std::endl; )
				curType = field->get_type();
				memberIter.reset( createMemberIterator( location, curType ) );
				return true;
			}
		}
		return false;
	}

	AggregateIterator(
		const CodeLocation & loc, const std::string k, const std::string & n, const Type * i,
		const MemberList & ms )
	: location( loc ), kind( k ), name( n ), inst( i ), members( ms ), curMember( ms.begin() ),
	  sub( genericSubstitution( i ) ) {
		PRINT( std::cerr << "Creating " << kind << "(" << name << ")"; )
		init();
	}

public:
	void setPosition(
		std::deque< ptr< Expr > >::const_iterator begin,
		std::deque< ptr< Expr > >::const_iterator end
	) final {
		if ( begin == end ) return;

		if ( auto varExpr = begin->as< VariableExpr >() ) {
			for ( curMember = members.begin(); curMember != members.end(); ++curMember ) {
				if ( *curMember != varExpr->var ) continue;

				++begin;

				memberIter.reset( createMemberIterator( location, varExpr->result ) );
				curType = varExpr->result;
				atbegin = curMember == members.begin() && begin == end;
				memberIter->setPosition( begin, end );
				return;
			}
			assertf( false, "could not find member in %s: %s", kind.c_str(), toString( varExpr ).c_str() );
		} else {
			assertf( false, "1 bad designator given to %s: %s", kind.c_str(), toString( *begin ).c_str() );
		}
	}

	std::deque< InitAlternative > operator* () const final {
		if ( memberIter && *memberIter ) {
			std::deque< InitAlternative > ret = memberIter->first();
			PRINT( std::cerr << "sub: " << sub << std::endl; )
			for ( InitAlternative & alt : ret ) {
				PRINT( std::cerr << "iterating and adding designators" << std::endl; )
				alt.designation.get_and_mutate()->designators.emplace_front(
					new VariableExpr{ location, curMember->strict_as< ObjectDecl >() } );
				// need to substitute for generic types so that casts are to concrete types
				alt.type = shallowCopy(alt.type.get());
				PRINT( std::cerr << "  type is: " << alt.type; )
				sub.apply( alt.type ); // also apply to designation??
				PRINT( std::cerr << " ==> " << alt.type << std::endl; )
			}
			return ret;
		}
		return {};
	}

	AggregateIterator & smallStep() final {
		PRINT( std::cerr << "smallStep in " << kind << std::endl; )
		atbegin = false;
		if ( memberIter ) {
			PRINT( std::cerr << "has member iter, incrementing..." << std::endl; )
			memberIter->smallStep();
			if ( *memberIter ) {
				PRINT( std::cerr << "success!" << std::endl; )
				return *this;
			}
		}
		return bigStep();
	}

	AggregateIterator & bigStep() override = 0;

	const Type * getType() final { return inst; }

	const Type * getNext() final {
		bool hasMember = memberIter && *memberIter;
		return hasMember ? memberIter->getType() : nullptr;
	}

	std::deque< InitAlternative > first() const final {
		std::deque< InitAlternative > ret;
		PRINT( std::cerr << "first " << kind << std::endl; )
		if ( memberIter && *memberIter ) {
			PRINT( std::cerr << "adding children" << std::endl; )
			ret = memberIter->first();
			for ( InitAlternative & alt : ret ) {
				PRINT( std::cerr << "iterating and adding designators" << std::endl; )
				alt.designation.get_and_mutate()->designators.emplace_front(
					new VariableExpr{ location, curMember->strict_as< ObjectDecl >() } );
			}
		}
		if ( atbegin ) {
			// only add self if at the very beginning of the structure
			PRINT( std::cerr << "adding self" << std::endl; )
			ret.emplace_front( inst, new Designation{ location } );
		}
		return ret;
	}
};

class StructIterator final : public AggregateIterator {
public:
	StructIterator( const CodeLocation & loc, const StructInstType * inst )
	: AggregateIterator( loc, "StructIterator", inst->name, inst, inst->base->members ) {}

	operator bool() const override {
		return curMember != members.end() || (memberIter && *memberIter);
	}

	StructIterator & bigStep() override {
		PRINT( std::cerr << "bigStep in " << kind << std::endl; )
		atbegin = false;
		memberIter = nullptr;
		curType = nullptr;
		while ( curMember != members.end() ) {
			++curMember;
			if ( init() ) return *this;
		}
		return *this;
	}
};

class UnionIterator final : public AggregateIterator {
public:
	UnionIterator( const CodeLocation & loc, const UnionInstType * inst )
	: AggregateIterator( loc, "UnionIterator", inst->name, inst, inst->base->members ) {}

	operator bool() const override { return memberIter && *memberIter; }

	UnionIterator & bigStep() override {
		// unions only initialize one member
		PRINT( std::cerr << "bigStep in " << kind << std::endl; )
		atbegin = false;
		memberIter = nullptr;
		curType = nullptr;
		curMember = members.end();
		return *this;
	}
};

/// Iterates across the positions in a tuple:
class TupleIterator final : public IndexIterator {
	ast::TupleType const * const tuple;

	const ast::Type * typeAtIndex() const {
		assert( index < size );
		return tuple->types[ index ].get();
	}

public:
	TupleIterator( const CodeLocation & loc, const TupleType * type )
	: IndexIterator( loc, type->size() ), tuple( type ) {
		PRINT( std::cerr << "Creating tuple iterator: " << type << std::endl; )
		memberIter.reset( createMemberIterator( loc, typeAtIndex() ) );
	}

	TupleIterator & bigStep() override {
		++index;
		memberIter.reset( index < size ?
			createMemberIterator( location, typeAtIndex() ) : nullptr );
		return *this;
	}

	TupleIterator & smallStep() override {
		if ( memberIter ) {
			PRINT( std::cerr << "has member iter: " << *memberIter << std::endl; )
			memberIter->smallStep();
			if ( !memberIter ) {
				PRINT( std::cerr << "has valid member iter" << std::endl; )
				return *this;
			}
		}
		return bigStep();
	}

	const ast::Type * getType() override {
		return tuple;
	}

	const ast::Type * getNext() override {
		bool hasMember = memberIter && *memberIter;
		return hasMember ? memberIter->getType() : nullptr;
	}

	std::deque< InitAlternative > first() const override {
		PRINT( std::cerr << "first in TupleIterator (" << index << "/" << size << ")" << std::endl; )
		if ( memberIter && *memberIter ) {
			std::deque< InitAlternative > ret = memberIter->first();
			for ( InitAlternative & alt : ret ) {
				alt.designation.get_and_mutate()->designators.emplace_front(
					ConstantExpr::from_ulong( location, index ) );
			}
			return ret;
		}
		return {};
	}
};

MemberIterator * createMemberIterator( const CodeLocation & loc, const Type * type ) {
	if ( auto aggr = dynamic_cast< const BaseInstType * >( type ) ) {
		if ( auto sit = dynamic_cast< const StructInstType * >( aggr ) ) {
			assert( sit->base );
			return new StructIterator{ loc, sit };
		} else if ( auto uit = dynamic_cast< const UnionInstType * >( aggr ) ) {
			assert( uit->base );
			return new UnionIterator{ loc, uit };
		} else {
			assertf(
				dynamic_cast< const EnumInstType * >( type )
					|| dynamic_cast< const TypeInstType * >( type ),
				"Encountered unhandled BaseInstType in createMemberIterator: %s",
					toString( type ).c_str() );
			return new SimpleIterator{ loc, type };
		}
	} else if ( auto at = dynamic_cast< const ArrayType * >( type ) ) {
		return new ArrayIterator{ loc, at };
	} else if ( auto tt = dynamic_cast< const TupleType * >( type ) ) {
		return new TupleIterator{ loc, tt };
	} else {
		return new SimpleIterator{ loc, type };
	}
}

} // namespace

CurrentObject::CurrentObject( const CodeLocation & loc, const Type * type ) : objStack() {
	objStack.emplace_back( new SimpleIterator{ loc, type } );
}

const Designation * CurrentObject::findNext( const Designation * designation ) {
	using DesignatorChain = std::deque< ptr< Expr > >;
	PRINT( std::cerr << "___findNext" << std::endl; )

	// find all the d's
	std::vector< DesignatorChain > desigAlts{ {} }, newDesigAlts;
	std::deque< const Type * > curTypes{ objStack.back()->getType() }, newTypes;
	for ( const Expr * expr : designation->designators ) {
		PRINT( std::cerr << "____untyped: " << expr << std::endl; )
		auto dit = desigAlts.begin();
		auto nexpr = dynamic_cast< const NameExpr * >( expr );

		for ( const Type * t : curTypes ) {
			assert( dit != desigAlts.end() );
			DesignatorChain & d = *dit;
			// Name Designation:
			if ( nexpr ) {
				PRINT( std::cerr << "____actual: " << t << std::endl; )
				if ( auto refType = dynamic_cast< const BaseInstType * >( t ) ) {
					// concatenate identical field names
					for ( const Decl * mem : refType->lookup( nexpr->name ) ) {
						if ( auto field = dynamic_cast< const ObjectDecl * >( mem ) ) {
							PRINT( std::cerr << "____alt: " << field->type << std::endl; )
							DesignatorChain d2 = d;
							d2.emplace_back( new VariableExpr{ expr->location, field } );
							newDesigAlts.emplace_back( std::move( d2 ) );
							newTypes.emplace_back( field->type );
						}
					}
				}

				++dit;
			// Index Designation:
			} else {
				if ( auto at = dynamic_cast< const ArrayType * >( t ) ) {
					PRINT( std::cerr << "____alt: " << at->get_base() << std::endl; )
					d.emplace_back( expr );
					newDesigAlts.emplace_back( d );
					newTypes.emplace_back( at->base );
				}
			}
		}

		// reset queue
		desigAlts = std::move( newDesigAlts );
		newDesigAlts.clear();
		curTypes = std::move( newTypes );
		newTypes.clear();
		assertf( desigAlts.size() == curTypes.size(), "Designator alternatives (%zu) and current types (%zu) out of sync", desigAlts.size(), curTypes.size() );
	}

	if ( desigAlts.size() > 1 ) {
		SemanticError( designation, toString("Too many alternatives (", desigAlts.size(), ") for designation: ") );
	} else if ( desigAlts.empty() ) {
		SemanticError( designation, "No reasonable alternatives for designation: " );
	}

	DesignatorChain & d = desigAlts.back();
	PRINT( for ( Expression * expr : d ) {
		std::cerr << "____desig: " << expr << std::endl;
	} ) // for
	assertf( ! curTypes.empty(), "empty designator chosen");

	// set new designators
	assertf( ! objStack.empty(), "empty object stack when setting designation" );
	Designation * actualDesignation =
		new Designation{ designation->location, DesignatorChain{d} };
	objStack.back()->setPosition( d ); // destroys d
	return actualDesignation;
}

void CurrentObject::setNext( const Designation * designation ) {
	PRINT( std::cerr << "____setNext" << designation << std::endl; )
	assertf( ! objStack.empty(), "obj stack empty in setNext" );
	objStack.back()->setPosition( designation->designators );
}

void CurrentObject::increment() {
	PRINT( std::cerr << "____increment" << std::endl; )
	if ( objStack.empty() ) return;
	PRINT( std::cerr << *objStack.back() << std::endl; )
	objStack.back()->smallStep();
}

void CurrentObject::enterListInit( const CodeLocation & loc ) {
	PRINT( std::cerr << "____entering list init" << std::endl; )
	assertf( ! objStack.empty(), "empty obj stack entering list init" );
	const ast::Type * type = objStack.back()->getNext();
	assert( type );
	objStack.emplace_back( createMemberIterator( loc, type ) );
}

void CurrentObject::exitListInit() {
	PRINT( std::cerr << "____exiting list init" << std::endl; )
	assertf( ! objStack.empty(), "objstack empty" );
	objStack.pop_back();
	if ( ! objStack.empty() ) {
		PRINT( std::cerr << *objStack.back() << std::endl; )
		objStack.back()->bigStep();
	}
}

std::deque< InitAlternative > CurrentObject::getOptions() {
	PRINT( std::cerr << "____getting current options" << std::endl; )
	assertf( ! objStack.empty(), "objstack empty in getOptions" );
	return **objStack.back();
}

const Type * CurrentObject::getCurrentType() {
	PRINT( std::cerr << "____getting current type" << std::endl; )
	assertf( ! objStack.empty(), "objstack empty in getCurrentType" );
	return objStack.back()->getNext();
}

} // namespace ast

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
