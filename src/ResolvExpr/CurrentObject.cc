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
// Last Modified On : Fri Jul  1 09:16:01 2022
// Update Count     : 15
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
#include "SynTree/Constant.h"          // for Constant
#include "SynTree/Declaration.h"       // for ObjectDecl, Declaration, Struc...
#include "SynTree/Expression.h"        // for InitAlternative, VariableExpr
#include "SynTree/Initializer.h"       // for Designation, operator<<
#include "SynTree/Type.h"              // for Type, StructInstType, UnionIns...
#include "SynTree/TypeSubstitution.h"  // for TypeSubstitution

#if 0
#define PRINT(x) x
#else
#define PRINT(x)
#endif

namespace ResolvExpr {
	template< typename AggrInst >
	TypeSubstitution makeGenericSubstitution( AggrInst * inst ) {
		assert( inst );
		assert( inst->get_baseParameters() );
		std::list< TypeDecl * > baseParams = *inst->get_baseParameters();
		std::list< Expression * > typeSubs = inst->get_parameters();
		TypeSubstitution subs( baseParams.begin(), baseParams.end(), typeSubs.begin() );
		return subs;
	}

	TypeSubstitution makeGenericSubstitution( Type * type ) {
		if ( StructInstType * inst = dynamic_cast< StructInstType * >( type ) ) {
			return makeGenericSubstitution( inst );
		} else if ( UnionInstType * inst = dynamic_cast< UnionInstType * >( type ) ) {
			return makeGenericSubstitution( inst );
		} else {
			return TypeSubstitution();
		}
	}

	class MemberIterator {
	public:
		virtual ~MemberIterator() {}

		/// walks the current object using the given designators as a guide
		virtual void setPosition( std::list< Expression * > & designators ) = 0;

		/// retrieve the list of possible Type/Designation pairs for the current position in the currect object
		virtual std::list<InitAlternative> operator*() const = 0;

		/// true if the iterator is not currently at the end
		virtual operator bool() const = 0;

		/// moves the iterator by one member in the current object
		virtual MemberIterator & bigStep() = 0;

		/// moves the iterator by one member in the current subobject
		virtual MemberIterator & smallStep() = 0;

		/// the type of the current object
		virtual Type * getType() = 0;

		/// the type of the current subobject
		virtual Type * getNext() = 0;

		/// printing for debug
		virtual void print( std::ostream & out, Indenter indent ) const = 0;

		/// helper for operator*; aggregates must add designator to each init alternative, but
		/// adding designators in operator* creates duplicates.
		virtual std::list<InitAlternative> first() const = 0; // should be protected
	};

	std::ostream & operator<<(std::ostream & out, const MemberIterator & it) {
		Indenter indenter;
		it.print( out, indenter );
		return out;
	}

	/// create a new MemberIterator that traverses a type correctly
	MemberIterator * createMemberIterator( Type * type );

	/// iterates "other" types, e.g. basic types, pointer types, etc. which do not change at list initializer entry
	class SimpleIterator : public MemberIterator {
	public:
		SimpleIterator( Type * type ) : type( type ) {}

		virtual void setPosition( std::list< Expression * > & designators ) {
			assertf( designators.empty(), "simple iterator given non-empty designator..." ); // xxx - might be semantic error
		}

		virtual std::list<InitAlternative> operator*() const { return first(); }
		virtual operator bool() const { return type; }

		// big step is the same as small step
		virtual MemberIterator & bigStep() { return smallStep(); }
		virtual MemberIterator & smallStep() {
			type = nullptr;  // type is nullified on increment since SimpleIterators do not have members
			return *this;
		}

		virtual void print( std::ostream & out, __attribute__((unused)) Indenter indent ) const {
			out << "SimpleIterator(" << type << ")";
		}

		virtual Type * getType() { return type; }
		virtual Type * getNext() { return type; }

	protected:
		virtual std::list<InitAlternative> first() const {
			if ( type ) return std::list<InitAlternative>{ { type->clone(), new Designation( {} ) } };
			else return std::list<InitAlternative>{};
		}
	private:
		Type * type = nullptr;
	};

	class ArrayIterator : public MemberIterator {
	public:
		ArrayIterator( ArrayType * at ) : array( at ) {
			PRINT( std::cerr << "Creating array iterator: " << at << std::endl; )
			base = at->base;
			memberIter = createMemberIterator( base );
			if ( at->isVarLen ) SemanticError( at, "VLA initialization does not support @=: " );
			setSize( at->dimension );
		}

		~ArrayIterator() {
			delete memberIter;
		}

	private:
		void setSize( Expression * expr ) {
			auto res = eval( expr );
			if (res.second) {
				size = res.first;
			} else {
				SemanticError( expr->location, toString("Array designator must be a constant expression: ", expr) );
			}
		}

	public:
		void setPosition( Expression * expr ) {
			// need to permit integer-constant-expressions, including: integer constants, enumeration constants, character constants, sizeof expressions, _Alignof expressions, cast expressions
			auto arg = eval( expr );
			index = arg.first;
			return;

			// if ( ConstantExpr * constExpr = dynamic_cast< ConstantExpr * >( expr ) ) {
			// 	try {
			// 		index = constExpr->intValue();
			// 	} catch( SemanticErrorException & ) {
			// 		SemanticError( expr, "Constant expression of non-integral type in array designator: " );
			// 	}
			// } else if ( CastExpr * castExpr = dynamic_cast< CastExpr * >( expr ) ) {
			// 	setPosition( castExpr->get_arg() );
			// } else if ( VariableExpr * varExpr = dynamic_cast< VariableExpr * >( expr ) ) {
			// 	EnumInstType * inst = dynamic_cast<EnumInstType *>( varExpr->get_result() );
			// 	assertf( inst, "ArrayIterator given variable that isn't an enum constant : %s", toString( expr ).c_str() );
			// 	long long int value;
			// 	if ( inst->baseEnum->valueOf( varExpr->var, value ) ) {
			// 		index = value;
			// 	}
			// } else if ( dynamic_cast< SizeofExpr * >( expr ) || dynamic_cast< AlignofExpr * >( expr ) ) {
			// 	index = 0; // xxx - get actual sizeof/alignof value?
			// } else {
			// 	assertf( false, "4 bad designator given to ArrayIterator: %s", toString( expr ).c_str() );
			// }
		}

		virtual void setPosition( std::list< Expression * > & designators ) {
			if ( ! designators.empty() ) {
				setPosition( designators.front() );
				designators.pop_front();
				memberIter->setPosition( designators );
			}
		}

		virtual std::list<InitAlternative> operator*() const {
			return first();
		}

		virtual operator bool() const { return index < size; }

		virtual MemberIterator & bigStep() {
			PRINT( std::cerr << "bigStep in ArrayIterator (" << index << "/" << size << ")" << std::endl; )
			++index;
			delete memberIter;
			if ( index < size ) memberIter = createMemberIterator( base );
			else memberIter = nullptr;
			return *this;
		}

		virtual MemberIterator & smallStep() {
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

		virtual Type * getType() { return array; }
		virtual Type * getNext() { return base; }

		virtual std::list<InitAlternative> first() const {
			PRINT( std::cerr << "first in ArrayIterator (" << index << "/" << size << ")" << std::endl; )
			if ( memberIter && *memberIter ) {
				std::list<InitAlternative> ret = memberIter->first();
				for ( InitAlternative & alt : ret ) {
					alt.designation->get_designators().push_front( new ConstantExpr( Constant::from_ulong( index ) ) );
				}
				return ret;
			}
			return std::list<InitAlternative>();
		}

		virtual void print( std::ostream & out, Indenter indent ) const {
			out << "ArrayIterator(Array of " << base << ")";
			if ( memberIter ) {
				Indenter childIndent = indent+1;
				out << std::endl << childIndent;
				memberIter->print( out, childIndent );
			}
		}

	private:
		ArrayType * array = nullptr;
		Type * base = nullptr;
		size_t index = 0;
		size_t size = 0;
		MemberIterator * memberIter = nullptr;
	};

	class AggregateIterator : public MemberIterator {
	public:
		typedef std::list<Declaration *> MemberList;
		typedef MemberList::const_iterator iterator;
		std::string kind = ""; // for debug
		std::string name;
		Type * inst = nullptr;
		const MemberList & members;
		iterator curMember;
		bool atbegin = true; // false at first {small,big}Step -- this aggr type is only added to the possibilities at the beginning
		Type * curType = nullptr;
		MemberIterator * memberIter = nullptr;
		mutable TypeSubstitution sub;

		AggregateIterator( const std::string & kind, const std::string & name, Type * inst, const MemberList & members ) : kind( kind ), name( name ), inst( inst ), members( members ), curMember( members.begin() ), sub( makeGenericSubstitution( inst ) ) {
			PRINT( std::cerr << "Creating " << kind << "(" << name << ")"; )
			init();
		}

		virtual ~AggregateIterator() {
			delete memberIter;
		}

		bool init() {
			PRINT( std::cerr << "--init()--" << members.size() << std::endl; )
			if ( curMember != members.end() ) {
				if ( ObjectDecl * field = dynamic_cast< ObjectDecl * >( *curMember ) ) {
					PRINT( std::cerr << "incremented to field: " << field << std::endl; )
					curType = field->get_type();
					memberIter = createMemberIterator( curType );
					return true;
				}
			}
			return false;
		}

		virtual std::list<InitAlternative> operator*() const {
			if (memberIter && *memberIter) {
				std::list<InitAlternative> ret = memberIter->first();
				PRINT( std::cerr << "sub: " << sub << std::endl; )
				for ( InitAlternative & alt : ret ) {
					PRINT( std::cerr << "iterating and adding designators" << std::endl; )
					alt.designation->get_designators().push_front( new VariableExpr( strict_dynamic_cast< ObjectDecl * >( *curMember ) ) );
					// need to substitute for generic types, so that casts are to concrete types
					PRINT( std::cerr << "  type is: " << alt.type; )
					sub.apply( alt.type ); // also apply to designation??
					PRINT( std::cerr << " ==> " << alt.type << std::endl; )
				}
				return ret;
			}
			return std::list<InitAlternative>();
		}

		virtual void setPosition( std::list< Expression * > & designators ) {
			if ( ! designators.empty() ) {
				if ( VariableExpr * varExpr = dynamic_cast< VariableExpr * >( designators.front() ) ) {
					for ( curMember = members.begin(); curMember != members.end(); ++curMember ) {
						if ( *curMember == varExpr->get_var() ) {
							designators.pop_front();
							delete memberIter;
							memberIter = createMemberIterator( varExpr->get_result() );
							curType = varExpr->get_result();
							atbegin = curMember == members.begin() && designators.empty(); // xxx - is this the right condition for atbegin??
							memberIter->setPosition( designators );
							return;
						} // if
					} // for
					assertf( false, "could not find member in %s: %s", kind.c_str(), toString( varExpr ).c_str() );
				} else {
					assertf( false, "3 bad designator given to %s: %s", kind.c_str(), toString( designators.front() ).c_str() );
				} // if
			} // if
		}

		virtual MemberIterator & smallStep() {
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

		virtual Type * getType() { return inst; }
		virtual Type * getNext() {
			if ( memberIter && *memberIter ) return memberIter->getType(); // xxx - ??? recursive call???
			return nullptr;
		}

		virtual std::list<InitAlternative> first() const {
			std::list<InitAlternative> ret;
			PRINT( std::cerr << "first " << kind << std::endl; )
			if ( memberIter && *memberIter ) { // might not need *memberIter??
				PRINT( std::cerr << "adding children" << std::endl; )
				ret = memberIter->first();
				for ( InitAlternative & alt : ret ) {
					PRINT( std::cerr << "iterating and adding designators" << std::endl; )
					alt.designation->get_designators().push_front( new VariableExpr( strict_dynamic_cast< ObjectDecl * >( *curMember ) ) );
				}
			}
			if ( atbegin ) {
				// xxx - what about case of empty struct??
				// only add self if at the very beginning of the structure
				PRINT( std::cerr << "adding self" << std::endl; )
				ret.push_front( { inst->clone(), new Designation( {} ) } );
			}
			return ret;
		}

		virtual void print( std::ostream & out, Indenter indent ) const {
			out << kind << "(" << name << ")";
			if ( memberIter ) {
				Indenter childIndent = indent+1;
				out << std::endl << childIndent;
				memberIter->print( out, childIndent );
			}
		}
	};

	class UnionIterator : public AggregateIterator {
	public:
		UnionIterator( UnionInstType * inst ) : AggregateIterator( "UnionIterator", inst->get_name(), inst, inst->get_baseUnion()->get_members() ) {}

		virtual operator bool() const { return (memberIter && *memberIter); }
		virtual MemberIterator & bigStep() {
			// unions only initialize one member
			PRINT( std::cerr << "bigStep in " << kind << std::endl; )
			atbegin = false;
			delete memberIter;
			memberIter = nullptr;
			curType = nullptr;
			curMember = members.end();
			return *this;
		}
	};

	class StructIterator : public AggregateIterator {
	public:
		StructIterator( StructInstType * inst ) : AggregateIterator( "StructIterator", inst->get_name(), inst, inst->get_baseStruct()->get_members() ) {}

		virtual operator bool() const { return curMember != members.end() || (memberIter && *memberIter); }

		virtual MemberIterator & bigStep() {
			PRINT( std::cerr << "bigStep in " << kind << std::endl; )
			atbegin = false;
			delete memberIter;
			memberIter = nullptr;
			curType = nullptr;
			for ( ; curMember != members.end(); ) {
				++curMember;
				if ( init() ) {
					return *this;
				}
			}
			return *this;
		}
	};

	class TupleIterator : public AggregateIterator {
	public:
		TupleIterator( TupleType * inst ) : AggregateIterator( "TupleIterator", toString("Tuple", inst->size()), inst, inst->get_members() ) {}

		virtual operator bool() const { return curMember != members.end() || (memberIter && *memberIter); }

		virtual MemberIterator & bigStep() {
			PRINT( std::cerr << "bigStep in " << kind << std::endl; )
			atbegin = false;
			delete memberIter;
			memberIter = nullptr;
			curType = nullptr;
			for ( ; curMember != members.end(); ) {
				++curMember;
				if ( init() ) {
					return *this;
				}
			}
			return *this;
		}
	};

	MemberIterator * createMemberIterator( Type * type ) {
		if ( ReferenceToType * aggr = dynamic_cast< ReferenceToType * >( type ) ) {
			if ( StructInstType * sit = dynamic_cast< StructInstType * >( aggr ) ) {
				return new StructIterator( sit );
			} else if ( UnionInstType * uit = dynamic_cast< UnionInstType * >( aggr ) ) {
				return new UnionIterator( uit );
			} else {
				assertf( dynamic_cast< EnumInstType * >( type ) || dynamic_cast< TypeInstType * >( type ), "Encountered unhandled ReferenceToType in createMemberIterator: %s", toString( type ).c_str() );
				return new SimpleIterator( type );
			}
		} else if ( ArrayType * at = dynamic_cast< ArrayType * >( type ) ) {
			return new ArrayIterator( at );
		} else if ( TupleType * tt = dynamic_cast< TupleType * >( type ) ) {
			return new TupleIterator( tt );
		} else {
			return new SimpleIterator( type );
		}
	}

	CurrentObject::CurrentObject() {}
	CurrentObject::CurrentObject( Type * type ) {
		objStack.push( new SimpleIterator( type ) );
	}


	void CurrentObject::setNext( Designation * designation ) {
		assertf( ! objStack.empty(), "obj stack empty in setNext" );
		PRINT( std::cerr << "____setNext" << designation << std::endl; )
		objStack.top()->setPosition( designation->get_designators() );
	}

	Designation * CurrentObject::findNext( Designation * designation ) {
		typedef std::list< Expression * > DesignatorChain;
		PRINT( std::cerr << "___findNext" << std::endl; )
		// find all the d's
		std::list<DesignatorChain> desigAlts{ { } }, newDesigAlts;
		std::list<Type *> curTypes { (objStack.top())->getType() }, newTypes;
		for ( Expression * expr : designation->get_designators() ) {
			PRINT( std::cerr << "____untyped: " << expr << std::endl; )
			std::list<DesignatorChain>::iterator dit = desigAlts.begin();
			if ( NameExpr * nexpr = dynamic_cast<NameExpr *>(expr) ) {
				for ( Type * t : curTypes ) {
					assert( dit != desigAlts.end() );
					DesignatorChain & d = *dit;
					PRINT( std::cerr << "____actual: " << t << std::endl; )
					ReferenceToType * refType = dynamic_cast<ReferenceToType *>(t);
					std::list<Declaration *> members;
					if ( refType ) {
						refType->lookup( nexpr->get_name(), members ); // concatenate identical field name
						// xxx - need to also include anonymous members in this somehow...
						for ( Declaration * mem: members ) {
							if ( ObjectDecl * field = dynamic_cast<ObjectDecl *>(mem) ) {
								PRINT( std::cerr << "____alt: " << field->get_type() << std::endl; )
								DesignatorChain newD = d;
								newD.push_back( new VariableExpr( field ) );
								newDesigAlts.push_back( newD );
								newTypes.push_back( field->get_type() );
							} // if
						} // for
					} // if
					++dit;
				} // for
			} else {
				for ( Type * t : curTypes ) {
					assert( dit != desigAlts.end() );
					DesignatorChain & d = *dit;
					if ( ArrayType * at = dynamic_cast< ArrayType * > ( t ) ) {
						PRINT( std::cerr << "____alt: " << at->get_base() << std::endl; )
						d.push_back( expr );
						newDesigAlts.push_back( d );
						newTypes.push_back( at->get_base() );
					}
					++dit;
				} // for
			} // if
			desigAlts = newDesigAlts;
			newDesigAlts.clear();
			curTypes = newTypes;
			newTypes.clear();
			assertf( desigAlts.size() == curTypes.size(), "Designator alternatives (%zu) and current types (%zu) out of sync", desigAlts.size(), curTypes.size() );
		} // for
		if ( desigAlts.size() > 1 ) {
			SemanticError( designation, toString("Too many alternatives (", desigAlts.size(), ") for designation: ") );
		} else if ( desigAlts.size() == 0 ) {
			SemanticError( designation, "No reasonable alternatives for designation: " );
		}
		DesignatorChain & d = desigAlts.back();
		PRINT( for ( Expression * expr : d ) {
			std::cerr << "____desig: " << expr << std::endl;
		} ) // for
		assertf( ! curTypes.empty(), "empty designator chosen");

		// set new designators
		assertf( ! objStack.empty(), "empty object stack when setting designation" );
		Designation * actualDesignation = new Designation( d );
		objStack.top()->setPosition( d ); // destroys d
		return actualDesignation;
	}

	void CurrentObject::increment() {
		PRINT( std::cerr << "____increment" << std::endl; )
		if ( ! objStack.empty() ) {
			PRINT( std::cerr << *objStack.top() << std::endl; )
			objStack.top()->smallStep();
		}
	}

	void CurrentObject::enterListInit() {
		PRINT( std::cerr << "____entering list init" << std::endl; )
		assertf( ! objStack.empty(), "empty obj stack entering list init" );
		Type * type = objStack.top()->getNext();
		if ( type ) {
			objStack.push( createMemberIterator( type ) );
		} else {
			assertf( false, "not sure about this case..." );
		}
	}

	void CurrentObject::exitListInit() {
		PRINT( std::cerr << "____exiting list init" << std::endl; )
		assertf( ! objStack.empty(), "objstack empty" );
		delete objStack.top();
		objStack.pop();
		if ( ! objStack.empty() ) {
			PRINT( std::cerr << *objStack.top() << std::endl; )
			objStack.top()->bigStep();
		}
	}

	std::list< InitAlternative > CurrentObject::getOptions() {
		PRINT( std::cerr << "____getting current options" << std::endl; )
		assertf( ! objStack.empty(), "objstack empty in getOptions" );
		return **objStack.top();
	}

	Type * CurrentObject::getCurrentType() {
		PRINT( std::cerr << "____getting current type" << std::endl; )
		assertf( ! objStack.empty(), "objstack empty in getCurrentType" );
		return objStack.top()->getNext();
	}
} // namespace ResolvExpr

namespace ast {
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

	/// Iterates array types
	class ArrayIterator final : public MemberIterator {
		CodeLocation location;
		const ArrayType * array = nullptr;
		const Type * base = nullptr;
		size_t index = 0;
		size_t size = 0;
		std::unique_ptr< MemberIterator > memberIter;

		void setSize( const Expr * expr ) {
			auto res = eval( expr );
			if ( ! res.second ) {
				SemanticError( location, toString( "Array designator must be a constant expression: ", expr ) );
			}
			size = res.first;
		}

	public:
		ArrayIterator( const CodeLocation & loc, const ArrayType * at ) : location( loc ), array( at ), base( at->base ) {
			PRINT( std::cerr << "Creating array iterator: " << at << std::endl; )
			memberIter.reset( createMemberIterator( loc, base ) );
			if ( at->isVarLen ) {
				SemanticError( location, at, "VLA initialization does not support @=: " );
			}
			setSize( at->dimension );
		}

		void setPosition( const Expr * expr ) {
			// need to permit integer-constant-expressions, including: integer constants,
			// enumeration constants, character constants, sizeof expressions, alignof expressions,
			// cast expressions

			auto arg = eval( expr );
			index = arg.first;
			return;

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
			std::deque< ptr< Expr > >::const_iterator begin,
			std::deque< ptr< Expr > >::const_iterator end
		) override {
			if ( begin == end ) return;

			setPosition( *begin );
			memberIter->setPosition( ++begin, end );
		}

		std::deque< InitAlternative > operator* () const override { return first(); }

		operator bool() const override { return index < size; }

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
			return ( memberIter && *memberIter ) ? memberIter->getType() : nullptr;
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

	class TupleIterator final : public AggregateIterator {
	public:
		TupleIterator( const CodeLocation & loc, const TupleType * inst )
		: AggregateIterator(
			loc, "TupleIterator", toString("Tuple", inst->size()), inst, inst->members
		) {}

		operator bool() const override {
			return curMember != members.end() || (memberIter && *memberIter);
		}

		TupleIterator & bigStep() override {
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
			if ( auto nexpr = dynamic_cast< const NameExpr * >( expr ) ) {
				for ( const Type * t : curTypes ) {
					assert( dit != desigAlts.end() );

					DesignatorChain & d = *dit;
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
				}
			} else {
				for ( const Type * t : curTypes ) {
					assert( dit != desigAlts.end() );

					DesignatorChain & d = *dit;
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
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
