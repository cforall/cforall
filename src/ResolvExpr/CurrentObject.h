//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CurrentObject.h --
//
// Author           : Rob Schluntz
// Created On       : Thu Jun  8 11:07:25 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:36:48 2017
// Update Count     : 3
//

#pragma once

#include <deque>
#include <list>   // for list
#include <memory> // for unique_ptr
#include <stack>  // for stack
#include <vector>

#include "AST/Node.hpp"  // for ptr
#include "Common/CodeLocation.h"

class Designation;
class Type;
struct InitAlternative;

namespace ResolvExpr {
	class MemberIterator;

	// TODO: memory management of MemberIterators
	class CurrentObject {
	public:
		CurrentObject();
		CurrentObject( Type * type );

		/// resolves unresolved designation
		Designation * findNext( Designation * designation );
		/// sets current position using resolved designation
		void setNext( Designation * designation );
		/// steps to next sub-object of current-object
		void increment();
		/// sets new current-object for the duration of this brace-enclosed initializer-list
		void enterListInit();
		/// restores previous current-object
		void exitListInit();
		/// produces a list of alternatives (Type *, Designation *) for the current sub-object's initializer
		std::list< InitAlternative > getOptions();
		/// produces the type of the current object but no subobjects
		Type * getCurrentType();

	private:
		std::stack< MemberIterator * > objStack;
	};
} // namespace ResolvExpr

namespace ast {
	// AST class types
	class Designation;
	struct InitAlternative;
	class Type;

	/// Iterates members of a type by initializer
	class MemberIterator {
	public:
		virtual ~MemberIterator() {}

		/// Internal set position based on iterator ranges
		virtual void setPosition( 
			std::deque< ptr< Expr > >::const_iterator it, 
			std::deque< ptr< Expr > >::const_iterator end ) = 0;

		/// walks the current object using the given designators as a guide
		void setPosition( const std::deque< ptr< Expr > > & designators ) {
			setPosition( designators.begin(), designators.end() );
		}

		/// retrieve the list of possible (Type,Designation) pairs for the current position in the 
		/// current object
		virtual std::deque< InitAlternative > operator* () const = 0;

		/// true if the iterator is not currently at the end
		virtual operator bool() const = 0;

		/// moves the iterator by one member in the current object
		virtual MemberIterator & bigStep() = 0;

		/// moves the iterator by one member in the current subobject
		virtual MemberIterator & smallStep() = 0;

		/// the type of the current object
		virtual const Type * getType() = 0;

		/// the type of the current subobject
		virtual const Type * getNext() = 0;
	
		/// helper for operator*; aggregates must add designator to each init alternative, but 
		/// adding designators in operator* creates duplicates
		virtual std::deque< InitAlternative > first() const = 0;
	};

	/// Builds initializer lists in resolution
	class CurrentObject final {
		std::vector< std::shared_ptr<MemberIterator> > objStack;
	
	public:
		CurrentObject() = default;
		CurrentObject( const CodeLocation & loc, const Type * type );

		/// resolves unresolved designation
		const Designation * findNext( const Designation * designation );
		/// sets current position using the resolved designation
		void setNext( const ast::Designation * designation );
		/// steps to next sub-object of current object
		void increment();
		/// sets new current object for the duration of this brace-enclosed intializer-list
		void enterListInit( const CodeLocation & loc );
		/// restores previous current object
		void exitListInit();
		/// produces a list of alternatives (Type *, Designation *) for the current sub-object's 
		/// initializer.
		std::deque< InitAlternative > getOptions();
		/// produces the type of the current object but no subobjects
		const Type * getCurrentType();
	};
} // namespace ast

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //

