//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TrackLoc.cc --
//
// Author           : Andrew Beach
// Created On       : Tues May 2 15:46:00 2017
// Last Modified By : Andrew Beach
// Last Modified On : Fri Nov 27 18:00:00 2020
// Update Count     : 1
//

#include "TrackLoc.h"

#include <cstdlib>                   // for exit, EXIT_FAILURE
#include <iostream>                  // for operator<<, ostream, basic_ostream
#include <iterator>                  // for back_inserter, inserter
#include <stack>                     // for stack
#include <string>                    // for operator<<, string
#include <typeindex>                 // for type_index
#include <vector>                    // for vector

#include "Common/PassVisitor.h"      // for PassVisitor
#include "Common/utility.h"          // for CodeLocation
#include "SynTree/BaseSyntaxNode.h"  // for BaseSyntaxNode
#include "SynTree/Mutator.h"         // for mutateAll
#include "SynTree/Visitor.h"         // for acceptAll

class Declaration;

namespace CodeTools {
	class LocationPrinter {
		size_t printLevel;

		CodeLocation *lastNode;

		std::stack< CodeLocation *, std::vector< CodeLocation * > > parents;
	public:
		LocationPrinter(size_t printLevel) :
			printLevel(printLevel), lastNode(nullptr)
		{}

		void print( const std::string& name, BaseSyntaxNode *node) {
			for (size_t i = 0 ; i < parents.size() ; ++i) {
				std::cout << "    ";
			}
			if (2 <= printLevel) {
				std::cout << name << '@';
			}
			std::cout << node->location << std::endl;
		}

		void atNode( BaseSyntaxNode *node ) {
			std::string name = std::type_index(typeid(*node)).name();
			if ( node->location.isUnset() ) {
				if ( !parents.empty() ) {
					node->location = *parents.top();
				}
				else if (nullptr != lastNode) {
					node->location = *lastNode;
				}
				else {
					assertf( false, "Top level node has no CodeLocation %s", toString( node ).c_str() );
				}
			}

			if (0 < printLevel) {
				print( name, node );
			}
			lastNode = &node->location;
		}

		void previsit(BaseSyntaxNode * node) {
			atNode(node);
			parents.push( &node->location );
		}

		void postvisit( __attribute__((unused)) BaseSyntaxNode * node ) {
			parents.pop();
		}

	}; // LocationPrinter

	void fillLocations( std::list< Declaration * > & translationUnit, size_t printLevel) {
		PassVisitor<LocationPrinter> printer(printLevel);
		acceptAll( translationUnit, printer );
	}
} // namespace CodeTools

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
