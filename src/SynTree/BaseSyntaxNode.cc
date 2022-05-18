//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// BaseSyntaxNode.cc --
//
// Author           : Andrew Beach
// Created On       : Fri May 13 13:58:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Fri May 13 14:01:00 2022
// Update Count     : 0
//

#include "BaseSyntaxNode.h"

std::ostream & operator<<( std::ostream & out, const BaseSyntaxNode * node ) {
    if ( node ) {
        node->print( out );
    } else {
        out << "nullptr";
    }
    return out;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
