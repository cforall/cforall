//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Lvalue2.cc -- Seperate Lvalue module for linking.
//
// Author           : Andrew Beach
// Created On       : Mon May 16 14:05:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon May 16 14:05:00 2022
// Update Count     : 0
//

namespace GenPoly {

bool referencesEliminated = false;

/// Are reference types still allowed in the AST?
bool referencesPermissable() {
	return !referencesEliminated;
}

}
