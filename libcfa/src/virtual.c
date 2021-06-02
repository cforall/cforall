//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// virtual.c --
//
// Author           : Andrew Beach
// Created On       : Tus Jul 11 15:10:00 2017
// Last Modified By : Andrew Beach
// Last Modified On : Mon May 17 11:01:00 2021
// Update Count     : 2
//

#include "virtual.h"
#include "assert.h"

int __cfavir_is_parent(
		__cfavir_type_id parent,
		__cfavir_type_id child ) {
	assert( child );
	do {
		if ( parent == child )
			return 1;
		child = child->parent;
	} while ( child );
	return 0;
}

void * __cfavir_virtual_cast(
		__cfavir_type_id parent,
		__cfavir_type_id const * child ) {
	assert( child );
	return (__cfavir_is_parent(parent, *child)) ? (void *)child : (void *)0;
}
