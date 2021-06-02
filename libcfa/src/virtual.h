//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// virtual.h -- Builtins for hierarchy objects.
//
// Author           : Andrew Beach
// Created On       : Tus Jul 11 15:08:00 2017
// Last Modified By : Andrew Beach
// Last Modified On : Mon May 17 11:03:00 2021
// Update Count     : 2
//

#pragma once

#ifdef __cforall
extern "C" {
#endif

// Information on a type for the virtual system.
// There should be exactly one instance per type and there should be a
// pointer to it at the head of every virtual table.
struct __cfavir_type_info {
	// Type id of parent type, null if this is a root type.
    struct __cfavir_type_info const * const parent;
};

// A pointer to type information acts as the type id.
typedef struct __cfavir_type_info const * __cfavir_type_id;

// Takes in two non-null type ids.
int __cfavir_is_parent(
		__cfavir_type_id parent, __cfavir_type_id child );

// If parent is a parent of child then return child, otherwise return NULL.
// Input pointers are none-null, child's first level should be an object with
// a vtable
void * __cfavir_virtual_cast(
		__cfavir_type_id parent, __cfavir_type_id const * child );

#ifdef __cforall
}
#endif
