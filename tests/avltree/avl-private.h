#pragma once
#include "avl.h"

// functions that really shouldn't be exposed, but are to reduce compilation time

// attempt to fix the tree, if necessary
forall(K | Comparable(K), V)
tree(K, V) * tryFix(tree(K, V) * t);

// sets parent field of c to be p
forall(K | Comparable(K), V)
void setParent(tree(K, V) * c, tree(K, V) * p);

forall(K | Comparable(K), V)
int height(tree(K, V) * t);
