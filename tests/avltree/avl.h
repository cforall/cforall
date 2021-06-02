#pragma once

extern "C" {
#define NULL 0
#define assert(cond) if (! (cond)) { printf("Assertion failed: (%s) at %s:%d\n", #cond, __FILE__, __LINE__); abort(); }
}

// #include <types.h>
// #include <lib.h>

trait Comparable(T) {
  int ?<?(T, T);
};

forall(T | Comparable(T))
int ?==?(T t1, T t2);

forall(T | Comparable(T))
int ?>?(T t1, T t2);

// xxx - unbound type variable problems when trying to use new instead of create
// forall( otype T, ttype Params | { void ?{}(T *, Params); } ) T * new( Params p );

// To-do: properly use height or balance factor
// Right now I'm recomputing the height for each
// node multiple times. It's Theta-log(n), but still..

// Balanced Binary Search Tree of void pointers; almost an AVL tree -
//   just needs to make use of the balance factor properly
// Operations:
// ?{}, ^?{}
// create   - allocate a new tree. Just a wrapper around malloc which also calls the tree constructor.
// find     - search through the tree for the given key; return the associated value
// empty    - return true if the tree is empty
// insert   - insert node with key and value pair. Returns 0 on success
// remove   - remove node with the given key, returns 0 on success, 1 on failure
// copy     - deep copy of a tree
// for_each - applies the given function to every data element in the tree
//    assumes that a non-zero return value is an error, will return
//    the error code from func

// temporary: need forward decl to get around typedef problem
forall(K | Comparable(K), V)
struct tree;

forall(K | Comparable(K), V)
struct tree {
  K key;
  V value;
  tree(K, V) * parent;
  tree(K, V) * left;
  tree(K, V) * right;
  int balance;
};

forall(K | Comparable(K), V)
void ?{}(tree(K, V) &t, K key, V value);

forall(K | Comparable(K), V)
void ^?{}(tree(K, V) & t);

forall(K | Comparable(K), V)
tree(K, V) * create(K key, V value);

forall(K | Comparable(K), V)
V * find(tree(K, V) * t, K key);

forall(K | Comparable(K), V)
int empty(tree(K, V) * t);

// returns the root of the tree
forall(K | Comparable(K), V)
int insert(tree(K, V) ** t, K key, V value);

forall(K | Comparable(K), V)
int remove(tree(K, V) ** t, K key);

forall(K | Comparable(K), V)
void copy(tree(K, V) * src, tree(K, V) ** ret);

forall(K | Comparable(K), V)
void for_each(tree(K, V) * t, void (*func)(V));

// // Helper function to print trees
// forall(otype K | Comparable(K), otype V)
// void printTree(tree * t, int level){
//   if (empty(t)){
//     return;
//   }

//   printTree(t->left, level+1);
//   printf("key: %d, value: %s, level: %d\n", t->key, t->value, level);
//   printTree(t->right, level+1);
// }

// // inorder traversal of t
// // prints each key, followed by the value
// forall(otype K | Comparable(K), otype V)
// void printTree(tree(K, V) * t){
//     printTree(t, 0);
// }
