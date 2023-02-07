//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// exception.h -- Internal exception handling definitions.
//
// Author           : Andrew Beach
// Created On       : Mon Jun 26 15:11:00 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Feb  2 11:20:19 2023
// Update Count     : 13
//

#pragma once

// This could be considered several headers. All are internal to the exception
// system but needed to depending on whether they are C/Cforall code and
// whether or not they are part of the builtins.

#ifdef __cforall
extern "C" {
#endif

// Included in C code or the built-ins.
#if !defined(__cforall) || defined(__cforall_builtins__)

struct __cfaehm_base_exception_t;
typedef struct __cfaehm_base_exception_t exception_t;
struct __cfavir_type_info;
struct __cfaehm_base_exception_t_vtable {
	const struct __cfavir_type_info * __cfavir_typeid;
	size_t size;
	void (*copy)(struct __cfaehm_base_exception_t *this,
	             struct __cfaehm_base_exception_t * other);
	void (*free)(struct __cfaehm_base_exception_t *this);
	const char * (*msg)(struct __cfaehm_base_exception_t *this);
};
struct __cfaehm_base_exception_t {
	struct __cfaehm_base_exception_t_vtable const * virtual_table;
};
extern struct __cfavir_type_info __cfatid_exception_t;


void __cfaehm_cancel_stack(exception_t * except) __attribute__((noreturn));

// Used in throw statement translation.
void __cfaehm_throw_terminate(exception_t * except, void (*)(exception_t *));
void __cfaehm_rethrow_terminate() __attribute__((noreturn));
void __cfaehm_throw_resume(exception_t * except, void (*)(exception_t *));

// Function catches termination exceptions.
void __cfaehm_try_terminate(
	void (*try_block)(),
	void (*catch_block)(int index, exception_t * except),
	int (*match_block)(exception_t * except));

// Clean-up the exception in catch blocks.
void __cfaehm_cleanup_terminate(void * except);

// Data structure creates a list of resume handlers.
struct __cfaehm_try_resume_node {
	struct __cfaehm_try_resume_node * next;
	_Bool (*handler)(exception_t * except);
};

// These act as constructor and destructor for the resume node.
void __cfaehm_try_resume_setup(
	struct __cfaehm_try_resume_node * node,
	_Bool (*handler)(exception_t * except));
void __cfaehm_try_resume_cleanup(
	struct __cfaehm_try_resume_node * node);

// Check for a standard way to call fake deconstructors.
struct __cfaehm_cleanup_hook {};

#endif

// Included in C code and the library.
#if !defined(__cforall) || !defined(__cforall_builtins__)
struct __cfaehm_node {
	struct _Unwind_Exception unwind_exception;
	struct __cfaehm_node * next;
	int handler_index;
};

static inline exception_t * __cfaehm_cancellation_exception(
		struct _Unwind_Exception * unwind_exception ) {
	return (exception_t *)(1 + (struct __cfaehm_node *)unwind_exception);
}
#endif

#ifdef __cforall
}

// Built-ins not visible in C.
#if defined(__cforall_builtins__)

// Not all the built-ins can be expressed in C. These can't be
// implemented in the .c file either so they all have to be inline.

forall( exceptT &, virtualT & )
trait is_exception {
	/* The first field must be a pointer to a virtual table.
	 * That virtual table must be a decendent of the base exception virtual table.
	 * The virtual table must point at the prober type-id.
	 * None of these can be enforced in an assertion.
	 */
};

forall( exceptT &, virtualT & | is_exception(exceptT, virtualT) )
trait is_termination_exception {
	void defaultTerminationHandler(exceptT &);
};

forall( exceptT &, virtualT & | is_exception(exceptT, virtualT) )
trait is_resumption_exception {
	void defaultResumptionHandler(exceptT &);
};

forall(exceptT &, virtualT & | is_termination_exception(exceptT, virtualT))
static inline void $throw(exceptT & except) {
	__cfaehm_throw_terminate(
		(exception_t *)&except,
		(void(*)(exception_t *))defaultTerminationHandler
	);
}

forall(exceptT &, virtualT & | is_resumption_exception(exceptT, virtualT))
static inline void $throwResume(exceptT & except) {
	__cfaehm_throw_resume(
		(exception_t *)&except,
		(void(*)(exception_t *))defaultResumptionHandler
	);
}

forall(exceptT &, virtualT & | is_exception(exceptT, virtualT))
static inline void cancel_stack(exceptT & except) __attribute__((noreturn)) {
	__cfaehm_cancel_stack( (exception_t *)&except );
}

forall(exceptT &, virtualT & | is_exception(exceptT, virtualT))
static inline void defaultTerminationHandler(exceptT & except) {
	return cancel_stack( except );
}

forall(exceptT &, virtualT & | is_termination_exception(exceptT, virtualT))
static inline void defaultResumptionHandler(exceptT & except) {
	throw except;
}

#endif

#endif
