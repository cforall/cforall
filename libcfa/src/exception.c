//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// exception.c --
//
// Author           : Andrew Beach
// Created On       : Mon Jun 26 15:13:00 2017
// Last Modified By : Andrew Beach
// Last Modified On : Wed Feb 24 13:40:00 2021
// Update Count     : 36
//

// Normally we would get this from the CFA prelude.
#include <stddef.h> // for size_t

#include <unwind.h> // for struct _Unwind_Exception {...};

#include "exception.h"

#include <stdlib.h>
#include <stdio.h>
#include <bits/debug.hfa>
#include "concurrency/invoke.h"
#include "stdhdr/assert.h"
#include "virtual.h"
#include "lsda.h"

/* The exception class for our exceptions. Because of the vendor component
 * its value would not be standard.
 * Vendor: UWPL
 * Language: CFA\0
 */
const _Unwind_Exception_Class __cfaehm_exception_class = 0x4c50575500414643;

// Base Exception type id:
struct __cfavir_type_info __cfatid_exception_t = {
	NULL,
};


// Get the current exception context.
// There can be a single global until multithreading occurs, then each stack
// needs its own. We get this from libcfathreads (no weak attribute).
__attribute__((weak)) struct exception_context_t * this_exception_context() {
	static struct exception_context_t shared_stack = {NULL, NULL};
	return &shared_stack;
}


// RESUMPTION ================================================================

static void reset_top_resume(struct __cfaehm_try_resume_node ** store) {
	this_exception_context()->top_resume = *store;
}

void __cfaehm_throw_resume(exception_t * except, void (*defaultHandler)(exception_t *)) {
	struct exception_context_t * context = this_exception_context();

	__cfadbg_print_safe(exception, "Throwing resumption exception\n");

	{
		__attribute__((cleanup(reset_top_resume)))
		struct __cfaehm_try_resume_node * original_head = context->top_resume;
		struct __cfaehm_try_resume_node * current = context->top_resume;

		for ( ; current ; current = current->next) {
			context->top_resume = current->next;
			if (current->handler(except)) {
				return;
			}
		}
	} // End the search and return to the top of the stack.

	// No handler found, fall back to the default operation.
	__cfadbg_print_safe(exception, "Unhandled exception\n");
	defaultHandler(except);
}

// Do we control where exceptions get thrown even with concurency?
// If not these are not quite thread safe, the cleanup hook has to
// be added after the node is built but before it is made the top node.

void __cfaehm_try_resume_setup(struct __cfaehm_try_resume_node * node,
                        _Bool (*handler)(exception_t * except)) {
	struct exception_context_t * context = this_exception_context();
	node->next = context->top_resume;
	node->handler = handler;
	context->top_resume = node;
}

void __cfaehm_try_resume_cleanup(struct __cfaehm_try_resume_node * node) {
	struct exception_context_t * context = this_exception_context();
	context->top_resume = node->next;
}


// MEMORY MANAGEMENT =========================================================

#define NODE_TO_EXCEPT(node) ((exception_t *)(1 + (node)))
#define EXCEPT_TO_NODE(except) ((struct __cfaehm_node *)(except) - 1)
#define UNWIND_TO_NODE(unwind) ((struct __cfaehm_node *)(unwind))
#define NULL_MAP(map, ptr) ((ptr) ? (map(ptr)) : NULL)

// How to clean up an exception in various situations.
static void __cfaehm_exception_cleanup(
		_Unwind_Reason_Code reason,
		struct _Unwind_Exception * exception) {
	switch (reason) {
	case _URC_FOREIGN_EXCEPTION_CAUGHT:
		// This one we could clean-up to allow cross-language exceptions.
	case _URC_FATAL_PHASE1_ERROR:
	case _URC_FATAL_PHASE2_ERROR:
	default:
		abort();
	}
}

// Creates a copy of the indicated exception and sets current_exception to it.
static void __cfaehm_allocate_exception( exception_t * except ) {
	struct exception_context_t * context = this_exception_context();

	// Allocate memory for the exception.
	struct __cfaehm_node * store = malloc(
		sizeof( struct __cfaehm_node ) + except->virtual_table->size );

	if ( ! store ) {
		// Failure: cannot allocate exception. Terminate thread.
		abort(); // <- Although I think it might be the process.
	}

	// Initialize the node:
	exception_t * except_store = NODE_TO_EXCEPT(store);
	store->unwind_exception.exception_class = __cfaehm_exception_class;
	store->unwind_exception.exception_cleanup = __cfaehm_exception_cleanup;
	store->handler_index = 0;
	except->virtual_table->copy( except_store, except );

	// Add the node to the list:
	store->next = NULL_MAP(EXCEPT_TO_NODE, context->current_exception);
	context->current_exception = except_store;
}

// Delete the provided exception, unsetting current_exception if relivant.
static void __cfaehm_delete_exception( exception_t * except ) {
	struct exception_context_t * context = this_exception_context();

	__cfadbg_print_safe(exception, "Deleting Exception\n");

	// Remove the exception from the list.
	struct __cfaehm_node * to_free = EXCEPT_TO_NODE(except);
	struct __cfaehm_node * node;

	if ( context->current_exception == except ) {
		node = to_free->next;
		context->current_exception = NULL_MAP(NODE_TO_EXCEPT, node);
	} else {
		node = EXCEPT_TO_NODE(context->current_exception);
		// It may always be in the first or second position.
		while ( to_free != node->next ) {
			node = node->next;
		}
		node->next = to_free->next;
	}

	// Free the old exception node.
	except->virtual_table->free( except );
	free( to_free );
}

// CANCELLATION ==============================================================

// Function needed by force unwind
// It basically says to unwind the whole stack and then exit when we reach the end of the stack
static _Unwind_Reason_Code _Stop_Fn(
		int version,
		_Unwind_Action actions,
		_Unwind_Exception_Class exception_class,
		struct _Unwind_Exception * unwind_exception,
		struct _Unwind_Context * unwind_context,
		void * stop_param) {
	// Verify actions follow the rules we expect.
	verify(actions & _UA_CLEANUP_PHASE);
	verify(actions & _UA_FORCE_UNWIND);
	verify(!(actions & _UA_SEARCH_PHASE));
	verify(!(actions & _UA_HANDLER_FRAME));

	if ( actions & _UA_END_OF_STACK ) {
		abort();
	} else {
		return _URC_NO_REASON;
	}
}

__attribute__((weak)) _Unwind_Reason_Code
__cfaehm_cancellation_unwind( struct _Unwind_Exception * exception ) {
	return _Unwind_ForcedUnwind( exception, _Stop_Fn, (void*)0x22 );
}

// Cancel the current stack, prefroming approprate clean-up and messaging.
void __cfaehm_cancel_stack( exception_t * exception ) {
	__cfaehm_allocate_exception( exception );

	struct exception_context_t * context = this_exception_context();
	struct __cfaehm_node * node = EXCEPT_TO_NODE(context->current_exception);

	// Preform clean-up of any extra active exceptions.
	while ( node->next ) {
		struct __cfaehm_node * to_free = node->next;
		node->next = to_free->next;
		exception_t * except = NODE_TO_EXCEPT( to_free );
		except->virtual_table->free( except );
	    free( to_free );
	}

	_Unwind_Reason_Code ret;
	ret = __cfaehm_cancellation_unwind( &node->unwind_exception );
	printf("UNWIND ERROR %d after force unwind\n", ret);
	abort();
}


// TERMINATION ===============================================================

// If this isn't a rethrow (*except==0), delete the provided exception.
void __cfaehm_cleanup_terminate( void * except ) {
	if ( *(void**)except ) __cfaehm_delete_exception( *(exception_t **)except );
}

static void __cfaehm_cleanup_default( exception_t ** except ) {
	__cfaehm_delete_exception( *except );
	*except = NULL;
}

// The exception that is being thrown must already be stored.
static void __cfaehm_begin_unwind(void(*defaultHandler)(exception_t *)) {
	struct exception_context_t * context = this_exception_context();
	if ( NULL == context->current_exception ) {
		printf("UNWIND ERROR missing exception in begin unwind\n");
		abort();
	}
	struct _Unwind_Exception * storage =
		&EXCEPT_TO_NODE(context->current_exception)->unwind_exception;

	// Call stdlibc to raise the exception
	__cfadbg_print_safe(exception, "Begin unwinding (storage &p, context %p)\n", storage, context);
	_Unwind_Reason_Code ret = _Unwind_RaiseException( storage );

	// If we reach here it means something happened. For resumption to work we need to find a way
	// to return back to here. Most of them will probably boil down to setting a global flag and
	// making the phase 1 either stop or fail. Causing an error on purpose may help avoiding
	// unnecessary work but it might have some weird side effects. If we just pretend no handler
	// was found that would work but may be expensive for no reason since we will always search
	// the whole stack.

#if defined( __x86_64 ) || defined( __i386 )
	// We did not simply reach the end of the stack without finding a handler. This is an error.
	if ( ret != _URC_END_OF_STACK ) {
#else // defined( __ARM_ARCH )
	// The return code from _Unwind_RaiseException seems to be corrupt on ARM at end of stack.
	// This workaround tries to keep default exception handling working. 
	if ( ret == _URC_FATAL_PHASE1_ERROR || ret == _URC_FATAL_PHASE2_ERROR ) {
#endif
		printf("UNWIND ERROR %d after raise exception\n", ret);
		abort();
	}

	// No handler found, go to the default operation.
	__cfadbg_print_safe(exception, "Uncaught exception %p\n", storage);

	__attribute__((cleanup(__cfaehm_cleanup_default)))
	exception_t * exception = context->current_exception;
	defaultHandler( exception );
}

void __cfaehm_throw_terminate( exception_t * val, void (*defaultHandler)(exception_t *) ) {
	__cfadbg_print_safe(exception, "Throwing termination exception\n");

	__cfaehm_allocate_exception( val );
	__cfaehm_begin_unwind( defaultHandler );
}

static __attribute__((noreturn)) void __cfaehm_rethrow_adapter( exception_t * except ) {
	// TODO: Print some error message.
	(void)except;
	abort();
}

void __cfaehm_rethrow_terminate(void) {
	__cfadbg_print_safe(exception, "Rethrowing termination exception\n");

	__cfaehm_begin_unwind( __cfaehm_rethrow_adapter );
	abort();
}

#if defined( __x86_64 ) || defined( __i386 ) || defined( __ARM_ARCH )
// This is our personality routine. For every stack frame annotated with
// ".cfi_personality 0x3,__gcfa_personality_v0" this function will be called twice when unwinding.
//  Once in the search phase and once in the cleanup phase.
_Unwind_Reason_Code __gcfa_personality_v0(
		int version,
		_Unwind_Action actions,
		unsigned long long exception_class,
		struct _Unwind_Exception * unwind_exception,
		struct _Unwind_Context * unwind_context)
{

	//__cfadbg_print_safe(exception, "CFA: 0x%lx\n", _Unwind_GetCFA(context));
	__cfadbg_print_safe(exception, "Personality function (%d, %x, %llu, %p, %p):",
			version, actions, exception_class, unwind_exception, unwind_context);

	// Verify that actions follow the rules we expect.
	// This function should never be called at the end of the stack.
	verify(!(actions & _UA_END_OF_STACK));
	// Either only the search phase flag is set or...
	if (actions & _UA_SEARCH_PHASE) {
		verify(actions == _UA_SEARCH_PHASE);
		__cfadbg_print_safe(exception, " lookup phase");
	// ... we are in clean-up phase.
	} else {
		verify(actions & _UA_CLEANUP_PHASE);
		__cfadbg_print_safe(exception, " cleanup phase");
		// We shouldn't be the handler frame during forced unwind.
		if (actions & _UA_HANDLER_FRAME) {
			verify(!(actions & _UA_FORCE_UNWIND));
			__cfadbg_print_safe(exception, " (handler frame)");
		} else if (actions & _UA_FORCE_UNWIND) {
			__cfadbg_print_safe(exception, " (force unwind)");
		}
	}

	// Get a pointer to the language specific data from which we will read what we need
	const unsigned char * lsd = _Unwind_GetLanguageSpecificData( unwind_context );

	if ( !lsd ) {	//Nothing to do, keep unwinding
		printf(" no LSD");
		goto UNWIND;
	}

	// Get the instuction pointer and a reading pointer into the exception table
	lsda_header_info lsd_info;
	const unsigned char * cur_ptr = parse_lsda_header(unwind_context, lsd, &lsd_info);
	_Unwind_Ptr instruction_ptr = _Unwind_GetIP(unwind_context);

	struct exception_context_t * context = this_exception_context();

	// Linearly search the table for stuff to do
	while ( cur_ptr < lsd_info.action_table ) {
		_Unwind_Ptr callsite_start;
		_Unwind_Ptr callsite_len;
		_Unwind_Ptr callsite_landing_pad;
		_uleb128_t  callsite_action;

		// Decode the common stuff we have in here
		cur_ptr = read_encoded_value(0, lsd_info.call_site_encoding, cur_ptr, &callsite_start);
		cur_ptr = read_encoded_value(0, lsd_info.call_site_encoding, cur_ptr, &callsite_len);
		cur_ptr = read_encoded_value(0, lsd_info.call_site_encoding, cur_ptr, &callsite_landing_pad);
		cur_ptr = read_uleb128(cur_ptr, &callsite_action);

		// Have we reach the correct frame info yet?
		if ( lsd_info.Start + callsite_start + callsite_len < instruction_ptr ) {
#ifdef __CFA_DEBUG_PRINT__
			void * ls = (void*)lsd_info.Start;
			void * cs = (void*)callsite_start;
			void * cl = (void*)callsite_len;
			void * bp = (void*)lsd_info.Start + callsite_start;
			void * ep = (void*)lsd_info.Start + callsite_start + callsite_len;
			void * ip = (void*)instruction_ptr;
			__cfadbg_print_safe(exception, "\nfound %p - %p (%p, %p, %p), looking for %p\n",
					bp, ep, ls, cs, cl, ip);
#endif // __CFA_DEBUG_PRINT__
			continue;
		}

		// Have we gone too far?
		if ( lsd_info.Start + callsite_start > instruction_ptr ) {
			printf(" gone too far");
			break;
		}

		// Check for what we must do:
		if ( 0 == callsite_landing_pad ) {
			// Nothing to do, move along
			__cfadbg_print_safe(exception, " no landing pad");
		} else if (actions & _UA_SEARCH_PHASE) {
			// In search phase, these means we found a potential handler we must check.

			// We have arbitrarily decided that 0 means nothing to do and 1 means there is
			// a potential handler. This doesn't seem to conflict the gcc default behavior.
			if (callsite_action != 0) {
				// Now we want to run some code to see if the handler matches
				// This is the tricky part where we want to the power to run arbitrary code
				// However, generating a new exception table entry and try routine every time
				// is way more expansive than we might like
				// The information we have is :
				//  - The GR (Series of registers)
				//    GR1=GP Global Pointer of frame ref by context
				//  - The instruction pointer
				//  - The instruction pointer info (???)
				//  - The CFA (Canonical Frame Address)
				//  - The BSP (Probably the base stack pointer)

				// The current apprach uses one exception table entry per try block
				_uleb128_t imatcher;
				// Get the relative offset to the {...}?
				cur_ptr = read_uleb128(cur_ptr, &imatcher);

				_Unwind_Word match_pos =
#				if defined( __x86_64 )
				    _Unwind_GetCFA(unwind_context) + 8;
#				elif defined( __i386 )
				    _Unwind_GetCFA(unwind_context) + 24;
#				elif defined( __ARM_ARCH )
				    _Unwind_GetCFA(unwind_context) + 40;
#				endif
				int (*matcher)(exception_t *) = *(int(**)(exception_t *))match_pos;

				int index = matcher(context->current_exception);
				_Unwind_Reason_Code ret = (0 == index)
					? _URC_CONTINUE_UNWIND : _URC_HANDLER_FOUND;
				UNWIND_TO_NODE(unwind_exception)->handler_index = index;

				// Based on the return value, check if we matched the exception
				if (ret == _URC_HANDLER_FOUND) {
					__cfadbg_print_safe(exception, " handler found\n");
				} else {
					// TODO: Continue the search if there is more in the table.
					__cfadbg_print_safe(exception, " no handler\n");
				}
				return ret;
			}

			// This is only a cleanup handler, ignore it
			__cfadbg_print_safe(exception, " no action");
		} else {
			// In clean-up phase, no destructors here but this could be the handler.

			if ( (callsite_action != 0) && !(actions & _UA_HANDLER_FRAME) ){
				// If this is a potential exception handler
				// but not the one that matched the exception in the seach phase,
				// just ignore it
				goto UNWIND;
			}

			// We need to run some clean-up or a handler
			// These statment do the right thing but I don't know any specifics at all
			_Unwind_SetGR( unwind_context, __builtin_eh_return_data_regno(0),
				(_Unwind_Ptr)unwind_exception );
			_Unwind_SetGR( unwind_context, __builtin_eh_return_data_regno(1), 0 );

			// I assume this sets the instruction pointer to the adress of the landing pad
			// It doesn't actually set it, it only state the value that needs to be set once we
			// return _URC_INSTALL_CONTEXT
			_Unwind_SetIP( unwind_context, ((lsd_info.LPStart) + (callsite_landing_pad)) );

			__cfadbg_print_safe(exception, " action\n");

			// Return have some action to run
			return _URC_INSTALL_CONTEXT;
		}
	}
	// No handling found
	__cfadbg_print_safe(exception, " table end reached");

	UNWIND:
	__cfadbg_print_safe(exception, " unwind\n");

	// Keep unwinding the stack
	return _URC_CONTINUE_UNWIND;
}

#pragma GCC push_options
#pragma GCC optimize(0)

// Try statements are hoisted out see comments for details. While this could probably be unique
// and simply linked from libcfa but there is one problem left, see the exception table for details
__attribute__((noinline))
void __cfaehm_try_terminate(void (*try_block)(),
		void (*catch_block)(int index, exception_t * except),
		__attribute__((unused)) int (*match_block)(exception_t * except)) {
	//! volatile int xy = 0;
	//! printf("%p %p %p %p\n", &try_block, &catch_block, &match_block, &xy);

	// Setup the personality routine and exception table.
	// Unforturnately these clobber gcc cancellation support which means we can't get access to
	// the attribute cleanup tables at the same time. We would have to inspect the assembly to
	// create a new set ourselves.
#ifdef __PIC__
	asm volatile (".cfi_personality 0x9b,CFA.ref.__gcfa_personality_v0");
	asm volatile (".cfi_lsda 0x1b, .LLSDACFA2");
#else
	asm volatile (".cfi_personality 0x3,__gcfa_personality_v0");
	asm volatile (".cfi_lsda 0x3, .LLSDACFA2");
#endif

	// Label which defines the start of the area for which the handler is setup.
	asm volatile (".TRYSTART:");

	// The actual statements of the try blocks
	try_block();

	// asm statement to prevent deadcode removal
	asm volatile goto ("" : : : : CATCH );

	// Normal return for when there is no throw.
	return;

	// Exceptionnal path
	CATCH : __attribute__(( unused ));
	// Label which defines the end of the area for which the handler is setup.
	asm volatile (".TRYEND:");
	// Label which defines the start of the exception landing pad. Basically what is called when
	// the exception is caught. Note, if multiple handlers are given, the multiplexing should be
	// done by the generated code, not the exception runtime.
	asm volatile (".CATCH:");

	// Exception handler
	// Note: Saving the exception context on the stack breaks termination exceptions.
	catch_block( EXCEPT_TO_NODE( this_exception_context()->current_exception )->handler_index,
	             this_exception_context()->current_exception );
}

// Exception table data we need to generate. While this is almost generic, the custom data refers
// to {*}try_terminate, which is no way generic. Some more works need to be done if we want to
// have a single call to the try routine.

#ifdef __PIC__
asm (
	// HEADER
	".LFECFA1:\n"
#if defined( __x86_64 ) || defined( __i386 )
	"	.globl	__gcfa_personality_v0\n"
#else // defined( __ARM_ARCH )
	"	.global	__gcfa_personality_v0\n"
#endif
	"	.section	.gcc_except_table,\"a\",@progbits\n"
	// TABLE HEADER (important field is the BODY length at the end)
	".LLSDACFA2:\n"
	"	.byte	0xff\n"
	"	.byte	0xff\n"
	"	.byte	0x1\n"
	"	.uleb128 .LLSDACSECFA2-.LLSDACSBCFA2\n"
	// BODY (language specific data)
	// This uses language specific data and can be modified arbitrarily
	// We use handled area offset, handled area length,
	// handler landing pad offset and 1 (action code, gcc seems to use 0).
	".LLSDACSBCFA2:\n"
	"	.uleb128 .TRYSTART-__cfaehm_try_terminate\n"
	"	.uleb128 .TRYEND-.TRYSTART\n"
	"	.uleb128 .CATCH-__cfaehm_try_terminate\n"
	"	.uleb128 1\n"
	".LLSDACSECFA2:\n"
	// TABLE FOOTER
	"	.text\n"
	"	.size	__cfaehm_try_terminate, .-__cfaehm_try_terminate\n"
);

// Somehow this piece of helps with the resolution of debug symbols.
__attribute__((unused)) static const int dummy = 0;

asm (
	// Add a hidden symbol which points at the function.
	"	.hidden	CFA.ref.__gcfa_personality_v0\n"
	"	.weak	CFA.ref.__gcfa_personality_v0\n"
	// No clue what this does specifically
	"	.section	.data.rel.local.CFA.ref.__gcfa_personality_v0,\"awG\",@progbits,CFA.ref.__gcfa_personality_v0,comdat\n"
#if defined( __x86_64 ) || defined( __i386 )
	"	.align 8\n"
#else // defined( __ARM_ARCH )
	"	.align 3\n"
#endif
	"	.type CFA.ref.__gcfa_personality_v0, @object\n"
	"	.size CFA.ref.__gcfa_personality_v0, 8\n"
	"CFA.ref.__gcfa_personality_v0:\n"
#if defined( __x86_64 )
	"	.quad __gcfa_personality_v0\n"
#elif defined( __i386 )
	"	.long __gcfa_personality_v0\n"
#else // defined( __ARM_ARCH )
	"	.xword __gcfa_personality_v0\n"
#endif
);
#else // __PIC__
asm (
	// HEADER
	".LFECFA1:\n"
#if defined( __x86_64 ) || defined( __i386 )
	"	.globl	__gcfa_personality_v0\n"
#else // defined( __ARM_ARCH )
	"	.global	__gcfa_personality_v0\n"
#endif
	"	.section	.gcc_except_table,\"a\",@progbits\n"
	// TABLE HEADER (important field is the BODY length at the end)
	".LLSDACFA2:\n"
	"	.byte	0xff\n"
	"	.byte	0xff\n"
	"	.byte	0x1\n"
	"	.uleb128 .LLSDACSECFA2-.LLSDACSBCFA2\n"
	// BODY (language specific data)
	".LLSDACSBCFA2:\n"
	//	Handled area start (relative to start of function)
	"	.uleb128 .TRYSTART-__cfaehm_try_terminate\n"
	//	Handled area length
	"	.uleb128 .TRYEND-.TRYSTART\n"
	//	Handler landing pad address (relative to start of function)
	"	.uleb128 .CATCH-__cfaehm_try_terminate\n"
	//	Action code, gcc seems to always use 0.
	"	.uleb128 1\n"
	// TABLE FOOTER
	".LLSDACSECFA2:\n"
	"	.text\n"
	"	.size	__cfaehm_try_terminate, .-__cfaehm_try_terminate\n"
	"	.ident	\"GCC: (Ubuntu 6.2.0-3ubuntu11~16.04) 6.2.0 20160901\"\n"
	"	.section	.note.GNU-stack,\"x\",@progbits\n"
);
#endif // __PIC__

#pragma GCC pop_options

#else
	#error unsupported hardware architecture
#endif // __x86_64 || __i386 || __ARM_ARCH
