//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// invoke.c --
//
// Author           : Thierry Delisle
// Created On       : Tue Jan 17 12:27:26 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Oct 24 14:35:28 2020
// Update Count     : 32
//

#define __cforall_thread__

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <unwind.h>

#include "invoke.h"

#define __CFA_INVOKE_PRIVATE__
#include "invoke.h"

// magically invoke the "main" of the most derived class
// Called from the kernel when starting a coroutine or task so must switch back to user mode.

extern struct $coroutine * __cfactx_cor_finish(void);
extern void __cfactx_cor_leave ( struct $coroutine * );
extern void __cfactx_thrd_leave();

extern void disable_interrupts() OPTIONAL_THREAD;
extern void enable_interrupts( _Bool poll );

void __cfactx_invoke_coroutine(
	void (*main)(void *),
	void *this
) {
	// Finish setting up the coroutine by setting its state
	struct $coroutine * cor = __cfactx_cor_finish();

	// Call the main of the coroutine
	main( this );

	//Final suspend, should never return
	__cfactx_cor_leave( cor );
	__cabi_abort( "Resumed dead coroutine" );
}

static _Unwind_Reason_Code __cfactx_coroutine_unwindstop(
	__attribute((__unused__)) int version,
	_Unwind_Action actions,
	__attribute((__unused__)) _Unwind_Exception_Class exceptionClass,
	__attribute((__unused__)) struct _Unwind_Exception * unwind_exception,
	__attribute((__unused__)) struct _Unwind_Context * context,
	void * param
) {
	if( actions & _UA_END_OF_STACK  ) {
		// We finished unwinding the coroutine,
		// leave it
		__cfactx_cor_leave( param );
		__cabi_abort( "Resumed dead coroutine" );
	}
	if( actions & _UA_CLEANUP_PHASE ) return _URC_NO_REASON;

	return _URC_FATAL_PHASE2_ERROR;
}

void __cfactx_coroutine_unwind(struct _Unwind_Exception * storage, struct $coroutine * cor) __attribute__ ((__noreturn__));
void __cfactx_coroutine_unwind(struct _Unwind_Exception * storage, struct $coroutine * cor) {
	_Unwind_Reason_Code ret = _Unwind_ForcedUnwind( storage, __cfactx_coroutine_unwindstop, cor );
	printf("UNWIND ERROR %d after force unwind\n", ret);
	abort();
}

void __cfactx_invoke_thread(
	void (*main)(void *),
	void *this
) {
	// Officially start the thread by enabling preemption
	enable_interrupts( true );

	// Call the main of the thread
	main( this );

	// To exit a thread we must :
	// 1 - Mark it as halted
	// 2 - Leave its monitor
	// 3 - Disable the interupts
	// 4 - Final suspend
	// The order of these 4 operations is very important
	//Final suspend, should never return
	__cfactx_thrd_leave();
	__cabi_abort( "Resumed dead thread" );
}

void __cfactx_start(
	void (*main)(void *),
	struct $coroutine * cor,
	void *this,
	void (*invoke)(void *)
) {
	struct __stack_t * stack = cor->stack.storage;

#if defined( __i386 )

	struct FakeStack {
	    void *fixedRegisters[3];						// fixed registers ebx, edi, esi (popped on 1st uSwitch, values unimportant)
	    void *rturn;									// where to go on return from uSwitch
	    void *dummyReturn;								// fake return compiler would have pushed on call to uInvoke
	    void *argument[3];								// for 16-byte ABI, 16-byte alignment starts here
	    void *padding;									// padding to force 16-byte alignment, as "base" is 16-byte aligned
	};

	cor->context.SP = (char *)stack->base - sizeof( struct FakeStack );
	cor->context.FP = NULL;		// terminate stack with NULL fp

	struct FakeStack *fs = (struct FakeStack *)cor->context.SP;

	fs->dummyReturn = NULL;
	fs->argument[0] = main;								// argument to invoke
	fs->argument[1] = this;								// argument to invoke
	fs->rturn = invoke;

#elif defined( __x86_64 )

	struct FakeStack {
		void *fixedRegisters[5];						// fixed registers rbx, r12, r13, r14, r15
		void *rturn;									// where to go on return from uSwitch
		void *dummyReturn;								// NULL return address to provide proper alignment
	};

	cor->context.SP = (char *)stack->base - sizeof( struct FakeStack );
	cor->context.FP = NULL;								// terminate stack with NULL fp

	struct FakeStack *fs = (struct FakeStack *)cor->context.SP;

	fs->dummyReturn = NULL;
	fs->rturn = __cfactx_invoke_stub;
	fs->fixedRegisters[0] = main;						// argument to invoke
	fs->fixedRegisters[1] = this;						// argument to invoke
	fs->fixedRegisters[2] = invoke;

#elif defined( __ARM_ARCH_32 )
#error ARM needs to be upgrade to use two parameters like X86/X64 (A.K.A. : I broke this and do not know how to fix it)
	// More details about the error:
	// To avoid the thunk problem, I changed the invoke routine to pass the main explicitly
	// instead of relying on an assertion. This effectively hoists any required thunk one level
	// which was enough to get to global scope in most cases.
	// This means that __cfactx_invoke_... now takes two parameters and the FakeStack needs
	// to be adjusted as a consequence of that.
	// I don't know how to do that for ARM, hence the #error

	struct FakeStack {
		float fpRegs[16];								// floating point registers
		void * intRegs[9];								// integer/pointer registers
		void * arg[2];									// placeholder for this pointer
	};

	cor->context.SP = (char *)stack->base - sizeof( struct FakeStack );
	cor->context.FP = NULL;

	struct FakeStack *fs = (struct FakeStack *)cor->context.SP;

	fs->intRegs[8] = __cfactx_invoke_stub;
	fs->arg[0] = this;
	fs->arg[1] = invoke;

#elif defined( __ARM_ARCH )
	struct FakeStack {
		void * intRegs[12];								// x19-x30 integer registers
		double fpRegs[8];								// v8-v15 floating point
	};

	cor->context.SP = (char *)stack->base - sizeof( struct FakeStack );
	cor->context.FP = NULL;

	struct FakeStack *fs = (struct FakeStack *)cor->context.SP;

	fs->intRegs[0] = main;								// argument to invoke x19 => x0
	fs->intRegs[1] = this;								// argument to invoke x20 => x1
	fs->intRegs[2] = invoke;
	fs->intRegs[11] = __cfactx_invoke_stub;				// link register x30 => ret moves to pc
#else
	#error uknown hardware architecture
#endif
}

// Local Variables: //
// mode: c //
// tab-width: 4 //
// End: //
