//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CtxSwitch-x86_64.S --
//
// Author           : Peter A. Buhr
// Created On       : Mon Aug 10 08:10:26 2020
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Oct 24 14:36:25 2020
// Update Count     : 10
//

// The context switch routine requires the initial the stack of a thread to
// look like the thread has saved its context in the normal manner.

// Offsets must synchronized with the __stack_context_t in invoke.h.

#define PTR_BYTE	8
#define SP_OFFSET	( 0 * PTR_BYTE )
#define FP_OFFSET	( 1 * PTR_BYTE )

// Context switch between coroutines/tasks.
//   void __cfactx_switch( struct __stack_context_t * from, struct __stack_context_t * to ) ;
// Arguments "from" in register rdi, "to" in register rsi.

	.file "CtxSwitch-x86_64.S"
	.text
	.align 2
	.global __cfactx_switch
	.type __cfactx_switch, @function
__cfactx_switch:

	// Save volatile registers on the stack.

	pushq %r15
	pushq %r14
	pushq %r13
	pushq %r12
	pushq %rbx

	// Save old context in the "from" area.

	movq %rsp,SP_OFFSET(%rdi)
	movq %rbp,FP_OFFSET(%rdi)

	// Load new context from the "to" area.

	movq SP_OFFSET(%rsi),%rsp
	movq FP_OFFSET(%rsi),%rbp

	// Load volatile registers from the stack.

	popq %rbx
	popq %r12
	popq %r13
	popq %r14
	popq %r15

	// Return to thread.

	ret
	.size __cfactx_switch, .-__cfactx_switch

// Stub to create new stacks which can be context switched to
//   void __cfactx_invoke_stub( void );

	.text
	.align 2
	.global __cfactx_invoke_stub
	.type __cfactx_invoke_stub, @function
__cfactx_invoke_stub:
	movq %rbx, %rdi						// move main and this to first two arguments
	movq %r12, %rsi
	jmp *%r13							// jmp to invoke
	.size __cfactx_invoke_stub, .-__cfactx_invoke_stub

// Local Variables: //
// mode: asm //
// tab-width: 4 //
// End: //
