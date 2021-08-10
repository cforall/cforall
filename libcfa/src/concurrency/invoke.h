//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// invoke.h --
//
// Author           : Thierry Delisle
// Created On       : Tue Jan 17 12:27:26 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Dec  5 16:26:03 2019
// Update Count     : 44
//

#include "bits/containers.hfa"
#include "bits/defs.hfa"
#include "bits/locks.hfa"
#include "kernel/fwd.hfa"

#ifdef __cforall
#include "containers/list.hfa"
extern "C" {
#endif

#if ! defined(__CFA_INVOKE_PRIVATE__)
#ifndef _INVOKE_H_
#define _INVOKE_H_

	struct __cfaehm_try_resume_node;
	struct __cfaehm_base_exception_t;
	struct exception_context_t {
		struct __cfaehm_try_resume_node * top_resume;
		struct __cfaehm_base_exception_t * current_exception;
	};

	struct __stack_context_t {
		void * SP;
		void * FP;
	};

	// low adresses  :           +----------------------+ <- start of allocation
	//                           |  optional guard page |
	//                           +----------------------+ <- __stack_t.limit
	//                           |                      |
	//                           |       /\ /\ /\       |
	//                           |       || || ||       |
	//                           |                      |
	//                           |    program  stack    |
	//                           |                      |
	// __stack_info_t.storage -> +----------------------+ <- __stack_t.base
	//                           |      __stack_t       |
	// high adresses :           +----------------------+ <- end of allocation

	struct __stack_t {
		// stack grows towards stack limit
		void * limit;

		// base of stack
		void * base;

		// Information for exception handling.
		struct exception_context_t exception_context;
	};

	struct __stack_info_t {
		// pointer to stack
		struct __stack_t * storage;
	};

	enum __Coroutine_State { Halted, Start, Primed, Blocked, Ready, Active, Cancelled, Halting };

	struct coroutine$ {
		// context that is switch during a __cfactx_switch
		struct __stack_context_t context;

		// stack information of the coroutine
		struct __stack_info_t stack;

		// textual name for coroutine/task
		const char * name;

		// current execution status for coroutine
		enum __Coroutine_State state;

		// first coroutine to resume this one
		struct coroutine$ * starter;

		// last coroutine to resume this one
		struct coroutine$ * last;

		// If non-null stack must be unwound with this exception
		struct _Unwind_Exception * cancellation;

	};
	// Wrapper for gdb
	struct cfathread_coroutine_t { struct coroutine$ debug; };

	static inline struct __stack_t * __get_stack( struct coroutine$ * cor ) {
		return (struct __stack_t*)(((uintptr_t)cor->stack.storage) & ((uintptr_t)-2));
	}

	// struct which calls the monitor is accepting
	struct __waitfor_mask_t {
		// the index of the accepted function, -1 if none
		short * accepted;

		// list of acceptable functions, null if any
		__cfa_anonymous_object( __small_array_t(struct __acceptable_t) );
	};

	struct monitor$ {
		// spinlock to protect internal data
		struct __spinlock_t lock;

		// current owner of the monitor
		struct thread$ * owner;

		// queue of threads that are blocked waiting for the monitor
		__queue_t(struct thread$) entry_queue;

		// stack of conditions to run next once we exit the monitor
		__stack_t(struct __condition_criterion_t) signal_stack;

		// monitor routines can be called recursively, we need to keep track of that
		unsigned int recursion;

		// mask used to know if some thread is waiting for something while holding the monitor
		struct __waitfor_mask_t mask;

		// node used to signal the dtor in a waitfor dtor
		struct __condition_node_t * dtor_node;
	};
	// Wrapper for gdb
	struct cfathread_monitor_t { struct monitor$ debug; };

	struct __monitor_group_t {
		// currently held monitors
		__cfa_anonymous_object( __small_array_t(monitor$*) );

		// last function that acquired monitors
		fptr_t func;
	};

	// Link lists fields
	// instrusive link field for threads
	struct __thread_desc_link {
		struct thread$ * next;
		volatile unsigned long long ts;
	};

	struct thread$ {
		// Core threading fields
		// context that is switch during a __cfactx_switch
		struct __stack_context_t context;

		// Link lists fields
		// instrusive link field for threads
		struct __thread_desc_link link;

		// current execution status for coroutine
		// Possible values are:
		//    - TICKET_BLOCKED (-1) thread is blocked
		//    - TICKET_RUNNING ( 0) thread is running
		//    - TICKET_UNBLOCK ( 1) thread should ignore next block
		volatile int ticket;
		enum __Coroutine_State state:8;
		enum __Preemption_Reason preempted:8;

		bool corctx_flag;

		int last_cpu;

		//SKULLDUGGERY errno is not save in the thread data structure because returnToKernel appears to be the only function to require saving and restoring it

		// pointer to the cluster on which the thread is running
		struct cluster * curr_cluster;

		// preferred ready-queue
		unsigned preferred;

		// coroutine body used to store context
		struct coroutine$  self_cor;

		// current active context
		struct coroutine$ * curr_cor;

		// monitor body used for mutual exclusion
		struct monitor$    self_mon;

		// pointer to monitor with sufficient lifetime for current monitors
		struct monitor$ *  self_mon_p;

		// monitors currently held by this thread
		struct __monitor_group_t monitors;

		// used to put threads on user data structures
		struct {
			struct thread$ * next;
			struct thread$ * back;
		} seqable;

		// used to put threads on dlist data structure
		__cfa_dlink(thread$);

		struct {
			struct thread$ * next;
			struct thread$ * prev;
		} node;

		struct processor * last_proc;

		#if defined( __CFA_WITH_VERIFY__ )
			void * canary;
		#endif
	};
	#ifdef __cforall
		P9_EMBEDDED( thread$, dlink(thread$) )
	#endif
	// Wrapper for gdb
	struct cfathread_thread_t { struct thread$ debug; };

	#ifdef __CFA_DEBUG__
		void __cfaabi_dbg_record_thrd(thread$ & this, bool park, const char prev_name[]);
	#else
		#define __cfaabi_dbg_record_thrd(x, y, z)
	#endif

	#ifdef __cforall
	extern "Cforall" {

		static inline thread$ *& get_next( thread$ & this ) __attribute__((const)) {
			return this.link.next;
		}

		static inline [thread$ *&, thread$ *& ] __get( thread$ & this ) __attribute__((const)) {
			return this.node.[next, prev];
		}

		static inline thread$ * volatile & ?`next ( thread$ * this )  __attribute__((const)) {
			return this->seqable.next;
		}

		static inline thread$ *& Back( thread$ * this ) __attribute__((const)) {
			return this->seqable.back;
		}

		static inline thread$ *& Next( thread$ * this ) __attribute__((const)) {
				return this->seqable.next;
		}

		static inline bool listed( thread$ * this ) {
			return this->seqable.next != 0p;
		}

		static inline void ?{}(__monitor_group_t & this) {
			(this.data){0p};
			(this.size){0};
			(this.func){NULL};
		}

		static inline void ?{}(__monitor_group_t & this, struct monitor$ ** data, __lock_size_t size, fptr_t func) {
			(this.data){data};
			(this.size){size};
			(this.func){func};
		}

		static inline bool ?==?( const __monitor_group_t & lhs, const __monitor_group_t & rhs ) __attribute__((const)) {
			if( (lhs.data != 0) != (rhs.data != 0) ) return false;
			if( lhs.size != rhs.size ) return false;
			if( lhs.func != rhs.func ) return false;

			// Check that all the monitors match
			for( int i = 0; i < lhs.size; i++ ) {
				// If not a match, check next function
				if( lhs[i] != rhs[i] ) return false;
			}

			return true;
		}

		static inline void ?=?(__monitor_group_t & lhs, const __monitor_group_t & rhs) {
			lhs.data = rhs.data;
			lhs.size = rhs.size;
			lhs.func = rhs.func;
		}
	}
	#endif

#endif //_INVOKE_H_
#else //! defined(__CFA_INVOKE_PRIVATE__)
#ifndef _INVOKE_PRIVATE_H_
#define _INVOKE_PRIVATE_H_

	struct machine_context_t {
		void *SP;
		void *FP;
		void *PC;
	};

	// assembler routines that performs the context switch
	extern void __cfactx_invoke_stub( void );
	extern void __cfactx_switch( struct __stack_context_t * from, struct __stack_context_t * to ) asm ("__cfactx_switch");
	// void CtxStore ( void * this ) asm ("CtxStore");
	// void CtxRet   ( void * dst  ) asm ("CtxRet");

#endif //_INVOKE_PRIVATE_H_
#endif //! defined(__CFA_INVOKE_PRIVATE__)
#ifdef __cforall
}
#endif

// Local Variables: //
// mode: c //
// tab-width: 4 //
// End: //
