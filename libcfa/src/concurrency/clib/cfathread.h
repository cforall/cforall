//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// clib/cfathread.h --
//
// Author           : Thierry Delisle
// Created On       : Tue Sep 22 15:31:20 2020
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#if defined(__cforall) || defined(__cplusplus)
extern "C" {
#endif
	#include <asm/types.h>
	#include <errno.h>
	#include <unistd.h>


	//--------------------
	// Basic types

	typedef struct cluster * cfathread_cluster_t;

	int cfathread_cluster_create(cfathread_cluster_t * cluster);
	cfathread_cluster_t cfathread_cluster_self(void);
	int cfathread_cluster_print_stats(cfathread_cluster_t cluster);
	int cfathread_cluster_add_worker(cfathread_cluster_t cluster, pthread_t* tid, void (*init_routine) (void *), void * arg);
	int cfathread_cluster_pause (cfathread_cluster_t cluster);
	int cfathread_cluster_resume(cfathread_cluster_t cluster);

	//--------------------
	// thread attribute
	typedef struct cfathread_attr {
		cfathread_cluster_t cl;
	} cfathread_attr_t;

	int cfathread_attr_init(cfathread_attr_t * attr) __attribute__((nonnull (1)));
	static inline int cfathread_attr_destroy(cfathread_attr_t * attr) __attribute__((nonnull (1)));
	static inline int cfathread_attr_destroy(cfathread_attr_t * attr) { return 0; }
	static inline int cfathread_attr_setbackground(cfathread_attr_t * attr, int background) __attribute__((nonnull (1)));
	static inline int cfathread_attr_setbackground(cfathread_attr_t * attr, int background) { return 0; }
	static inline int cfathread_attr_setcluster(cfathread_attr_t * attr, cfathread_cluster_t cl) __attribute__((nonnull (1)));
	static inline int cfathread_attr_setcluster(cfathread_attr_t * attr, cfathread_cluster_t cl) { attr->cl = cl; return 0; }

	//--------------------
	// thread type
	struct cfathread_object;
	typedef struct cfathread_object * cfathread_t;

	int cfathread_create( cfathread_t * h, const cfathread_attr_t * a, void *(*main)( void * ), void * arg ) __attribute__((nonnull (1)));
	int cfathread_join( cfathread_t, void ** retval );

	int cfathread_get_errno(void);
	cfathread_t cfathread_self(void);

	int cfathread_usleep(useconds_t usecs);
	int cfathread_sleep(unsigned int secs);

	void cfathread_park( void );
	void cfathread_unpark( cfathread_t );
	void cfathread_yield( void );

	//--------------------
	// mutex and condition
	struct timespec;

	typedef struct cfathread_mutex_attr {
	} cfathread_mutexattr_t;
	typedef struct cfathread_mutex * cfathread_mutex_t;
	int cfathread_mutex_init(cfathread_mutex_t *restrict mut, const cfathread_mutexattr_t *restrict attr) __attribute__((nonnull (1)));
	int cfathread_mutex_destroy(cfathread_mutex_t *mut) __attribute__((nonnull (1)));
	int cfathread_mutex_lock(cfathread_mutex_t *mut) __attribute__((nonnull (1)));
	int cfathread_mutex_trylock(cfathread_mutex_t *mut) __attribute__((nonnull (1)));
	int cfathread_mutex_unlock(cfathread_mutex_t *mut) __attribute__((nonnull (1)));

	typedef struct cfathread_cond_attr {
		// WARNING: adding support for pthread_condattr_setclock would require keeping track of the clock
		// and reading it in cond_timedwait
	} cfathread_condattr_t;
	typedef struct cfathread_condition * cfathread_cond_t;
	int cfathread_cond_init(cfathread_cond_t *restrict cond, const cfathread_condattr_t *restrict attr) __attribute__((nonnull (1)));
	int cfathread_cond_wait(cfathread_cond_t *restrict cond, cfathread_mutex_t *restrict mut) __attribute__((nonnull (1,2)));
	int cfathread_cond_timedwait(cfathread_cond_t *restrict cond, cfathread_mutex_t *restrict mut, const struct timespec *restrict abstime) __attribute__((nonnull (1,2,3)));
	int cfathread_cond_signal(cfathread_cond_t *cond) __attribute__((nonnull (1)));

	//--------------------
	// IO operations
	struct sockaddr;
	struct msghdr;
	int cfathread_socket(int domain, int type, int protocol);
	int cfathread_bind(int socket, const struct sockaddr *address, socklen_t address_len);
	int cfathread_listen(int socket, int backlog);
	int cfathread_accept(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len);
	int cfathread_connect(int socket, const struct sockaddr *address, socklen_t address_len);
	int cfathread_dup(int fildes);
	int cfathread_close(int fildes);
	ssize_t cfathread_sendmsg(int socket, const struct msghdr *message, int flags);
	ssize_t cfathread_write(int fildes, const void *buf, size_t nbyte);
	ssize_t cfathread_recvfrom(int socket, void *restrict buffer, size_t length, int flags, struct sockaddr *restrict address, socklen_t *restrict address_len);
	ssize_t cfathread_read(int fildes, void *buf, size_t nbyte);

#if defined(__cforall) || defined(__cplusplus)
}
#endif
