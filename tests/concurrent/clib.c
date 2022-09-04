#include <stdio.h>
#include <stdlib.h>
#include <clib/cfathread.h>
#include <bits/defs.hfa>

extern "C" {
void _exit(int status);
}

_Thread_local struct drand48_data buffer = { 0 };
int myrand() {
	long int result;
	lrand48_r(&buffer, &result);
	return result;
}


enum Constants { blocked_size = 20 };
cfathread_t volatile blocked[blocked_size];

void * Worker( void * ) {
	for(int i = 0; i < 1000; i++) {
		int idx = myrand() % blocked_size;
		if(blocked[idx]) {
			cfathread_t thrd = __atomic_exchange_n(&blocked[idx], NULL, __ATOMIC_SEQ_CST);
			cfathread_unpark( thrd );
		} else {
			cfathread_t thrd = __atomic_exchange_n(&blocked[idx], cfathread_self(), __ATOMIC_SEQ_CST);
			cfathread_unpark( thrd );
			cfathread_park();
		}
	}
	printf("Done\n");
	return NULL;
}

volatile bool stop;
void * Unparker( void * ) {
	while(!stop) {
		int idx = myrand() % blocked_size;
		cfathread_t thrd = __atomic_exchange_n(&blocked[idx], NULL, __ATOMIC_SEQ_CST);
		cfathread_unpark( thrd );
		int r = myrand() % 20;
		for( int i = 0; i < r; i++ ) {
			cfathread_yield();
		}
	}
	printf("Done Unparker\n");
	return NULL;
}


int main() {
	stop = false;
	for(int i = 0; i < blocked_size; i++) {
		blocked[i] = NULL;
	}

	cfathread_cluster_t cl = cfathread_cluster_self();

	cfathread_cluster_add_worker( cl, NULL, NULL, NULL );
	cfathread_cluster_add_worker( cl, NULL, NULL, NULL );
	cfathread_cluster_add_worker( cl, NULL, NULL, NULL );

	cfathread_attr_t attr;
	cfathread_attr_init(&attr);
	cfathread_attr_setcluster(&attr, cl);

	cfathread_t u;
	cfathread_create( &u, &attr, Unparker, NULL );
	{
		cfathread_t t[20];
		for(int i = 0; i < 20; i++) {
			cfathread_create( &t[i], &attr, Worker, NULL );
		}
		for(int i = 0; i < 20; i++) {
			cfathread_join( t[i], NULL );
		}
	}
	stop = true;
	cfathread_join(u, NULL);
	cfathread_attr_destroy(&attr);
	fflush(stdout);
	_exit(0);
}