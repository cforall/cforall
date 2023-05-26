#include <stdio.h>
#include <stdlib.h>
#include <clib/cfathread.h>
#include <bits/defs.hfa>
#include <time_t.hfa>

extern "C" {
void _exit(int status);
}

Duration default_preemption(){
	return 0;
}


__thread int checkval = 0xBAADF00D;

void init(void * ) {
	printf("Local Init\n");
	checkval = 0xFEEDFACE;
}

void * checker( void * ) {
	for(int i = 0; i < 50; i++) {
		if(checkval != 0xFeedFace) {
			printf("Bad Food!\n");
		}
		cfathread_yield();
	}
	printf("Done\n");
	return NULL;
}

int main() {
	init(NULL);
	cfathread_cluster_t cl = cfathread_cluster_self();

	cfathread_cluster_add_worker( cl, NULL, init, NULL );
	cfathread_cluster_add_worker( cl, NULL, init, NULL );
	cfathread_cluster_add_worker( cl, NULL, init, NULL );

	cfathread_attr_t attr;
	cfathread_attr_init(&attr);
	cfathread_attr_setcluster(&attr, cl);
	{
		printf("Starting Checkers\n");
		cfathread_t t[7];
		for(int i = 0; i < 7; i++) {
			cfathread_create( &t[i], &attr, checker, NULL );
		}
		for(int i = 0; i < 7; i++) {
			cfathread_join( t[i], NULL );
		}
	}
	cfathread_attr_destroy(&attr);
	fflush(stdout);
	_exit(0);
}