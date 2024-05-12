#if 0
#include <dlfcn.h>
#include <unistd.h>
#include <stdlib.h>

bool recursion = false;

static char hex[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };

static union {
    void *addr;
    unsigned char bytes[sizeof(void *)];
};

struct Mallocmsg {
    const char start[9];
    char addr[16];
    const char sep[3];
    char size[16];
    const char end[1];
} mallocmsg = {
    'm', 'a', 'l', 'l', 'o', 'c', ' ', '0', 'x',
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ' ', '0', 'x',
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    '\n'
};

void * malloc( size_t size ) {
    if ( recursion ) { write( STDERR_FILENO, "recursion\n", 10 ); abort(); }
    recursion = true;
    __typeof__( ::malloc ) *libc_malloc = (__typeof__( ::malloc ) *)dlsym(RTLD_NEXT, "malloc");
    addr = (void *)size;
    for ( int i = 0, j = 7; i < 16; i += 2, j -= 1 ) {
    	mallocmsg.size[i] = hex[bytes[j] >> 4];
    	mallocmsg.size[i + 1] = hex[bytes[j] & 0x0f];
    } // for
    addr = libc_malloc( size );
    for ( int i = 0, j = 7; i < 16; i += 2, j -= 1 ) {
	mallocmsg.addr[i] = hex[bytes[j] >> 4];
	mallocmsg.addr[i + 1] = hex[bytes[j] & 0x0f];
    } // for
    write( STDERR_FILENO, &mallocmsg, sizeof(mallocmsg) );
    recursion = false;
    return addr;
}

struct Freemsg {
    const char start[7];
    char addr[16];
    const char end[1];
} freemsg = {
    'f', 'r', 'e', 'e', ' ', '0', 'x',
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    '\n'
};

void free( void * x ) {
    if ( recursion ) { write( STDERR_FILENO, "recursion\n", 10 ); abort(); }
    recursion = true;
    __typeof__( ::free ) *libc_free = (__typeof__( ::free ) *)dlsym(RTLD_NEXT, "free");
    addr = x;
    for ( int i = 0, j = 7; i < 16; i += 2, j -= 1 ) {
	freemsg.addr[i] = hex[bytes[j] >> 4];
	freemsg.addr[i + 1] = hex[bytes[j] & 0x0f];
    } // for
    write( STDERR_FILENO, &freemsg, sizeof(freemsg) );
    recursion = false;
    libc_free( addr );
}
#endif // 0
