#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([cfa-cc],[1.0.0],[cforall@plg.uwaterloo.ca])
AC_CONFIG_AUX_DIR([automake])
AC_CONFIG_MACRO_DIRS([automake])
AM_SILENT_RULES([yes])

m4_include([../tools/build/cfa.m4])

AM_INIT_AUTOMAKE([subdir-objects])

# don't use the default CFLAGS as they unconditonnaly add -O2
: ${CFLAGS=""}

# define this to override
AR_FLAGS="cr"

# Allow program name tansformation
# will fill program_transform_name with appropriate sed regex
AC_ARG_PROGRAM

AC_ARG_VAR(ARCHITECTURE,  [The architecture  to use when building libcfa])
AC_ARG_VAR(CONFIGURATION, [The configuration to use when building libcfa, options are: deubg, nodebug, nolib (prelude-only)])
AC_ARG_VAR(DRIVER_DIR, [The path to the cforall driver directory])
AC_ARG_VAR(CFA_VERSION, [The long version of cfa])

AC_ARG_ENABLE(distcc,
	[  --enable-distcc     whether or not to enable distributed compilation],
	enable_distcc=$enableval, enable_distcc=no)

AC_ARG_WITH(bwlimit,
	[  --with-bwlimit=RATE     RATE the maximum rate at which rsync will be limited when using distributed builds],
	DIST_BWLIMIT=$withval, DIST_BWLIMIT=0)

echo -n "checking for distributated build... "
if test x$enable_distcc = xno; then
	CFACC=${DRIVER_DIR}cfa
	echo "no"
else
	tools="$(readlink -m $ac_confdir/)/../tools/build"
	config=$(basename $(readlink -f .))
	echo "$tools/distcc_hash $config"
	CFADIR_HASH=$($tools/distcc_hash $config)
	CFACC="distcc ~/.cfadistcc/${CFADIR_HASH}/cfa"
	echo "yes (hash=${CFADIR_HASH})"
fi
CFACPP=${DRIVER_DIR}cfa-cpp
LOCAL_CFACC=${DRIVER_DIR}cfa
LOCAL_CC1=${DRIVER_DIR}cc1

AM_CONDITIONAL([ENABLE_DISTCC], [test x$enable_distcc = xyes])

AC_SUBST(CFACC)
AC_SUBST(CFACPP)
AC_SUBST(LOCAL_CFACC)
AC_SUBST(LOCAL_CC1)
AC_SUBST(CFADIR_HASH)
AC_SUBST(CFA_VERSION)
AC_SUBST(DIST_BWLIMIT)

#==============================================================================
#Handle specific flags
case $ARCHITECTURE in
	"x64"        ) ARCH_FLAGS="-m64";;
	"x86"        ) ARCH_FLAGS="-m32";;
	"arm"        ) ARCH_FLAGS="";;
esac

AC_SUBST(ARCH_FLAGS)

case $CONFIGURATION in
	"debug"   )
		CONFIG_CFLAGS="-O0 -g"
		CONFIG_CFAFLAGS="-debug"
		CONFIG_BUILDLIB="yes"
	;;
	"nodebug" )
		CONFIG_CFLAGS="-O3 -s"
		CONFIG_CFAFLAGS="-nodebug"
		CONFIG_BUILDLIB="yes"
	;;
	"nolib"   )
		CONFIG_CFLAGS="-O3 -s"
		CONFIG_CFAFLAGS="-nolib"
		CONFIG_BUILDLIB="no"
	;;
	"profile" )
		CONFIG_CFLAGS="-O3 -g -fno-omit-frame-pointer"
		CONFIG_CFAFLAGS="-nodebug"
		CONFIG_BUILDLIB="yes"
	;;
	*)
	AC_MSG_ERROR('Invalid value $CONFIGURATION for \$CONFIGURATION')
	;;
esac

CONFIG_CFAFLAGS="${CONFIG_CFAFLAGS} ${CFAFLAGS}"

AC_SUBST(CONFIG_CFLAGS)
AC_SUBST(CONFIG_CFAFLAGS)
AC_SUBST(CONFIG_BUILDLIB)

AM_CONDITIONAL([BUILDLIB], [test "x${CONFIG_BUILDLIB}" = "xyes"])

AM_T='$(T)'
AC_SUBST(AM_T)

#==============================================================================
#Trasforming cc1 will break compilation
M4CFA_PROGRAM_NAME

#==============================================================================
# Installation paths
M4CFA_PARSE_PREFIX

# Checks for programs.
LT_INIT([disable-static])

AC_PROG_CXX
AC_PROG_CC
AM_PROG_AS
AC_PROG_LIBTOOL
AC_PROG_INSTALL
AC_PROG_MAKE_SET



#io_uring 5.4 and earlier uses defines
#io_uring 5.5 uses enum values
#io_uring 5.6 and later uses probes

AH_TEMPLATE([CFA_HAVE_LINUX_RSEQ_H],[Defined if rseq support is present when compiling libcfathread.])
AC_CHECK_HEADERS([linux/rseq.h], [AC_DEFINE(CFA_HAVE_LINUX_RSEQ_H)])

AH_TEMPLATE([CFA_HAVE_LINUX_LIBRSEQ],[Defined if librseq support is present when compiling libcfathread.])
AC_CHECK_LIB([rseq], [rseq_available], [AC_DEFINE(CFA_HAVE_LINUX_RSEQ_H)], [])

AH_TEMPLATE([CFA_HAVE_LINUX_IO_URING_H],[Defined if io_uring support is present when compiling libcfathread.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_NOP],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_NOP.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_READV],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_READV.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_WRITEV],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_WRITEV.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_FSYNC],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_FSYNC.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_READ_FIXED],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_READ_FIXED.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_WRITE_FIXED],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_WRITE_FIXED.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_POLL_ADD],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_POLL_ADD.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_POLL_REMOVE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_POLL_REMOVE.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_SYNC_FILE_RANGE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_SYNC_FILE_RANGE.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_SENDMSG],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_SENDMSG.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_RECVMSG],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_RECVMSG.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_TIMEOUT],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_TIMEOUT.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_TIMEOUT_REMOVE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_TIMEOUT_REMOVE.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_ACCEPT],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_ACCEPT.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_ASYNC_CANCEL],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_ASYNC_CANCEL.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_LINK_TIMEOUT],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_LINK_TIMEOUT.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_CONNECT],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_CONNECT.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_FALLOCATE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_FALLOCATE.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_OPENAT],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_OPENAT.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_CLOSE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_CLOSE.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_FILES_UPDATE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_FILES_UPDATE.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_STATX],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_STATX.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_READ],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_READ.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_WRITE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_WRITE.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_FADVISE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_FADVISE.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_MADVISE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_MADVISE.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_SEND],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_SEND.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_RECV],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_RECV.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_OPENAT2],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_OPENAT2.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_EPOLL_CTL],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_EPOLL_CTL.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_SPLICE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_SPLICE.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_PROVIDE_BUFFERS],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_PROVIDE_BUFFERS.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_REMOVE_BUFFER],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_REMOVE_BUFFER.])
AH_TEMPLATE([CFA_HAVE_IORING_OP_TEE],[Defined if io_uring support is present when compiling libcfathread and supports the operation IORING_OP_TEE.])
AH_TEMPLATE([CFA_HAVE_IOSQE_FIXED_FILE],[Defined if io_uring support is present when compiling libcfathread and supports the flag FIXED_FILE.])
AH_TEMPLATE([CFA_HAVE_IOSQE_IO_DRAIN],[Defined if io_uring support is present when compiling libcfathread and supports the flag IO_DRAIN.])
AH_TEMPLATE([CFA_HAVE_IOSQE_IO_LINK],[Defined if io_uring support is present when compiling libcfathread and supports the flag IO_LINK.])
AH_TEMPLATE([CFA_HAVE_IOSQE_IO_HARDLINK],[Defined if io_uring support is present when compiling libcfathread and supports the flag IO_HARDLINK.])
AH_TEMPLATE([CFA_HAVE_IOSQE_ASYNC],[Defined if io_uring support is present when compiling libcfathread and supports the flag ASYNC.])
AH_TEMPLATE([CFA_HAVE_IOSQE_BUFFER_SELECT],[Defined if io_uring support is present when compiling libcfathread and supports the flag BUFFER_SELEC.])
AH_TEMPLATE([CFA_HAVE_SPLICE_F_FD_IN_FIXED],[Defined if io_uring support is present when compiling libcfathread and supports the flag SPLICE_F_FD_IN_FIXED.])
AH_TEMPLATE([CFA_HAVE_IORING_SETUP_ATTACH_WQ],[Defined if io_uring support is present when compiling libcfathread and supports the flag IORING_SETUP_ATTACH_WQ.])
AH_TEMPLATE([CFA_HAVE_PREADV2],[Defined if preadv2 support is present when compiling libcfathread.])
AH_TEMPLATE([CFA_HAVE_PWRITEV2],[Defined if pwritev2 support is present when compiling libcfathread.])
AH_TEMPLATE([CFA_HAVE_PWRITEV2],[Defined if pwritev2 support is present when compiling libcfathread.])
AH_TEMPLATE([CFA_HAVE_STATX],[Defined if statx support is present when compiling libcfathread.])
AH_TEMPLATE([CFA_HAVE_OPENAT2],[Defined if openat2 support is present when compiling libcfathread.])
AH_TEMPLATE([__CFA_NO_STATISTICS__],[Defined if libcfathread was compiled without support for statistics.])

define(ioring_ops, [IORING_OP_NOP,IORING_OP_READV,IORING_OP_WRITEV,IORING_OP_FSYNC,IORING_OP_READ_FIXED,IORING_OP_WRITE_FIXED,IORING_OP_POLL_ADD,IORING_OP_POLL_REMOVE,IORING_OP_SYNC_FILE_RANGE,IORING_OP_SENDMSG,IORING_OP_RECVMSG,IORING_OP_TIMEOUT,IORING_OP_TIMEOUT_REMOVE,IORING_OP_ACCEPT,IORING_OP_ASYNC_CANCEL,IORING_OP_LINK_TIMEOUT,IORING_OP_CONNECT,IORING_OP_FALLOCATE,IORING_OP_OPENAT,IORING_OP_CLOSE,IORING_OP_FILES_UPDATE,IORING_OP_STATX,IORING_OP_READ,IORING_OP_WRITE,IORING_OP_FADVISE,IORING_OP_MADVISE,IORING_OP_SEND,IORING_OP_RECV,IORING_OP_OPENAT2,IORING_OP_EPOLL_CTL,IORING_OP_SPLICE,IORING_OP_PROVIDE_BUFFERS,IORING_OP_REMOVE_BUFFER,IORING_OP_TEE])
define(ioring_flags, [IOSQE_FIXED_FILE,IOSQE_IO_DRAIN,IOSQE_IO_LINK,IOSQE_IO_HARDLINK,IOSQE_ASYNC,IOSQE_BUFFER_SELECT,SPLICE_F_FD_IN_FIXED,IORING_SETUP_ATTACH_WQ])

define(ioring_from_decls, [
	m4_foreach([op], [ioring_ops], [
		AC_CHECK_DECL(op, [AC_DEFINE([CFA_HAVE_]op)], [], [[#include <linux/io_uring.h>]])
	])
])

AC_CHECK_HEADERS([linux/io_uring.h], [
	AC_DEFINE(CFA_HAVE_LINUX_IO_URING_H)
	AC_CHECK_HEADER([liburing.h], [
		AC_CHECK_LIB([uring], [io_uring_get_probe], [
			m4_foreach([op], [ioring_ops], [
				AC_CHECK_DECL(op, [
					AC_RUN_IFELSE([
						AC_LANG_PROGRAM(
							[[#include <liburing.h>]],
							[[int main() {]]
							[[	struct io_uring_probe *probe = io_uring_get_probe();]]
							[[	if(io_uring_opcode_supported(probe, ]]op[[))]]
							[[		return 0;]]
							[[	else]]
							[[		return 1;]]
							[[}]]
						)
					],[
						AC_DEFINE([CFA_HAVE_]op)
					],[
						AC_MSG_FAILURE([Check support for] op [ with liburing failed])
					])
				], [], [[#include <linux/io_uring.h>]])
			])
		], [
			ioring_from_decls
		])
	], [
		ioring_from_decls
	])

	# check support for various io_uring flags
	m4_foreach([op], [ioring_flags], [
		AC_CHECK_DECL(op, [AC_DEFINE([CFA_HAVE_]op)], [], [[#include <linux/io_uring.h>]])
	])
])
AC_CHECK_FUNC([preadv2], [AC_DEFINE([CFA_HAVE_PREADV2])])
AC_CHECK_FUNC([pwritev2], [AC_DEFINE([CFA_HAVE_PWRITEV2])])

AC_CONFIG_FILES([
	Makefile
	src/Makefile
	prelude/Makefile
	])
AC_CONFIG_FILES([src/concurrency/io/call.cfa], [python3 ${srcdir}/src/concurrency/io/call.cfa.in > src/concurrency/io/call.cfa])

AC_CONFIG_HEADERS(prelude/defines.hfa)

AC_OUTPUT()

# Final text
AC_MSG_RESULT(Cforall library configuraton completed. Type "make -j 8 install".)
