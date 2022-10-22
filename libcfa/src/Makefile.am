######################## -*- Mode: Makefile-Automake -*- ######################
##
## Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## Makefile.am --
##
## Author           : Peter A. Buhr
## Created On       : Sun May 31 08:54:01 2015
## Last Modified By : Peter A. Buhr
## Last Modified On : Fri Jul 16 16:00:40 2021
## Update Count     : 255
###############################################################################

# create object files in directory with source files
AUTOMAKE_OPTIONS = foreign subdir-objects
ACLOCAL_AMFLAGS  = -I automake

include $(top_srcdir)/../tools/build/cfa.make

libdir = ${CFA_LIBDIR}
lib_LTLIBRARIES = libcfa.la libcfathread.la

VPATH += :../prelude

gdbwaittarget=""

# AM_CFLAGS for all cfa source
# AM_CFAFLAGS for only cfa source
# use -no-include-stdhdr to prevent rebuild cycles
# The built sources must not depend on the installed inst_headers_src
AM_CFAFLAGS = -quiet -cfalib -I$(srcdir)/stdhdr -I$(srcdir)/concurrency $(if $(findstring ${gdbwaittarget}, ${@}), -XCFA --gdb) @CONFIG_CFAFLAGS@
AM_CFLAGS = -g -Wall -Werror=return-type -Wno-unused-function -fPIC -fexceptions -fvisibility=hidden -pthread @ARCH_FLAGS@ @CONFIG_CFLAGS@
AM_CCASFLAGS = -g -Wall -Werror=return-type -Wno-unused-function @ARCH_FLAGS@ @CONFIG_CFLAGS@
CFACC = @CFACC@

#----------------------------------------------------------------------------------------------------------------
if BUILDLIB
inst_headers_nosrc = \
	bitmanip.hfa \
	clock.hfa \
	exception.hfa \
	exception.h \
	gmp.hfa \
	math.trait.hfa \
	math.hfa \
	time_t.hfa \
	bits/algorithm.hfa \
	bits/align.hfa \
	bits/containers.hfa \
	bits/debug.hfa \
	bits/defs.hfa \
	bits/locks.hfa \
	bits/collection.hfa \
	bits/stack.hfa \
	bits/queue.hfa \
	bits/sequence.hfa \
	concurrency/iofwd.hfa \
	concurrency/barrier.hfa \
	containers/array.hfa \
	containers/list.hfa \
	containers/queueLockFree.hfa \
	containers/stackLockFree.hfa \
	containers/string_sharectx.hfa \
	containers/vector2.hfa \
	vec/vec.hfa \
	vec/vec2.hfa \
	vec/vec3.hfa \
	vec/vec4.hfa

inst_headers_src = \
	common.hfa \
	fstream.hfa \
	heap.hfa \
	iostream.hfa \
	iterator.hfa \
	limits.hfa \
	memory.hfa \
	parseargs.hfa \
	parseconfig.hfa \
	rational.hfa \
	stdlib.hfa \
	strstream.hfa \
	time.hfa \
	bits/weakso_locks.hfa \
	algorithms/range_iterator.hfa \
	containers/maybe.hfa \
	containers/pair.hfa \
	containers/result.hfa \
	containers/string.hfa \
	containers/string_res.hfa \
	containers/vector.hfa \
	device/cpu.hfa

libsrc = ${inst_headers_src} ${inst_headers_src:.hfa=.cfa} \
	assert.cfa \
	bits/debug.cfa \
	exception.c \
	interpose.cfa \
	lsda.h \
	startup.cfa \
	startup.hfa \
	virtual.c \
	virtual.h

# not all platforms support concurrency, add option do disable it
inst_thread_headers_nosrc = \
	bits/random.hfa \
	concurrency/clib/cfathread.h \
	concurrency/invoke.h \
	concurrency/future.hfa \
	concurrency/kernel/fwd.hfa \
	concurrency/mutex_stmt.hfa

inst_thread_headers_src = \
	concurrency/alarm.hfa \
	concurrency/coroutine.hfa \
	concurrency/exception.hfa \
	concurrency/kernel.hfa \
	concurrency/kernel/cluster.hfa \
	concurrency/locks.hfa \
	concurrency/monitor.hfa \
	concurrency/mutex.hfa \
	concurrency/thread.hfa

thread_libsrc = ${inst_thread_headers_src} ${inst_thread_headers_src:.hfa=.cfa} \
	interpose_thread.cfa \
	bits/signal.hfa \
	concurrency/clib/cfathread.cfa \
	concurrency/CtxSwitch-@ARCHITECTURE@.S \
	concurrency/invoke.c \
	concurrency/io.cfa \
	concurrency/io/setup.cfa \
	concurrency/io/types.hfa \
	concurrency/io/call.cfa \
	concurrency/iofwd.hfa \
	concurrency/kernel/private.hfa \
	concurrency/kernel/startup.cfa \
	concurrency/preemption.cfa \
	concurrency/preemption.hfa \
	concurrency/ready_queue.cfa \
	concurrency/ready_subqueue.hfa \
	concurrency/snzi.hfa \
	concurrency/stats.cfa \
	concurrency/stats.hfa \
	concurrency/stats.hfa \
	concurrency/pthread.cfa

else
inst_headers_src =
inst_thread_headers_src =
inst_headers_nosrc =
inst_thread_headers_nosrc =
libsrc =
endif


#----------------------------------------------------------------------------------------------------------------
# add dependency to cfa-cpp so all libraries are rebuilt with new translator
#@CFACC@ @CFACPP@ prelude.cfa

# add dependency of cfa files
libobjs = $(addsuffix .lo, $(basename $(filter %.cfa,$(libsrc))))
$(libobjs) : @LOCAL_CFACC@ @CFACPP@ prelude.cfa

thread_libobjs = $(addsuffix .lo, $(basename $(filter %.cfa,$(thread_libsrc))))
$(thread_libobjs) : @LOCAL_CFACC@ @CFACPP@ prelude.cfa


# .deps inclusion is not done automatically by automake for new languages
libdeps = $(join \
	$(addsuffix $(DEPDIR)/ , $(dir $(libobjs) ) ), \
	$(notdir ${libobjs:.lo=.Plo}) \
)

-include $(libdeps)

thread_libdeps = $(join \
	$(addsuffix $(DEPDIR)/ , $(dir $(thread_libobjs) ) ), \
	$(notdir ${thread_libobjs:.lo=.Plo}) \
)

-include $(thread_libdeps)


if ENABLE_DISTCC

../prelude/distribution: @LOCAL_CFACC@ @LOCAL_CC1@ @CFACPP@ ../prelude/defines.hfa ../prelude/gcc-builtins.cf ../prelude/builtins.cf ../prelude/extras.cf ../prelude/prelude.cfa ../prelude/bootloader.c $(srcdir)/../../tools/build/push2dist.sh
	@+make -C ../prelude distribution

prelude.o prelude.lo $(libobjs) $(thread_libobjs) : ../prelude/distribution

endif ENABLE_DISTCC

prelude.o : prelude.cfa extras.cf gcc-builtins.cf builtins.cf @LOCAL_CFACC@ @CFACPP@
	${AM_V_GEN}$(CFACOMPILE) -quiet -XCFA,-l ${<} -c -fvisibility=default -o ${@}

prelude.lo: prelude.cfa extras.cf gcc-builtins.cf builtins.cf @LOCAL_CFACC@ @CFACPP@
	${AM_V_GEN}$(LIBTOOL) $(AM_V_lt) --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=compile \
	$(CFACOMPILE) -quiet -XCFA,-l ${<} -c -fvisibility=default -o ${@}

concurrency/io/call.cfa: $(srcdir)/concurrency/io/call.cfa.in
	${AM_V_GEN}python3 $< > $@

#----------------------------------------------------------------------------------------------------------------
libcfa_la_SOURCES = ${libsrc}
nodist_libcfa_la_SOURCES = prelude.cfa
libcfa_la_LDFLAGS = -version-info @CFA_VERSION@

libcfathread_la_SOURCES = ${thread_libsrc}
libcfathread_la_LDFLAGS = -version-info @CFA_VERSION@

stdhdr = $(shell find $(srcdir)/stdhdr -type f -printf "%p ")

cfa_includedir = $(CFA_INCDIR)
nobase_cfa_include_HEADERS = ${stdhdr} ${inst_headers_src} ${inst_headers_nosrc} ${inst_thread_headers_src} ${inst_thread_headers_nosrc}
EXTRA_DIST = stdhdr
DISTCLEANFILES = $(libdeps) $(thread_libdeps)

#----------------------------------------------------------------------------------------------------------------
maintainer-clean-local:
	-rm -rf ${CFA_INCDIR} ${CFA_LIBDIR}

#distclean-local:
#	find ${builddir} -path '*.Plo' -delete


# $(AM_V_CFA)depbase=`echo $@ | sed 's|[^/]*$$|$(DEPDIR)/&|;s|\.o$$||'`;\
# 	$(CFACOMPILE) -MT $@ -MD -MP -MF $$depbase.Tpo -c -o $@ $< &&\
# 	$(am__mv) $$depbase.Tpo $$depbase.Po
