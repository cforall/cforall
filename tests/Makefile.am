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
## Created On       : Sun May 31 09:08:15 2015
## Last Modified By : Peter A. Buhr
## Last Modified On : Thu Dec 19 22:11:14 2024
## Update Count     : 200
###############################################################################

AUTOMAKE_OPTIONS = foreign    # do not require all the GNU file names
ACLOCAL_AMFLAGS  = -I automake

include $(top_srcdir)/tools/build/cfa.make

DEFAULT_INCLUDES = -I${abs_srcdir}

debug ?= yes
installed ?= no
ARCH = ${if ${arch},"--arch=${arch}"}
arch_support = "x86/x64/arm"
TIMEOUT = ${if ${timeout},"--timeout=${timeout}"}
GLOBAL_TIMEOUT = ${if ${global-timeout},"--global-timeout=${global-timeout}"}
ARCHIVE_ERRORS = ${if ${archive-errors},"--archive-errors=${archive-errors}"}

DEBUG_FLAGS = -debug -g -O0

quick_test = avl_test operators numericConstants expression enum array typeof cast raii/dtor-early-exit raii/init_once attributes meta/dumpable

TEST_PY = python3 ${builddir}/test.py

WFLAGS_STRICT = \
	-Wall \
	-Wextra \
	-Werror

WFLAGS_LAX = \
	-Wall \
	-Werror=return-type \
	-Wno-unused-function \
	-Wno-psabi

# These tests opt for lax wflags.  (By default, new tests get strict wflags.)
# Indented list entries are finer-grained targets under the test.
# Making this association implicit would be ideal, but requires learning more automake than is affordable.
WFLGAS_OPT_LAX = \
	alloc \
	alloc2 \
	array \
	array-collections/array-raii-c \
	array-collections/array-raii-cfa \
	array-collections/boxed \
		array-collections/boxed% \
	array-collections/dimexpr-match-c \
	array-collections/dimexpr-match-cfa \
	array-ERR1 \
	array-ERR2 \
	array-ERR3 \
	attributes \
	attr-priority \
	builtins/sync \
	cast \
	collections/atomic_mpsc \
	collections/multi_list \
	collections/queue \
	collections/sequence \
	collections/stack \
	collections/vector-demo \
	concurrency/actors/dynamic \
	concurrency/actors/executor \
	concurrency/actors/inherit \
	concurrency/actors/inline \
	concurrency/actors/matrixMultiply \
	concurrency/actors/pingpong \
	concurrency/actors/poison \
	concurrency/actors/static \
	concurrency/actors/types \
	concurrency/barrier/generation \
	concurrency/barrier/last \
	concurrency/barrier/order \
	concurrency/channels/big_elems \
	concurrency/channels/churn \
	concurrency/channels/contend \
	concurrency/channels/daisy_chain \
	concurrency/channels/hot_potato \
	concurrency/channels/ping_pong \
	concurrency/channels/pub_sub \
	concurrency/channels/zero_size \
	concurrency/cluster \
	concurrency/cofor \
	concurrency/coroutineYield \
	concurrency/examples/boundedBufferEXT \
	concurrency/examples/boundedBufferINT \
	concurrency/futures/abandon \
	concurrency/futures/multi \
	concurrency/futures/select_future \
	concurrency/futures/typed \
	concurrency/futures/wait_any \
	concurrency/join \
	concurrency/lockfree_stack \
	concurrency/migrate \
	concurrency/monitor \
	concurrency/multi-monitor \
	concurrency/mutexstmt/locks \
	concurrency/mutexstmt/monitors \
	concurrency/mutexstmt/tuple \
	concurrency/once \
	concurrency/preempt \
	concurrency/pthread/bounded_buffer \
	concurrency/pthread/pthread_attr_test \
	concurrency/pthread/pthread_cond_test \
	concurrency/pthread/pthread_demo_create_join \
	concurrency/pthread/pthread_demo_lock \
	concurrency/pthread/pthread_key_test \
	concurrency/pthread/pthread_once_test \
	concurrency/readyQ/leader_spin \
	concurrency/signal/block \
	concurrency/signal/disjoint \
	concurrency/signal/wait \
	concurrency/sleep \
	concurrency/suspend_then \
	concurrency/thread \
	concurrency/unified_locking/block_spin_lock \
	concurrency/unified_locking/exp_backoff \
	concurrency/unified_locking/fast_block_lock \
	concurrency/unified_locking/futex_mutex \
	concurrency/unified_locking/locks \
	concurrency/unified_locking/mcs \
	concurrency/unified_locking/mcs_block_spin_lock \
	concurrency/unified_locking/mcs_spin \
	concurrency/unified_locking/pthread_locks \
	concurrency/unified_locking/simple_owner_lock \
	concurrency/unified_locking/spin_queue_lock \
	concurrency/unified_locking/timeout_lock \
	concurrency/waitfor/barge \
	concurrency/waitfor/parse \
	concurrency/waitfor/statment \
	concurrency/waitfor/when \
	concurrency/waituntil/all_types \
	concurrency/waituntil/basic_else \
	concurrency/waituntil/channel_close \
	concurrency/waituntil/channel_zero_size \
	concurrency/waituntil/channels \
	concurrency/waituntil/futures \
	concurrency/waituntil/locks \
	concurrency/waituntil/one_chan \
	concurrency/waituntil/repeat_close \
	concurrency/waituntil/timeout \
	configs/parsebools \
	configs/parsenums \
	configs/usage \
	coroutine/raii \
	ctrl-flow/goto \
	ctrl-flow/ifwhileCtl \
	ctrl-flow/labelledExit \
	ctrl-flow/loop_else \
	designations \
	enum \
	enum_tests/inc-dec \
	enum_tests/planet \
	enum_tests/structEnum \
	exceptions/cardgame \
	exceptions/defaults \
	exceptions/defaults-threads \
	exceptions/hotpotato \
	exceptions/hotpotato_checked \
	exceptions/pingpong_nonlocal \
	exceptions/polymorphic \
	exceptions/try-leave-catch \
	exceptions/virtual-poly \
	expression \
	forall \
	function-operator \
	gmp \
	heap \
	identFuncDeclarator \
	identParamDeclarator \
	include/includes \
	include/stdincludes \
	include/vector-collections \
	include/vector-fstream \
	include/vector-sequence \
	init1 \
	init1-ERROR \
	io/away_fair \
	io/comp_basic \
	io/comp_fair \
	io/io-acquire \
	io/io-acquire2 \
	io/io-acquire-in \
	io/io-acquire-no-io \
	io/io-acquire-out \
	io/manipulatorsInput \
	io/manipulatorsInput-uchunk \
	io/many_read \
	limits \
	linking/io-acquire \
	linking/mangling/anon \
		linking/mangling/lib.o \
		linking/mangling/main.o \
	linkonce \
		link-once/% \
	malloc \
	math \
	mathX \
	maybe \
	minmax \
	nested-types \
	nested-types-ERR1 \
	nested-types-ERR2 \
	numericConstants \
	operators \
	poly-d-cycle \
	poly-many-arsz \
	poly-member \
	polymorphism \
	poly-o-cycle \
	PRNG \
	quasiKeyword \
	quotedKeyword \
	raii/boxed-types \
	raii/ctor-autogen \
	raii/dtor-early-exit \
	raii/init_once \
	raii/partial \
	references \
	result \
	shortCircuit \
	sizeof \
	smart-pointers \
	sum \
	switch \
	tuple/tupleAssign \
	tuple/tupleCast \
	tuple/tupleMember \
	tuple/tuplePolymorphism \
	tuple/tupleVariadic \
	typedefRedef \
	typedefRedef-ERR1 \
	typeof \
	userLiterals \
	variableDeclarator \
	vector \
	warnings/self-assignment

WFLAGS=${if ${filter ${WFLGAS_OPT_LAX},${@}},${WFLAGS_LAX},${WFLAGS_STRICT}}

# applies to both programs
# since automake doesn't have support for CFA we have to
AM_CFLAGS = ${if ${test}, 2> ${test}, } \
	-fdebug-prefix-map=${abspath ${abs_srcdir}}= \
	-fdebug-prefix-map=/tmp= \
	-fno-diagnostics-show-caret \
	-g \
	$(WFLAGS) \
	-quiet @CFA_FLAGS@

AM_CFAFLAGS = -XCFA --deterministic-out

# get the desired cfa to test
TARGET_CFA = ${if ${filter ${installed},yes}, @CFACC_INSTALL@, @CFACC@}

# adjust CC to current flags
CC = LC_ALL=C ${if ${DISTCC_CFA_PATH},distcc ${DISTCC_CFA_PATH} ${ARCH_FLAGS} ,${TARGET_CFA} ${DEBUG_FLAGS} ${ARCH_FLAGS}}
CFACC = ${CC}

# get local binary for depedencies
CFACCBIN = @CFACC@

# adjusted CC but without the actual distcc call
CFACCLOCAL = ${if ${DISTCC_CFA_PATH},${DISTCC_CFA_PATH} ${ARCH_FLAGS} ,${TARGET_CFA} ${DEBUG_FLAGS} ${ARCH_FLAGS}}
CFACCLINK = ${CFACCLOCAL} -quiet ${if ${test}, 2> ${test}, } ${${shell echo "${@}_FLAGSLD" | sed 's/-\|\//_/g'}}

PRETTY_PATH = mkdir -p ${dir ${abspath ${@}}} && cd ${srcdir} &&

.PHONY : concurrency list .validate .test_makeflags
.INTERMEDIATE : .validate .validate.cfa .test_makeflags
EXTRA_PROGRAMS = array-collections/boxed avl_test linkonce linking/mangling/anon .dummy_hack # build but do not install
EXTRA_DIST = test.py \
	pybin/__init__.py \
	pybin/print-core.gdb \
	pybin/settings.py \
	pybin/test_run.py \
	pybin/tools.py \
	long_tests.hfa \
	array-collections/boxed.hfa \
	array-collections/boxed.cases.hfa \
	avltree/avl-private.h \
	avltree/avl.h \
	exceptions/except-io.hfa \
	exceptions/with-threads.hfa \
	meta/fork+exec.hfa \
	concurrency/clib_tls.c \
	concurrency/clib.c \
	concurrency/unified_locking/mutex_test.hfa \
	concurrency/channels/parallel_harness.hfa \
	array-collections/dimexpr-match.hfa \
	array-collections/dimexpr-match-detail.sh \
	array-collections/array-raii.hfa

dist-hook:
	echo "Gathering test files"
	for file in `${TEST_PY} --list-dist`; do \
		if ls ${srcdir}/$${file} > /dev/null 2>&1; then \
			${MKDIR_P} $$(dirname ${distdir}/$${file}); \
			cp -df ${srcdir}/$${file} $$(dirname ${distdir}/$${file}); \
		fi; \
	done

array_collections_boxed_SOURCES = array-collections/boxed.main.cfa array-collections/boxed.bookend.cfa
avl_test_SOURCES = avltree/avl_test.cfa avltree/avl0.cfa avltree/avl1.cfa avltree/avl2.cfa avltree/avl3.cfa avltree/avl4.cfa avltree/avl-private.cfa
linkonce_SOURCES = link-once/main.cfa link-once/partner.cfa
linking_mangling_anon_SOURCES = linking/mangling/header.hfa linking/mangling/lib.cfa linking/mangling/main.cfa
# automake doesn't know we still need C/CPP rules so pretend like we have a C program
nodist__dummy_hack_SOURCES = .dummy_hack.c .dummy_hackxx.cpp

#----------------------------------------------------------------------------------------------------------------

# '@' => do not echo command (SILENT), '+' => allows recursive make from within python program
all-local : # This name is important to automake and implies the default build target.
	@+${TEST_PY} --debug=${debug} --install=${installed} ${ARCHIVE_ERRORS} ${TIMEOUT} ${GLOBAL_TIMEOUT} ${ARCH} --all

tests : all-local # synonym

install : all-local  # synonym, PAB only

quick :
	@+${TEST_PY} --debug=${debug} --install=${installed} ${ARCHIVE_ERRORS} ${ARCH} ${quick_test}

concurrency :
	@+${TEST_PY} --debug=${debug} --install=${installed} ${ARCHIVE_ERRORS} ${TIMEOUT} ${GLOBAL_TIMEOUT} ${ARCH} -Iconcurrency

list :
	@+${TEST_PY} --list

help :
	@echo "user targets:"
	@echo "    Run the complete test suite."
	@echo "    $$ make (null) / tests [debug=yes/no] [installed=yes/no] [archive-errors=dump-dir] [timeout=seconds] [global-timeout=seconds] [arch=${arch_support}]"
	@echo ""
	@echo "    Run the short (quick) test suite."
	@echo "    $$ make quick [debug=yes/no] [installed=yes/no] [archive-errors=dump-dir] [arch=${arch_support}]"
	@echo ""
	@echo "    Run the concurrency test suite."
	@echo "    $$ make concurrency [debug=yes/no] [installed=yes/no] [archive-errors=dump-dir] [timeout=seconds] [global-timeout=seconds] [arch=${arch_support}]"
	@echo ""
	@echo "    List all tests in the test suite."
	@echo "    $$ make list"

mostlyclean-local :
	find ${builddir} -not -path './__pycache__/*' -path '*.o' -delete
	find ${builddir} -not -path './__pycache__/*' -path '*/.err/*.log' -delete
	find ${builddir} -not -path './__pycache__/*' -path '*/.out/*.log' -delete
	rm -f ${EXTRA_PROGRAMS}
	rm -rf __pycache__

distclean-local :
	find ${builddir} -path '*.Po' -delete

.test_makeflags:
	@echo "${MAKEFLAGS}"

.validate: .validate.cfa
	${CFACOMPILE} .validate.cfa -fsyntax-only -Wall -Wextra -Werror

.validate.cfa:
	@echo "int main() { return 0; }" > ${@}

# automake doesn't know we still need C rules so pretend like we have a C program
.dummy_hack.c:
	@echo "int main() { return 0; }" > ${@}

.dummy_hackxx.cpp:
	@echo "int bar() { return 0; }" > ${@}

#----------------------------------------------------------------------------------------------------------------

# Use for all tests, make sure the path are correct and all flags are added
CFACOMPILETEST=${PRETTY_PATH} ${CFACOMPILE} ${shell realpath --relative-to=${srcdir} ${<}} ${${shell echo "${@}_FLAGSCFA" | sed 's/-\|\//_/g'}}

#----------------------------------------------------------------------------------------------------------------

# implicit rule so not all test require a rule
# split into two steps to support compiling remotely using distcc
# don't use distcc to do the linking because distcc doesn't do linking
% : %.cfa ${CFACCBIN}
	${CFACOMPILETEST} -c -o ${abspath ${@}}.o -DIN_DIR="${abspath ${dir ${<}}}/.in/"
	${CFACCLINK} ${@}.o -o ${abspath ${@}}
	rm ${abspath ${@}}.o

# implicit rule for c++ test
# convient for testing the testsuite itself but not actuall used
% : %.cpp
	${PRETTY_PATH} ${CXXCOMPILE} ${shell realpath --relative-to=${srcdir} ${<}} -o ${abspath ${@}}

#------------------------------------------------------------------------------
# TARGETS WITH CUSTOM FLAGS
#------------------------------------------------------------------------------
# custom libs
gmp_FLAGSLD= -lgmp

#------------------------------------------------------------------------------
# Generated code
GENERATED_CODE = declarationSpecifier gccExtensions extension attributes functions KRfunctions
${GENERATED_CODE} : % : %.cfa ${CFACCBIN}
	${CFACOMPILETEST} -CFA -XCFA -p -c -fsyntax-only -o ${abspath ${@}}

#------------------------------------------------------------------------------
# CUSTOM TARGET
#------------------------------------------------------------------------------
# tests that just validate syntax and compiler output should be compared to stderr
CFACOMPILE_SYNTAX = ${CFACOMPILETEST} -Wno-unused-variable -Wno-unused-label -c -fsyntax-only -o ${abspath ${@}}

SYNTAX_ONLY_CODE = \
	array cast expression identFuncDeclarator init1 limits nested-types numericConstants opt-params quasiKeyword switch typedefRedef variableDeclarator \
	builtins/sync concurrency/waitfor/parse ctrl-flow/labelledExit include/stdincludes include/includes warnings/self-assignment

${SYNTAX_ONLY_CODE} : % : %.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX}
	${if ${test}, cp ${test} ${abspath ${@}}, }

# expected failures
# use custom target since they require a custom define *and* have a name that doesn't match the file

array-ERR1 : array.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR1
	-cp ${test} ${abspath ${@}}

array-ERR2 : array.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR2
	-cp ${test} ${abspath ${@}}

array-ERR3 : array.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR3
	-cp ${test} ${abspath ${@}}

array-collections/dimexpr-match-c-ERRS : array-collections/dimexpr-match-c.cfa
	${CFACOMPILE_SYNTAX} -DERRS
	-cp ${test} ${abspath ${@}}

array-collections/dimexpr-match-cfa-ERRS : array-collections/dimexpr-match-cfa.cfa
	${CFACOMPILE_SYNTAX} -DERRS
	-cp ${test} ${abspath ${@}}

alloc-ERROR : alloc.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR1
	-cp ${test} ${abspath ${@}}

init1-ERROR : init1.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR1
	-cp ${test} ${abspath ${@}}

typedefRedef-ERR1 : typedefRedef.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR1
	-cp ${test} ${abspath ${@}}

nested-types-ERR1 : nested-types.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR1
	-cp ${test} ${abspath ${@}}

nested-types-ERR2 : nested-types.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR2
	-cp ${test} ${abspath ${@}}

raii/memberCtors-ERR1 : raii/memberCtors.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR1
	-cp ${test} ${abspath ${@}}

raii/ctor-autogen-ERR1 : raii/ctor-autogen.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR1
	-cp ${test} ${abspath ${@}}

raii/dtor-early-exit-ERR1 : raii/dtor-early-exit.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR1
	-cp ${test} ${abspath ${@}}

raii/dtor-early-exit-ERR2 : raii/dtor-early-exit.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR2
	-cp ${test} ${abspath ${@}}

raii/partial-ERR1 : raii/partial.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR1
	-cp ${test} ${abspath ${@}}

# Exception Tests
# Test with libcfathread; it changes how storage works.

exceptions/%-threads : exceptions/%.cfa ${CFACCBIN}
	${CFACOMPILETEST} -include exceptions/with-threads.hfa -c -o ${abspath ${@}}.o
	${CFACCLOCAL} ${${shell echo "${@}_FLAGSLD" | sed 's/-\|\//_/g'}} ${abspath ${@}}.o -o ${abspath ${@}}

# Linking tests
# Meta tests to make sure we see linking errors (can't compile with -O2 since it may multiply number of calls)
linking/linkerror : linking/linkerror.cfa ${CFACCBIN}
	${CFACOMPILETEST} -O0 -c -o ${abspath ${@}}.o
	${CFACCLINK}  -O0 ${@}.o -o ${abspath ${@}}
	rm ${abspath ${@}}.o

#------------------------------------------------------------------------------
# Other targets
