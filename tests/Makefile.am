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
## Last Modified On : Sat Feb  1 08:20:27 2025
## Update Count     : 201
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
WFLAGS_OPT_LAX = \
	${WFLAGS_OPT_LAX_EXPECT_WARN} \
	${WFLAGS_OPT_LAX_WAITING_ON_303} \
	${WFLAGS_OPT_LAX_TO_INVESTIGATE}

# Tests checking that cfa-cc raises a certain warning, so -Werror is permanently inappropriate
WFLAGS_OPT_LAX_EXPECT_WARN = \
	attr-priority \
	ctrl-flow/loop_else-WRN1 \
	warnings/self-assignment

# These are failing because of trac #303; it is not likely to be fixed soon.
# It only shows up on some configurations and machines.
WFLAGS_OPT_LAX_WAITING_ON_303 = \
	concurrency/actors/dynamic \
	concurrency/actors/executor \
	concurrency/actors/inherit \
	concurrency/actors/inline \
	concurrency/actors/matrixMultiply \
	concurrency/actors/pingpong \
	concurrency/actors/poison \
	concurrency/actors/static \
	concurrency/actors/types \
	io/away_fair \
	io/comp_basic \
	io/comp_fair \
	io/many_read

# Tests that need investigation from the CFA team about why they require lax wflags.  Goal is to eliminate this list.
WFLAGS_OPT_LAX_TO_INVESTIGATE = \
	forall \
	include/includes \
	include/stdincludes \
	references \
	tuple/tupleCast \
	tuple/tupleMember

WFLAGS=${if ${filter ${WFLAGS_OPT_LAX},${@}},${WFLAGS_LAX},${WFLAGS_STRICT}}

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

SYNTAX_ONLY_CODE = attr-priority warnings/self-assignment

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

collections/string-operator-ERR09 : collections/string-operator.cfa
	${CFACOMPILE_SYNTAX} -DTRY_MR09
	-cp ${test} ${abspath ${@}}

collections/string-operator-ERR11 : collections/string-operator.cfa
	${CFACOMPILE_SYNTAX} -DTRY_MR11
	-cp ${test} ${abspath ${@}}

collections/string-operator-ERR13 : collections/string-operator.cfa
	${CFACOMPILE_SYNTAX} -DTRY_MR13
	-cp ${test} ${abspath ${@}}

collections/string-operator-ERR15 : collections/string-operator.cfa
	${CFACOMPILE_SYNTAX} -DTRY_MR15
	-cp ${test} ${abspath ${@}}

collections/string-operator-ERR16 : collections/string-operator.cfa
	${CFACOMPILE_SYNTAX} -DTRY_MR16
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

zero_one-ERR1 : zero_one.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX} -DERR1
	-cp ${test} ${abspath ${@}}

ctrl-flow/loop_else : ctrl-flow/loop_else.cfa ${CFACCBIN}
	${CC} ${AM_CFLAGS} -Wno-superfluous-else $< -o $@

ctrl-flow/loop_else-WRN1 : ctrl-flow/loop_else.cfa ${CFACCBIN}
	${CFACOMPILE_SYNTAX}
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
