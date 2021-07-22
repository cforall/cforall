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
## Last Modified On : Sat Jun  5 14:49:25 2021
## Update Count     : 92
###############################################################################

AUTOMAKE_OPTIONS = foreign    # do not require all the GNU file names
ACLOCAL_AMFLAGS  = -I automake

include $(top_srcdir)/tools/build/cfa.make

DEFAULT_INCLUDES = -I${abs_srcdir}

debug=yes
installed=no
archiveerrors=

DEBUG_FLAGS=-debug -g -O0

quick_test=avl_test operators numericConstants expression enum array typeof cast raii/dtor-early-exit raii/init_once attributes

concurrent=
timeouts=

TEST_PY = python3 ${builddir}/test.py

# applies to both programs
# since automake doesn't have support for CFA we have to
AM_CFLAGS = $(if $(test), 2> $(test), ) \
	-fdebug-prefix-map=$(abspath ${abs_srcdir})= \
	-fdebug-prefix-map=/tmp= \
	-fno-diagnostics-show-caret \
	-g \
	-Wall \
	-Wno-unused-function \
	-Wno-psabi \
	-quiet @CFA_FLAGS@

AM_CFAFLAGS = -XCFA --deterministic-out

# get the desired cfa to test
TARGET_CFA = $(if $(filter $(installed),yes), @CFACC_INSTALL@, @CFACC@)

# adjust CC to current flags
CC = LC_ALL=C $(if $(DISTCC_CFA_PATH),distcc $(DISTCC_CFA_PATH) ${ARCH_FLAGS} ${AST_FLAGS},$(TARGET_CFA) ${DEBUG_FLAGS} ${ARCH_FLAGS} ${AST_FLAGS})
CFACC = $(CC)

# get local binary for depedencies
CFACCBIN = @CFACC@

# adjusted CC but without the actual distcc call
CFACCLOCAL = $(if $(DISTCC_CFA_PATH),$(DISTCC_CFA_PATH) ${ARCH_FLAGS} ${AST_FLAGS},$(TARGET_CFA) ${DEBUG_FLAGS} ${ARCH_FLAGS} ${AST_FLAGS})
CFACCLINK = $(CFACCLOCAL) -quiet $(if $(test), 2> $(test), ) $($(shell echo "${@}_FLAGSLD" | sed 's/-\|\//_/g'))

PRETTY_PATH=mkdir -p $(dir $(abspath ${@})) && cd ${srcdir} &&

.PHONY: list .validate
.INTERMEDIATE: .validate .validate.cfa
EXTRA_PROGRAMS = avl_test linkonce .dummy_hack # build but do not install
EXTRA_DIST = test.py \
	pybin/__init__.py \
	pybin/print-core.gdb \
	pybin/settings.py \
	pybin/test_run.py \
	pybin/tools.py \
	long_tests.hfa \
	io/.in/io.data \
	io/.in/many_read.data \
	avltree/avl.h \
	avltree/avl-private.h \
	concurrent/clib.c \
	concurrent/clib_tls.c \
	exceptions/with-threads.hfa \
	exceptions/except-io.hfa \
	unified_locking/mutex_test.hfa

dist-hook:
	echo "Gathering test files"
	for file in `${TEST_PY} --list-dist`; do \
		if test -f ${srcdir}/$${file}; then \
			$(MKDIR_P) $$(dirname ${distdir}/$${file}); \
			cp -df ${srcdir}/$${file} ${distdir}/$${file}; \
		fi; \
	done

avl_test_SOURCES = avltree/avl_test.cfa avltree/avl0.cfa avltree/avl1.cfa avltree/avl2.cfa avltree/avl3.cfa avltree/avl4.cfa avltree/avl-private.cfa
linkonce_SOURCES = link-once/main.cfa link-once/partner.cfa
# automake doesn't know we still need C/CPP rules so pretend like we have a C program
nodist__dummy_hack_SOURCES = .dummy_hack.c .dummy_hackxx.cpp

#----------------------------------------------------------------------------------------------------------------
all-local :
	@+${TEST_PY} --debug=${debug} --install=${installed} --archive-errors=${archiveerrors} ${concurrent} ${timeouts} ${quick_test}

all-tests :
	@+${TEST_PY} --debug=${debug} --install=${installed} --archive-errors=${archiveerrors} ${concurrent} ${timeouts} --all # '@' => do not echo command (SILENT), '+' => allows recursive make from within python program

mostlyclean-local :
	find ${builddir} -not -path './__pycache__/*' -path '*.o' -delete
	find ${builddir} -not -path './__pycache__/*' -path '*/.err/*.log' -delete
	find ${builddir} -not -path './__pycache__/*' -path '*/.out/*.log' -delete
	rm -f ${EXTRA_PROGRAMS}
	rm -rf __pycache__

distclean-local :
	find ${builddir} -path '*.Po' -delete

list :
	@+${TEST_PY} --list ${concurrent}

.validate: .validate.cfa
	$(CFACOMPILE) .validate.cfa -fsyntax-only -Wall -Wextra -Werror

.validate.cfa:
	@echo "int main() { return 0; }" > ${@}

# automake doesn't know we still need C rules so pretend like we have a C program
.dummy_hack.c:
	@echo "int main() { return 0; }" > ${@}

.dummy_hackxx.cpp:
	@echo "int bar() { return 0; }" > ${@}

concurrency :
	@+${TEST_PY} --debug=${debug}  --install=${installed} -Iconcurrent

#----------------------------------------------------------------------------------------------------------------

# Use for all tests, make sure the path are correct and all flags are added
CFACOMPILETEST=$(PRETTY_PATH) $(CFACOMPILE) $(shell realpath --relative-to=${srcdir} ${<}) $($(shell echo "${@}_FLAGSCFA" | sed 's/-\|\//_/g'))

#----------------------------------------------------------------------------------------------------------------

# implicit rule so not all test require a rule
# split into two steps to support compiling remotely using distcc
# don't use distcc to do the linking because distcc doesn't do linking
% : %.cfa $(CFACCBIN)
	$(CFACOMPILETEST) -c -o $(abspath ${@}).o -DIN_DIR="$(abspath $(dir ${<}))/.in/"
	$(CFACCLINK) ${@}.o -o $(abspath ${@})
	rm $(abspath ${@}).o

# implicit rule for c++ test
# convient for testing the testsuite itself but not actuall used
% : %.cpp
	$(PRETTY_PATH) $(CXXCOMPILE) $(shell realpath --relative-to=${srcdir} ${<}) -o $(abspath ${@})

#------------------------------------------------------------------------------
# TARGETS WITH CUSTOM FLAGS
#------------------------------------------------------------------------------
# custom libs
gmp_FLAGSLD= -lgmp

#------------------------------------------------------------------------------
# Generated code
GENERATED_CODE = declarationSpecifier gccExtensions extension attributes functions KRfunctions
$(GENERATED_CODE): % : %.cfa $(CFACCBIN)
	$(CFACOMPILETEST) -CFA -XCFA -p -c -fsyntax-only -o $(abspath ${@})

#------------------------------------------------------------------------------
# CUSTOM TARGET
#------------------------------------------------------------------------------
# tests that just validate syntax and compiler output should be compared to stderr
CFACOMPILE_SYNTAX = $(CFACOMPILETEST) -Wno-unused-variable -Wno-unused-label -c -fsyntax-only -o $(abspath ${@})

SYNTAX_ONLY_CODE = expression typedefRedef variableDeclarator switch numericConstants identFuncDeclarator forall \
	init1 limits nested-types cast labelledExit array quasiKeyword include/stdincludes include/includes builtins/sync warnings/self-assignment
$(SYNTAX_ONLY_CODE): % : %.cfa $(CFACCBIN)
	$(CFACOMPILE_SYNTAX)
	$(if $(test), cp $(test) $(abspath ${@}), )

# expected failures
# use custom target since they require a custom define *and* have a name that doesn't match the file
alloc-ERROR : alloc.cfa $(CFACCBIN)
	$(CFACOMPILE_SYNTAX) -DERR1
	-cp $(test) $(abspath ${@})

init1-ERROR : init1.cfa $(CFACCBIN)
	$(CFACOMPILE_SYNTAX) -DERR1
	-cp $(test) $(abspath ${@})

typedefRedef-ERR1 : typedefRedef.cfa $(CFACCBIN)
	$(CFACOMPILE_SYNTAX) -DERR1
	-cp $(test) $(abspath ${@})

nested-types-ERR1 : nested-types.cfa $(CFACCBIN)
	$(CFACOMPILE_SYNTAX) -DERR1
	-cp $(test) $(abspath ${@})

nested-types-ERR2 : nested-types.cfa $(CFACCBIN)
	$(CFACOMPILE_SYNTAX) -DERR2
	-cp $(test) $(abspath ${@})

raii/memberCtors-ERR1 : raii/memberCtors.cfa $(CFACCBIN)
	$(CFACOMPILE_SYNTAX) -DERR1
	-cp $(test) $(abspath ${@})

raii/ctor-autogen-ERR1 : raii/ctor-autogen.cfa $(CFACCBIN)
	$(CFACOMPILE_SYNTAX) -DERR1
	-cp $(test) $(abspath ${@})

raii/dtor-early-exit-ERR1 : raii/dtor-early-exit.cfa $(CFACCBIN)
	$(CFACOMPILE_SYNTAX) -DERR1
	-cp $(test) $(abspath ${@})

raii/dtor-early-exit-ERR2 : raii/dtor-early-exit.cfa $(CFACCBIN)
	$(CFACOMPILE_SYNTAX) -DERR2
	-cp $(test) $(abspath ${@})

# Exception Tests
# Test with libcfathread; it changes how storage works.

exceptions/%-threads : exceptions/%.cfa $(CFACCBIN)
	$(CFACOMPILETEST) -include exceptions/with-threads.hfa -c -o $(abspath ${@}).o
	$(CFACCLOCAL) $($(shell echo "${@}_FLAGSLD" | sed 's/-\|\//_/g')) $(abspath ${@}).o -o $(abspath ${@})

# Linking tests
# Meta tests to make sure we see linking errors (can't compile with -O2 since it may multiply number of calls)
linking/linkerror : linking/linkerror.cfa $(CFACCBIN)
	$(CFACOMPILETEST) -O0 -c -o $(abspath ${@}).o
	$(CFACCLINK)  -O0 ${@}.o -o $(abspath ${@})
	rm $(abspath ${@}).o

#------------------------------------------------------------------------------
# Other targets
