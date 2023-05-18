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
## Created On       : Sun May 31 22:14:18 2015
## Last Modified By : Peter A. Buhr
## Last Modified On : Wed May 17 11:02:34 2023
## Update Count     : 56
###############################################################################

AUTOMAKE_OPTIONS = foreign    # do not require all the GNU file names
ACLOCAL_AMFLAGS  = -I automake

MAINTAINERCLEANFILES = lib/* bin/* tests/.deps/* tests/.out/* # order important
DISTCLEANFILES = version

SUBDIRS = driver src . @LIBCFA_TARGET_DIRS@
DIST_SUBDIRS = driver src . libcfa tests

@LIBCFA_TARGET_MAKEFILES@ : Makefile ${srcdir}/libcfa/configure
	@${eval config_file = ${dir ${@}}config.data}
	@ls ${config_file} || (echo "Missing config.data, re-run configure script again" && false)
	@${eval config_data = ${shell cat ${config_file}}}
	@echo "Configuring libcfa (${abs_top_srcdir}/libcfa/configure) with '${config_data}' from ${shell pwd} / ${dir ${@}}"
	@cd ${dir ${@}} && ${abs_top_srcdir}/libcfa/configure ${config_data}

noinst_DATA = @LIBCFA_TARGET_MAKEFILES@

man1_MANS = doc/man/cfa.1

EXTRA_DIST = LICENSE doc/man/cfa.1 libcfa/configure libcfa/Makefile.dist.am libcfa/Makefile.dist.in tools/build/distcc_hash tools/build/push2dist.sh

debug ?= yes
installed ?= no
ARCH = ${if ${arch},"arch=${arch}"}
arch_support = "x86/x64/arm"

# target "all" created by automake

check :
	${MAKE} -C tests tests installed=${installed} debug=${debug} ${ARCH}

tests : check # synonym

installcheck :
	${MAKE} -C tests tests installed=yes debug=${debug} ${ARCH}

installtest : installcheck # synonym

status : @LIBCFA_TARGET_MAKEFILES@
	@echo -ne "translator\n\t"
	@./config.status --config | sed "s/ /\n\t/g; s/\t'/\t/g; s/'\n/\n/g; s/^'//g; s/'$$//g"
	@find libcfa -name config.status -printf "\n%h\n\t" -exec {} --config \; | sed "s/ /\n\t/g; s/\t'/\t/g; s/'\n/\n/g; s/^'//g; s/'$$//g"

help :
	@echo "user targets:"
	@echo "    Compile compiler/runtime."
	@echo "    $$ make (null) / all"
	@echo ""
	@echo "    Compile compiler/runtime and run test suite."
	@echo "    $$ make check / tests [debug=yes/no] [installed=yes/no] [arch=${arch_support}]"
	@echo ""
	@echo "    Compile compiler/runtime, install, and run test suite on installed system."
	@echo "    $$ make installcheck / installtests [debug=yes/no] installed=yes [arch=${arch_support}]"
	@echo ""
	@echo "    Print configuration parameters and system build information."
	@echo "    $$ make status"

configure-libcfa : @LIBCFA_TARGET_MAKEFILES@
	@true

@LIBCFA_TARGET_DIRS@ ::
	${MAKE} -C ${@} ${MAKECMDGOALS}

mostlyclean clean distclean maintainer-clean : @LIBCFA_TARGET_DIRS@
