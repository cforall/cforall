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
## Last Modified On : Sat Feb  2 16:54:42 2019
## Update Count     : 21
###############################################################################

AUTOMAKE_OPTIONS = foreign    # do not require all the GNU file names
ACLOCAL_AMFLAGS  = -I automake

MAINTAINERCLEANFILES = lib/* bin/* tests/.deps/* tests/.out/* # order important
DISTCLEANFILES = version

SUBDIRS = driver src . @LIBCFA_TARGET_DIRS@
DIST_SUBDIRS = driver src . libcfa tests

@LIBCFA_TARGET_MAKEFILES@ : Makefile $(srcdir)/libcfa/configure
	@$(eval config_file = $(dir $@)config.data)
	@ls $(config_file) || (echo "Missing config.data, re-run configure script again" && false)
	@$(eval config_data = $(shell cat $(config_file)))
	@echo "Configuring libcfa ($(abs_top_srcdir)/libcfa/configure) with '$(config_data)' from $(shell pwd) / $(dir $@)"
	@cd $(dir $@) && $(abs_top_srcdir)/libcfa/configure $(config_data)

noinst_DATA = @LIBCFA_TARGET_MAKEFILES@

man1_MANS = doc/man/cfa.1

EXTRA_DIST = LICENSE doc/man/cfa.1 libcfa/configure libcfa/Makefile.dist.am libcfa/Makefile.dist.in tools/build/distcc_hash tools/build/push2dist.sh

debug=yes
check:
	$(MAKE) -C tests all-tests installed=no debug=${debug}

installcheck:
	$(MAKE) -C tests all-tests installed=yes debug=${debug}

configure-libcfa: @LIBCFA_TARGET_MAKEFILES@
	@true

status: @LIBCFA_TARGET_MAKEFILES@
	@echo -ne "translator\n\t"
	@./config.status --config | sed "s/ /\n\t/g; s/\t'/\t/g; s/'\n/\n/g; s/^'//g; s/'$$//g"
	@find libcfa -name config.status -printf "\n%h\n\t" -exec {} --config \; | sed "s/ /\n\t/g; s/\t'/\t/g; s/'\n/\n/g; s/^'//g; s/'$$//g"

mostlyclean-local: @LIBCFA_TARGET_MAKEFILES@
	for dir in @LIBCFA_TARGET_DIRS@; do \
		$(MAKE) -C $${dir} mostlyclean; \
	done

clean-local: @LIBCFA_TARGET_MAKEFILES@
	for dir in @LIBCFA_TARGET_DIRS@; do \
		$(MAKE) -C $${dir} clean; \
	done

distclean-local: @LIBCFA_TARGET_MAKEFILES@
	for dir in @LIBCFA_TARGET_DIRS@; do \
		$(MAKE) -C $${dir} distclean; \
		rm $${dir}/config.data; \
	done
