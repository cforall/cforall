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
## Last Modified On : Wed Nov  1 21:09:25 2023
## Update Count     : 221
###############################################################################

# create object files in directory with source files
AUTOMAKE_OPTIONS = foreign subdir-objects
ACLOCAL_AMFLAGS  = -I automake

# put into lib for now
cfalibdir = ${CFA_LIBDIR}
cfalib_DATA = gcc-builtins.cfa builtins.cfa extras.cfa prelude.cfa bootloader.c defines.hfa

EXTRA_DIST = bootloader.cfa builtins.c builtins.def extras.c extras.regx extras.regx2 prelude-gen.cc prototypes.awk prototypes.c prototypes.sed sync-builtins.cfa

CC = @LOCAL_CFACC@
AM_CFLAGS = -g -Wall -Werror=return-type -Wno-unused-function -fPIC @ARCH_FLAGS@ @CONFIG_CFLAGS@
AM_CFAFLAGS = @CONFIG_CFAFLAGS@

$(DEPDIR) :
	mkdir $(DEPDIR)

$(DEPDIR)/builtins.Po : $(DEPDIR)
	touch ${@}

# create extra forward types/declarations to reduce inclusion of library files
extras.cfa : ${srcdir}/extras.regx ${srcdir}/extras.c
	@echo '# 2 "${@}"  // needed for error messages from this file' > ${@}
	${AM_V_GEN}gcc ${AM_CFLAGS} -E ${srcdir}/extras.c | grep -f ${srcdir}/extras.regx >> ${@}
	${AM_V_GEN}gcc ${AM_CFLAGS} -E ${srcdir}/extras.c | grep -zo -f ${srcdir}/extras.regx2 | tr '\0' '\n' >> ${@}

# create forward declarations for gcc builtins
gcc-builtins.cfa : gcc-builtins.c ${srcdir}/prototypes.sed
	@echo '# 2 "${@}"  // needed for error messages from this file' > ${@}
	${AM_V_GEN}gcc -I${srcdir} -E -P $< | sed -r -f ${srcdir}/prototypes.sed >> ${@}

gcc-builtins.c : ${srcdir}/builtins.def ${srcdir}/prototypes.awk ${srcdir}/sync-builtins.cfa ${srcdir}/prototypes.c
	${AM_V_GEN}gcc -I${srcdir} -E ${srcdir}/prototypes.c | awk -f ${srcdir}/prototypes.awk > ${@}

prelude.cfa : prelude-gen.cc
	${AM_V_GEN}${CXX} ${AM_CXXFLAGS} ${CXXFLAGS} ${AM_CFLAGS} ${<} -o prelude-gen -Wall -Wextra -O2 -g -std=c++17
	@./prelude-gen > ${@}
	@rm ./prelude-gen

builtins.def :

prototypes.awk :

# create forward declarations for cfa builtins
builtins.cfa : builtins.c @LOCAL_CFACC@
	${AM_V_GEN}gcc ${AM_CFLAGS} -E ${<} -o ${@} -MD -MP -MF $(DEPDIR)/builtins.Po -D__cforall
	${AM_V_at}sed -i 's/builtins.o/builtins.cfa/g' $(DEPDIR)/builtins.Po

include $(DEPDIR)/builtins.Po

bootloader.c : ${srcdir}/bootloader.cfa prelude.cfa extras.cfa gcc-builtins.cfa builtins.cfa @CFACPP@
	${AM_V_GEN}@CFACPP@ --prelude-dir=${builddir} -tpm ${srcdir}/bootloader.cfa ${@}  # use src/cfa-cpp as not in lib until after install

maintainer-clean-local :
	rm -rf $(DEPDIR)

MOSTLYCLEANFILES = bootloader.c builtins.cfa extras.cfa gcc-builtins.c gcc-builtins.cfa prelude.cfa
DISTCLEANFILES = $(DEPDIR)/builtins.Po
MAINTAINERCLEANFILES = ${addprefix ${libdir}/,${cfalib_DATA}} ${addprefix ${libdir}/,${lib_LIBRARIES}}

if ENABLE_DISTCC
distribution: @LOCAL_CFACC@ @LOCAL_CC1@ @CFACPP@ defines.hfa gcc-builtins.cfa builtins.cfa extras.cfa prelude.cfa bootloader.c $(srcdir)/../../tools/build/push2dist.sh
	${AM_V_GEN}$(srcdir)/../../tools/build/push2dist.sh @CFADIR_HASH@ @DIST_BWLIMIT@
	@echo "Dummy file to track distribution to remote hosts" > ${@}

all: all-am distribution
endif ENABLE_DISTCC
