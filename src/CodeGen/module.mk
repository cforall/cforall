######################### -*- Mode: Makefile-Gmake -*- ########################
##
## Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## module.mk --
##
## Author           : Richard C. Bilson
## Created On       : Mon Jun  1 17:49:17 2015
## Last Modified By : Andrew Beach
## Last Modified On : Tue May 17 14:26:00 2022
## Update Count     : 5
###############################################################################

SRC_CODEGEN = \
	CodeGen/CodeGeneratorNew.cpp \
	CodeGen/CodeGeneratorNew.hpp \
	CodeGen/GenType.cc \
	CodeGen/GenType.h \
	CodeGen/OperatorTable.cc \
	CodeGen/OperatorTable.h

SRC += $(SRC_CODEGEN) \
	CodeGen/Generate.cc \
	CodeGen/Generate.h \
	CodeGen/FixMain.cc \
	CodeGen/FixMain.h \
	CodeGen/FixNames.cc \
	CodeGen/FixNames.h \
	CodeGen/LinkOnce.cc \
	CodeGen/LinkOnce.h \
	CodeGen/Options.h

SRCDEMANGLE += $(SRC_CODEGEN)
