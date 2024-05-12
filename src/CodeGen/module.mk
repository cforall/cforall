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
	CodeGen/CodeGenerator.cpp \
	CodeGen/CodeGenerator.hpp \
	CodeGen/GenType.cpp \
	CodeGen/GenType.hpp \
	CodeGen/OperatorTable.cpp \
	CodeGen/OperatorTable.hpp

SRC += $(SRC_CODEGEN) \
	CodeGen/FixMain.cpp \
	CodeGen/FixMain.hpp \
	CodeGen/FixNames.cpp \
	CodeGen/FixNames.hpp \
	CodeGen/Generate.cpp \
	CodeGen/Generate.hpp \
	CodeGen/LinkOnce.cpp \
	CodeGen/LinkOnce.hpp \
	CodeGen/Options.hpp

SRCDEMANGLE += $(SRC_CODEGEN)
