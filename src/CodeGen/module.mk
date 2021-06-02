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
## Last Modified By : Peter A. Buhr
## Last Modified On : Sat Dec 14 07:29:42 2019
## Update Count     : 4
###############################################################################

#SRC +=  ArgTweak/Rewriter.cc \
#	ArgTweak/Mutate.cc

SRC_CODEGEN = \
	CodeGen/CodeGenerator.cc \
	CodeGen/CodeGenerator.h \
	CodeGen/FixMain.cc \
	CodeGen/FixMain.h \
	CodeGen/GenType.cc \
	CodeGen/GenType.h \
	CodeGen/LinkOnce.cc \
	CodeGen/LinkOnce.h \
	CodeGen/OperatorTable.cc \
	CodeGen/OperatorTable.h \
	CodeGen/Options.h

SRC += $(SRC_CODEGEN) CodeGen/Generate.cc CodeGen/Generate.h CodeGen/FixNames.cc CodeGen/FixNames.h
SRCDEMANGLE += $(SRC_CODEGEN)
