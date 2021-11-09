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
## Last Modified By : Henry Xue
## Last Modified On : Tue Jul 20 04:10:50 2021
## Update Count     : 5
###############################################################################

SRC_CONTROLSTRUCT = \
	ControlStruct/ExceptDecl.cc \
	ControlStruct/ExceptDecl.h \
	ControlStruct/FixLabels.cpp \
	ControlStruct/FixLabels.hpp \
	ControlStruct/ForExprMutator.cc \
	ControlStruct/ForExprMutator.h \
	ControlStruct/LabelFixer.cc \
	ControlStruct/LabelFixer.h \
	ControlStruct/LabelGenerator.cc \
	ControlStruct/LabelGenerator.h \
	ControlStruct/MLEMutator.cc \
	ControlStruct/MLEMutator.h \
	ControlStruct/MultiLevelExit.cpp \
	ControlStruct/MultiLevelExit.hpp \
	ControlStruct/Mutate.cc \
	ControlStruct/Mutate.h

SRC += $(SRC_CONTROLSTRUCT) \
	ControlStruct/ExceptTranslateNew.cpp \
	ControlStruct/ExceptTranslate.cc \
	ControlStruct/ExceptTranslate.h

SRCDEMANGLE += $(SRC_CONTROLSTRUCT)

