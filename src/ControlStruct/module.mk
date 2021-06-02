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
## Last Modified On : Wed Jun 28 16:15:00 2017
## Update Count     : 4
###############################################################################

SRC_CONTROLSTRUCT = \
	ControlStruct/ForExprMutator.cc \
	ControlStruct/ForExprMutator.h \
	ControlStruct/LabelFixer.cc \
	ControlStruct/LabelFixer.h \
	ControlStruct/LabelGenerator.cc \
	ControlStruct/LabelGenerator.h \
	ControlStruct/MLEMutator.cc \
	ControlStruct/MLEMutator.h \
	ControlStruct/Mutate.cc \
	ControlStruct/Mutate.h

SRC += $(SRC_CONTROLSTRUCT) ControlStruct/ExceptTranslate.cc ControlStruct/ExceptTranslate.h
SRCDEMANGLE += $(SRC_CONTROLSTRUCT)

