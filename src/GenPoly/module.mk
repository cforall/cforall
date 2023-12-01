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
## Last Modified On : Tue May 17 14:31:00 2022
## Update Count     : 2
###############################################################################

SRC_GENPOLY = \
	GenPoly/GenPoly.cc \
	GenPoly/GenPoly.h \
	GenPoly/Lvalue2.cc \
	GenPoly/Lvalue.h

SRC += $(SRC_GENPOLY) \
	GenPoly/Box.cpp \
	GenPoly/Box.h \
	GenPoly/ErasableScopedMap.h \
	GenPoly/FindFunction.cc \
	GenPoly/FindFunction.h \
	GenPoly/InstantiateGeneric.cpp \
	GenPoly/InstantiateGeneric.h \
	GenPoly/Lvalue.cpp \
	GenPoly/ScopedSet.h \
	GenPoly/ScrubTypeVars.cpp \
	GenPoly/ScrubTypeVars.hpp \
	GenPoly/Specialize.cpp \
	GenPoly/Specialize.h

SRCDEMANGLE += $(SRC_GENPOLY)
