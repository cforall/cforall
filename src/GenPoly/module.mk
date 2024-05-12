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
	GenPoly/GenPoly.cpp \
	GenPoly/GenPoly.hpp \
	GenPoly/Lvalue2.cpp \
	GenPoly/Lvalue.hpp

SRC += $(SRC_GENPOLY) \
	GenPoly/Box.cpp \
	GenPoly/Box.hpp \
	GenPoly/ErasableScopedMap.hpp \
	GenPoly/FindFunction.cpp \
	GenPoly/FindFunction.hpp \
	GenPoly/InstantiateGeneric.cpp \
	GenPoly/InstantiateGeneric.hpp \
	GenPoly/Lvalue.cpp \
	GenPoly/ScopedSet.hpp \
	GenPoly/ScrubTypeVars.cpp \
	GenPoly/ScrubTypeVars.hpp \
	GenPoly/Specialize.cpp \
	GenPoly/Specialize.hpp

SRCDEMANGLE += $(SRC_GENPOLY)
