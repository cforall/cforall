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
## Last Modified On : Mon Jun  1 17:52:30 2015
## Update Count     : 1
###############################################################################

SRC += GenPoly/Box.cc \
       GenPoly/Box.h \
       GenPoly/ErasableScopedMap.h \
       GenPoly/FindFunction.cc \
       GenPoly/FindFunction.h \
       GenPoly/GenPoly.cc \
       GenPoly/GenPoly.h \
       GenPoly/InstantiateGeneric.cc \
       GenPoly/InstantiateGeneric.h \
       GenPoly/Lvalue.cc \
       GenPoly/Lvalue.h \
       GenPoly/ScopedSet.h \
       GenPoly/ScrubTyVars.cc \
       GenPoly/ScrubTyVars.h \
       GenPoly/Specialize.cc \
       GenPoly/Specialize.h

SRCDEMANGLE += GenPoly/GenPoly.cc GenPoly/GenPoly.h GenPoly/Lvalue.cc GenPoly/Lvalue.h

