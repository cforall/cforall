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
## Last Modified On : Tue Sep 27 11:06:38 2016
## Update Count     : 4
###############################################################################

SRC_COMMON = \
      Common/Assert.cc \
      Common/CodeLocation.h \
      Common/CodeLocationTools.hpp \
      Common/CodeLocationTools.cpp \
      Common/CompilerError.h \
      Common/Debug.h \
      Common/DeclStats.hpp \
      Common/DeclStats.cpp \
      Common/ErrorObjects.h \
      Common/Eval.cc \
      Common/Examine.cc \
      Common/Examine.h \
      Common/FilterCombos.h \
      Common/Indenter.h \
      Common/PassVisitor.cc \
      Common/PassVisitor.h \
      Common/PassVisitor.impl.h \
      Common/PassVisitor.proto.h \
      Common/PersistentMap.h \
      Common/ScopedMap.h \
      Common/SemanticError.cc \
      Common/SemanticError.h \
      Common/Stats.h \
      Common/Stats/Base.h \
      Common/Stats/Counter.cc \
      Common/Stats/Counter.h \
      Common/Stats/Heap.cc \
      Common/Stats/Heap.h \
      Common/Stats/ResolveTime.cc \
      Common/Stats/ResolveTime.h \
      Common/Stats/Stats.cc \
      Common/Stats/Time.cc \
      Common/Stats/Time.h \
      Common/UnimplementedError.h \
      Common/UniqueName.cc \
      Common/UniqueName.h \
      Common/utility.h \
      Common/VectorMap.h

SRC += $(SRC_COMMON) Common/DebugMalloc.cc
SRCDEMANGLE += $(SRC_COMMON)
