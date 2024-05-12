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
## Last Modified On : Tue May 17 14:27:00 2022
## Update Count     : 5
###############################################################################

SRC_COMMON = \
	Common/Assert.cpp \
	Common/CodeLocation.hpp \
	Common/CodeLocationTools.hpp \
	Common/CodeLocationTools.cpp \
	Common/DeclStats.hpp \
	Common/DeclStats.cpp \
	Common/ErrorObjects.hpp \
	Common/Eval.cpp \
	Common/Eval.hpp \
	Common/Examine.cpp \
	Common/Examine.hpp \
	Common/FilterCombos.hpp \
	Common/Indenter.hpp \
	Common/Indenter.cpp \
	Common/Iterate.hpp \
	Common/PersistentMap.hpp \
	Common/ResolvProtoDump.hpp \
	Common/ResolvProtoDump.cpp \
	Common/ScopedMap.hpp \
	Common/SemanticError.cpp \
	Common/SemanticError.hpp \
	Common/Stats.hpp \
	Common/Stats/Base.hpp \
	Common/Stats/Counter.cpp \
	Common/Stats/Counter.hpp \
	Common/Stats/Heap.cpp \
	Common/Stats/Heap.hpp \
	Common/Stats/ResolveTime.cpp \
	Common/Stats/ResolveTime.hpp \
	Common/Stats/Stats.cpp \
	Common/Stats/Time.cpp \
	Common/Stats/Time.hpp \
	Common/ToString.hpp \
	Common/UniqueName.cpp \
	Common/UniqueName.hpp \
	Common/Utility.hpp \
	Common/VectorMap.hpp

SRC += $(SRC_COMMON) \
	Common/DebugMalloc.cpp

SRCDEMANGLE += $(SRC_COMMON)
