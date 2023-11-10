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
## Last Modified On : Mon Jun  1 17:53:28 2015
## Update Count     : 1
###############################################################################

SRC_RESOLVEXPR = \
      ResolvExpr/AdjustExprType.cc \
      ResolvExpr/AdjustExprType.hpp \
      ResolvExpr/Candidate.cpp \
      ResolvExpr/CandidateFinder.cpp \
      ResolvExpr/CandidateFinder.hpp \
      ResolvExpr/Candidate.hpp \
      ResolvExpr/CastCost.cc \
      ResolvExpr/CastCost.hpp \
      ResolvExpr/CommonType.cc \
      ResolvExpr/CommonType.hpp \
      ResolvExpr/ConversionCost.cc \
      ResolvExpr/ConversionCost.h \
      ResolvExpr/Cost.h \
      ResolvExpr/CurrentObject.cc \
      ResolvExpr/CurrentObject.h \
      ResolvExpr/ExplodedArg.cpp \
      ResolvExpr/ExplodedArg.hpp \
      ResolvExpr/FindOpenVars.cc \
      ResolvExpr/FindOpenVars.h \
      ResolvExpr/PolyCost.cc \
      ResolvExpr/PolyCost.hpp \
      ResolvExpr/PtrsAssignable.cc \
      ResolvExpr/PtrsAssignable.hpp \
      ResolvExpr/PtrsCastable.cc \
      ResolvExpr/PtrsCastable.hpp \
      ResolvExpr/RenameVars.cc \
      ResolvExpr/RenameVars.h \
      ResolvExpr/Resolver.cc \
      ResolvExpr/Resolver.h \
      ResolvExpr/ResolveTypeof.cc \
      ResolvExpr/ResolveTypeof.h \
      ResolvExpr/ResolvMode.h \
      ResolvExpr/SatisfyAssertions.cpp \
      ResolvExpr/SatisfyAssertions.hpp \
      ResolvExpr/SpecCost.cc \
      ResolvExpr/SpecCost.hpp \
      ResolvExpr/typeops.h \
      ResolvExpr/Unify.cc \
      ResolvExpr/Unify.h \
      ResolvExpr/WidenMode.h

SRC += $(SRC_RESOLVEXPR) \
	ResolvExpr/CandidatePrinter.cpp \
	ResolvExpr/CandidatePrinter.hpp \
	ResolvExpr/EraseWith.cpp \
	ResolvExpr/EraseWith.hpp

SRCDEMANGLE += $(SRC_RESOLVEXPR)
