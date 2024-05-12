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
      ResolvExpr/AdjustExprType.cpp \
      ResolvExpr/AdjustExprType.hpp \
      ResolvExpr/Candidate.cpp \
      ResolvExpr/CandidateFinder.cpp \
      ResolvExpr/CandidateFinder.hpp \
      ResolvExpr/Candidate.hpp \
      ResolvExpr/CastCost.cpp \
      ResolvExpr/CastCost.hpp \
      ResolvExpr/CommonType.cpp \
      ResolvExpr/CommonType.hpp \
      ResolvExpr/ConversionCost.cpp \
      ResolvExpr/ConversionCost.hpp \
      ResolvExpr/Cost.hpp \
      ResolvExpr/CurrentObject.cpp \
      ResolvExpr/CurrentObject.hpp \
      ResolvExpr/ExplodedArg.cpp \
      ResolvExpr/ExplodedArg.hpp \
      ResolvExpr/FindOpenVars.cpp \
      ResolvExpr/FindOpenVars.hpp \
      ResolvExpr/PolyCost.cpp \
      ResolvExpr/PolyCost.hpp \
      ResolvExpr/PtrsAssignable.cpp \
      ResolvExpr/PtrsAssignable.hpp \
      ResolvExpr/PtrsCastable.cpp \
      ResolvExpr/PtrsCastable.hpp \
      ResolvExpr/RenameVars.cpp \
      ResolvExpr/RenameVars.hpp \
      ResolvExpr/Resolver.cpp \
      ResolvExpr/Resolver.hpp \
      ResolvExpr/ResolveTypeof.cpp \
      ResolvExpr/ResolveTypeof.hpp \
      ResolvExpr/ResolveMode.hpp \
      ResolvExpr/SatisfyAssertions.cpp \
      ResolvExpr/SatisfyAssertions.hpp \
      ResolvExpr/SpecCost.cpp \
      ResolvExpr/SpecCost.hpp \
      ResolvExpr/Typeops.hpp \
      ResolvExpr/Unify.cpp \
      ResolvExpr/Unify.hpp \
      ResolvExpr/WidenMode.hpp

SRC += $(SRC_RESOLVEXPR) \
	ResolvExpr/CandidatePrinter.cpp \
	ResolvExpr/CandidatePrinter.hpp \
	ResolvExpr/EraseWith.cpp \
	ResolvExpr/EraseWith.hpp

SRCDEMANGLE += $(SRC_RESOLVEXPR)
