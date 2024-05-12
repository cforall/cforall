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
## Update Count     : 4
###############################################################################

SRC_INITTWEAK = \
	InitTweak/GenInit.cpp \
	InitTweak/GenInit.hpp \
	InitTweak/InitTweak.cpp \
	InitTweak/InitTweak.hpp

SRC += $(SRC_INITTWEAK) \
	InitTweak/FixGlobalInit.cpp \
	InitTweak/FixGlobalInit.hpp \
	InitTweak/FixInit.cpp \
	InitTweak/FixInit.hpp

SRCDEMANGLE += $(SRC_INITTWEAK)
