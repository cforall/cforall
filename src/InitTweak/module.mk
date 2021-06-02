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
## Last Modified By : Rob Schluntz
## Last Modified On : Fri May 13 11:36:24 2016
## Update Count     : 3
###############################################################################

SRC += \
	InitTweak/FixGlobalInit.cc \
	InitTweak/FixGlobalInit.h \
	InitTweak/FixInit.cc \
	InitTweak/FixInit.h \
	InitTweak/GenInit.cc \
	InitTweak/GenInit.h \
	InitTweak/InitTweak.cc \
	InitTweak/InitTweak.h \
	InitTweak/FixInitNew.cpp

SRCDEMANGLE += \
	InitTweak/GenInit.cc \
	InitTweak/GenInit.h \
	InitTweak/InitTweak.cc \
	InitTweak/InitTweak.h

