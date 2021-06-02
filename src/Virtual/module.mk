######################### -*- Mode: Makefile-Gmake -*- ########################
##
## Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## module.mk --
##
## Author           : Andrew Beach
## Created On       : Tus Jul 25 10:18:00 2017
## Last Modified By : Andrew Beach
## Last Modified On : Tus Jul 25 10:18:00 2017
## Update Count     : 0
###############################################################################

SRC += Virtual/ExpandCasts.cc Virtual/ExpandCasts.h \
	Virtual/Tables.cc Virtual/Tables.h

SRCDEMANGLE += Virtual/Tables.cc
