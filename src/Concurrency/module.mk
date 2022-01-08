######################### -*- Mode: Makefile-Gmake -*- ########################
##
## Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## module.mk --
##
## Author           : Thierry Delisle
## Created On       : Mon Mar 13 12:48:40 2017
## Last Modified By :
## Last Modified On :
## Update Count     : 0
###############################################################################

SRC_CONCURRENCY = \
	Concurrency/KeywordsNew.cpp \
	Concurrency/Keywords.cc

SRC += $(SRC_CONCURRENCY) \
	Concurrency/Keywords.h \
	Concurrency/Waitfor.cc \
	Concurrency/Waitfor.h

SRCDEMANGLE += $(SRC_CONCURRENCY)

