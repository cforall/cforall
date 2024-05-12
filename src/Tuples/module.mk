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
## Last Modified On : Mon May 17 15:00:00 2022
## Update Count     : 3
###############################################################################

SRC_TUPLES = \
	Tuples/Explode.cpp \
	Tuples/Explode.hpp \
	Tuples/TupleAssignment.cpp \
	Tuples/TupleExpansion.cpp \
	Tuples/Tuples.cpp \
	Tuples/Tuples.hpp

SRC += $(SRC_TUPLES)

SRCDEMANGLE += $(SRC_TUPLES)
