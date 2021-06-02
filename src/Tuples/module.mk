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
## Last Modified On : Mon Jun  1 17:54:33 2015
## Update Count     : 1
###############################################################################

SRC_TUPLES = \
	Tuples/Explode.cc \
	Tuples/Explode.h \
	Tuples/TupleAssignment.cc \
	Tuples/TupleExpansion.cc \
	Tuples/Tuples.cc \
	Tuples/Tuples.h


SRC += $(SRC_TUPLES)
SRCDEMANGLE += $(SRC_TUPLES)
