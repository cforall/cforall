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
## Last Modified On : Tue May 17 14:46:00 2022
## Update Count     : 5
###############################################################################

SRC_SYMTAB = \
	SymTab/Autogen.cc \
	SymTab/Autogen.h \
	SymTab/FixFunction.cc \
	SymTab/FixFunction.h \
	SymTab/Indexer.cc \
	SymTab/Indexer.h \
	SymTab/Mangler.cc \
	SymTab/ManglerCommon.cc \
	SymTab/Mangler.h \
	SymTab/ValidateType.cc \
	SymTab/ValidateType.h

SRC += $(SRC_SYMTAB) \
	SymTab/Validate.cc \
	SymTab/Validate.h

SRCDEMANGLE += $(SRC_SYMTAB) \
	SymTab/Demangle.cc \
	SymTab/Demangle.h
