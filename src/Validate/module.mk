######################### -*- Mode: Makefile-Gmake -*- ########################
##
## Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## module.mk --
##
## Author           : Rob Schluntz
## Created On       : Fri Jul 27 10:10:10 2018
## Last Modified By : Rob Schluntz
## Last Modified On : Fri Jul 27 10:10:26 2018
## Update Count     : 2
###############################################################################

SRC_VALIDATE = \
	Validate/Autogen.cpp \
	Validate/Autogen.hpp \
	Validate/CompoundLiteral.cpp \
	Validate/CompoundLiteral.hpp \
	Validate/ForallPointerDecay.cpp \
	Validate/ForallPointerDecay.hpp \
	Validate/GenericParameter.cpp \
	Validate/GenericParameter.hpp \
	Validate/HandleAttributes.cc \
	Validate/HandleAttributes.h \
	Validate/InitializerLength.cpp \
	Validate/InitializerLength.hpp \
	Validate/LabelAddressFixer.cpp \
	Validate/LabelAddressFixer.hpp \
	Validate/ReturnCheck.cpp \
	Validate/ReturnCheck.hpp \
	Validate/FindSpecialDeclsNew.cpp \
	Validate/FindSpecialDecls.cc \
	Validate/FindSpecialDecls.h

SRC += $(SRC_VALIDATE)
SRCDEMANGLE += $(SRC_VALIDATE)
