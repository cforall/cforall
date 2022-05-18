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
## Last Modified By : Andrew Beach
## Last Modified On : Tue May 17 14:59:00 2022
## Update Count     : 3
###############################################################################

SRC_VALIDATE = \
	Validate/FindSpecialDecls.cc \
	Validate/FindSpecialDecls.h

SRC += $(SRC_VALIDATE) \
	Validate/Autogen.cpp \
	Validate/Autogen.hpp \
	Validate/CompoundLiteral.cpp \
	Validate/CompoundLiteral.hpp \
	Validate/EliminateTypedef.cpp \
	Validate/EliminateTypedef.hpp \
	Validate/FindSpecialDeclsNew.cpp \
	Validate/FixQualifiedTypes.cpp \
	Validate/FixQualifiedTypes.hpp \
	Validate/ForallPointerDecay.cpp \
	Validate/ForallPointerDecay.hpp \
	Validate/GenericParameter.cpp \
	Validate/GenericParameter.hpp \
	Validate/HandleAttributes.cc \
	Validate/HandleAttributes.h \
	Validate/HoistStruct.cpp \
	Validate/HoistStruct.hpp \
	Validate/InitializerLength.cpp \
	Validate/InitializerLength.hpp \
	Validate/LabelAddressFixer.cpp \
	Validate/LabelAddressFixer.hpp \
	Validate/NoIdSymbolTable.hpp \
	Validate/ReturnCheck.cpp \
	Validate/ReturnCheck.hpp

SRCDEMANGLE += $(SRC_VALIDATE)
