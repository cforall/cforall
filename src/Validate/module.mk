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
	Validate/FindSpecialDecls.h

SRC += $(SRC_VALIDATE) \
	Validate/Autogen.cpp \
	Validate/Autogen.hpp \
	Validate/CompoundLiteral.cpp \
	Validate/CompoundLiteral.hpp \
	Validate/EliminateTypedef.cpp \
	Validate/EliminateTypedef.hpp \
	Validate/EnumAndPointerDecay.cpp \
	Validate/EnumAndPointerDecay.hpp \
	Validate/FindSpecialDecls.cpp \
	Validate/FixQualifiedTypes.cpp \
	Validate/FixQualifiedTypes.hpp \
	Validate/FixReturnTypes.cpp \
	Validate/FixReturnTypes.hpp \
	Validate/ForallPointerDecay.cpp \
	Validate/ForallPointerDecay.hpp \
	Validate/GenericParameter.cpp \
	Validate/GenericParameter.hpp \
	Validate/HoistStruct.cpp \
	Validate/HoistStruct.hpp \
	Validate/HoistTypeDecls.cpp \
	Validate/HoistTypeDecls.hpp \
	Validate/InitializerLength.cpp \
	Validate/InitializerLength.hpp \
	Validate/LabelAddressFixer.cpp \
	Validate/LabelAddressFixer.hpp \
	Validate/LinkInstanceTypes.cpp \
	Validate/LinkInstanceTypes.hpp \
	Validate/NoIdSymbolTable.hpp \
	Validate/ReplaceTypedef.cpp \
	Validate/ReplaceTypedef.hpp \
	Validate/ReturnCheck.cpp \
	Validate/ReturnCheck.hpp \
	Validate/VerifyCtorDtorAssign.cpp \
	Validate/VerifyCtorDtorAssign.hpp \
	Validate/ImplementEnumFunc.cpp \
	Validate/ImplementEnumFunc.hpp

SRCDEMANGLE += $(SRC_VALIDATE)
