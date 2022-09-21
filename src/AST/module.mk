######################### -*- Mode: Makefile-Gmake -*- ########################
##
## Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## module.mk --
##
## Author           : Thierry Delisle
## Created On       : Thu May 09 16:05:36 2019
## Last Modified By : Peter A. Buhr
## Last Modified On : Sat Dec 14 07:29:10 2019
## Update Count     : 3
###############################################################################

SRC_AST = \
	AST/Attribute.cpp \
	AST/Attribute.hpp \
	AST/Bitfield.hpp \
	AST/Chain.hpp \
	AST/Convert.cpp \
	AST/Convert.hpp \
	AST/Copy.cpp \
	AST/Copy.hpp \
	AST/Create.cpp \
	AST/Create.hpp \
	AST/CVQualifiers.hpp \
	AST/Decl.cpp \
	AST/Decl.hpp \
	AST/DeclReplacer.cpp \
	AST/DeclReplacer.hpp \
	AST/Expr.cpp \
	AST/Expr.hpp \
	AST/FunctionSpec.hpp \
	AST/Fwd.hpp \
	AST/GenericSubstitution.cpp \
	AST/GenericSubstitution.hpp \
	AST/Init.cpp \
	AST/Init.hpp \
	AST/Inspect.cpp \
	AST/Inspect.hpp \
	AST/Label.hpp \
	AST/LinkageSpec.cpp \
	AST/LinkageSpec.hpp \
	AST/Node.cpp \
	AST/Node.hpp \
	AST/ParseNode.hpp \
	AST/Pass.cpp \
	AST/Pass.hpp \
	AST/Pass.impl.hpp \
	AST/Pass.proto.hpp \
	AST/Print.cpp \
	AST/Print.hpp \
	AST/Stmt.cpp \
	AST/Stmt.hpp \
	AST/StorageClasses.hpp \
	AST/SymbolTable.cpp \
	AST/SymbolTable.hpp \
	AST/TranslationUnit.hpp \
	AST/Type.cpp \
	AST/Type.hpp \
	AST/TypeEnvironment.cpp \
	AST/TypeEnvironment.hpp \
	AST/TypeSubstitution.cpp \
	AST/TypeSubstitution.hpp \
	AST/Util.cpp \
	AST/Util.hpp \
	AST/Visitor.hpp

SRC += $(SRC_AST)
SRCDEMANGLE += $(SRC_AST)
