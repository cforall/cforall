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
## Last Modified On : Sat Dec 14 07:26:43 2019
## Update Count     : 2
###############################################################################

SRC_SYNTREE = \
      SynTree/AddressExpr.cc \
      SynTree/AggregateDecl.cc \
      SynTree/ApplicationExpr.cc \
      SynTree/ArrayType.cc \
      SynTree/Attribute.cc \
      SynTree/Attribute.h \
      SynTree/AttrType.cc \
      SynTree/BaseSyntaxNode.h \
      SynTree/BasicType.cc \
      SynTree/CommaExpr.cc \
      SynTree/CompoundStmt.cc \
      SynTree/Constant.cc \
      SynTree/Constant.h \
      SynTree/Declaration.cc \
      SynTree/Declaration.h \
      SynTree/DeclarationWithType.cc \
      SynTree/DeclReplacer.cc \
      SynTree/DeclReplacer.h \
      SynTree/DeclStmt.cc \
      SynTree/Expression.cc \
      SynTree/Expression.h \
      SynTree/FunctionDecl.cc \
      SynTree/FunctionType.cc \
      SynTree/Initializer.cc \
      SynTree/Initializer.h \
      SynTree/Label.h \
      SynTree/LinkageSpec.cc \
      SynTree/LinkageSpec.h \
      SynTree/Mutator.h \
      SynTree/NamedTypeDecl.cc \
      SynTree/ObjectDecl.cc \
      SynTree/PointerType.cc \
      SynTree/ReferenceToType.cc \
      SynTree/ReferenceType.cc \
      SynTree/Statement.cc \
      SynTree/Statement.h \
      SynTree/SynTree.h \
      SynTree/TupleExpr.cc \
      SynTree/TupleType.cc \
      SynTree/Type.cc \
      SynTree/TypeDecl.cc \
      SynTree/TypeExpr.cc \
      SynTree/Type.h \
      SynTree/TypeofType.cc \
      SynTree/TypeSubstitution.cc \
      SynTree/TypeSubstitution.h \
      SynTree/VarArgsType.cc \
      SynTree/Visitor.h \
      SynTree/VoidType.cc \
      SynTree/ZeroOneType.cc

SRC += $(SRC_SYNTREE)
SRCDEMANGLE += $(SRC_SYNTREE)
