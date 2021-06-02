######################### -*- Mode: Makefile-Gmake -*- ########################
##
## Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## module.mk --
##
## Author           : Peter A. Buhr
## Created On       : Sat May 16 15:29:09 2015
## Last Modified By : Peter A. Buhr
## Last Modified On : Sat Dec 14 07:34:47 2019
## Update Count     : 107
###############################################################################

BUILT_SOURCES = Parser/parser.hh

AM_YFLAGS = -d -t -v -Wno-yacc

SRC += \
       Parser/DeclarationNode.cc \
       Parser/ExpressionNode.cc \
       Parser/InitializerNode.cc \
       Parser/lex.ll \
       Parser/ParseNode.cc \
       Parser/ParseNode.h \
       Parser/parser.yy \
       Parser/ParserTypes.h \
       Parser/parserutility.cc \
       Parser/parserutility.h \
       Parser/StatementNode.cc \
       Parser/TypeData.cc \
       Parser/TypeData.h \
       Parser/TypedefTable.cc \
       Parser/TypedefTable.h

MOSTLYCLEANFILES += Parser/lex.cc Parser/parser.cc Parser/parser.hh Parser/parser.output
