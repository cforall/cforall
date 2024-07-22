######################## -*- Mode: Makefile-Automake -*- ######################
##
## Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## Makefile.am --
##
## Author           : Peter A. Buhr
## Created On       : Wed Jun 28 12:07:10 2017
## Last Modified By : Peter A. Buhr
## Last Modified On : Mon Jul 22 11:33:01 2024
## Update Count     : 25
###############################################################################

AUTOMAKE_OPTIONS = foreign    # do not require all the GNU file names
ACLOCAL_AMFLAGS  = -I automake

BUILT_SOURCES = parser.hh

AM_YFLAGS = -d -t -v -Wno-yacc

SRC = ParserTypes.h \
      filter.cc \
      filter.h \
      lex.ll \
      main.cc \
      parser.yy \
      token.cc \
      token.h

tools_prettyprinter_PROGRAMS = pretty
tools_prettyprinterdir = ../
pretty_SOURCES = ${SRC}
pretty_LDADD = ${LEXLIB} -ldl			# yywrap
pretty_CXXFLAGS = -Wno-deprecated -Wall -Wextra -Werror=return-type -DYY_NO_INPUT -O2 -g -std=c++14

MOSTLYCLEANFILES = parser.output
