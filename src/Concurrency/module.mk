######################### -*- Mode: Makefile-Gmake -*- ########################
##
## Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
##
## The contents of this file are covered under the licence agreement in the
## file "LICENCE" distributed with Cforall.
##
## module.mk --
##
## Author           : Thierry Delisle
## Created On       : Mon Mar 13 12:48:40 2017
## Last Modified By : Andrew Beach
## Last Modified On : Tue May 17 13:28:00 2022
## Update Count     : 1
###############################################################################

SRC += \
	Concurrency/Actors.cpp \
	Concurrency/Actors.hpp \
	Concurrency/Corun.cpp \
	Concurrency/Corun.hpp \
	Concurrency/KeywordsNew.cpp \
	Concurrency/Keywords.cc \
	Concurrency/Keywords.h \
	Concurrency/WaitforNew.cpp \
	Concurrency/Waitfor.cc \
	Concurrency/Waitfor.h \
	Concurrency/Waituntil.cpp \
	Concurrency/Waituntil.hpp
