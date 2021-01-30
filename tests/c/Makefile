#!/usr/bin/make -f
#=============================================================================#
 #                                                                           # 
 #                              M A K E F I L E                              # 
 #                                                                           # 
 #                                 M A L E F                                 # 
 #                                                                           # 
 #                              M A K E F I L E                              # 
 #                                                                           # 
 #---------------------------------------------------------------------------# 
 #     Copyright (c) 2021 José Antonio Verde Jiménez All Rights Reserved     # 
 #---------------------------------------------------------------------------# 
 # This file is part of Malef.                                               # 
 #                                                                           # 
 # This program is free software:  you  can redistribute it and/or modify it # 
 # under  the terms  of the  GNU  General License  as published by the  Free # 
 # Software  Foundation,  either  version 3  of  the  License,  or  (at your # 
 # opinion) any later version.                                               # 
 #                                                                           # 
 # This  program  is distributed  in the  hope that  it will be  useful, but # 
 # WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of # 
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General # 
 # Public License for more details.                                          # 
 #                                                                           # 
 # You should have received  a copy of the  GNU General Public License along # 
 # with this program. If not, see <https://www.gnu.org/licenses/>.           # 
 #                                                                           # 
#=============================================================================#

INCLUDE=-I$(shell readlink -f ../../c-malef/include)
LIBRARY=$(shell readlink -f ../../alire/build/lib-)
FLAGS=-fprofile-arcs -ftest-coverage $(INCLUDE)

ifeq ($(OS),Windows_NT)
	LIBRARY:=$(LIBRARY)windows.cmd
else
	UNAME:=$(shell uname -s)
	ifeq ($(UNAME),Linux)
		LIBRARY:=$(LIBRARY)linux.ansi
	else ifeq ($(UNAME),Darwin)
	# TODO: Add MacOS support
		LIBRARY:=$(LIBRARY)unix.ansi
	else
		LIBRARY:=$(LIBRARY)unix.ansi
	endif
endif


all: obj ../bin/tests-c


obj:
	mkdir obj


../bin/tests-c: obj/main.o obj/tests.o obj/test_malef.o
	gcc obj/*.o -o ../bin/tests-c -L$(LIBRARY) -Wl,-rpath=$(LIBRARY) -lMalef -lgcov --coverage

obj/main.o: obj src/main.c
	gcc -c src/main.c -o obj/main.o $(FLAGS)

obj/tests.o: obj src/tests.c
	gcc -c src/tests.c -o obj/tests.o $(FLAGS)

obj/test_malef.o: obj src/test_malef.c
	gcc -c src/test_malef.c -o obj/test_malef.o $(FLAGS)



###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
