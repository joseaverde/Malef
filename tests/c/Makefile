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
OBJ=obj
BIN=../bin
FLAGS= $(INCLUDE)

ifeq ($(OS),Windows_NT)
	LIBRARY:=$(LIBRARY)windows
	OBJ:=$(OBJ)-windows
	BIN:=$(BIN)-windows
else
	UNAME:=$(shell uname -s)
	ifeq ($(UNAME),Linux)
		LIBRARY:=$(LIBRARY)linux
		OBJ:=$(OBJ)-linux
		BIN:=$(BIN)-linux
	else ifeq ($(UNAME),Darwin)
	# TODO: Add MacOS support
		LIBRARY:=$(LIBRARY)unix
		OBJ:=$(OBJ)-unix
		BIN:=$(BIN)-unix
	else
		LIBRARY:=$(LIBRARY)unix
		OBJ:=$(OBJ)-unix
		BIN:=$(BIN)-unix
	endif
endif


all: $(OBJ) $(BIN)/tests-c


$(OBJ):
	mkdir $(OBJ)


$(BIN)/tests-c: $(OBJ)/main.o $(OBJ)/tests.o $(OBJ)/test_malef.o
	gcc $(OBJ)/*.o -o $(BIN)/tests-c -L$(LIBRARY) -Wl,-rpath=$(LIBRARY) -lCMalef

$(OBJ)/main.o: $(OBJ) src/main.c
	gcc -c src/main.c -o $(OBJ)/main.o $(FLAGS)

$(OBJ)/tests.o: $(OBJ) src/tests.c
	gcc -c src/tests.c -o $(OBJ)/tests.o $(FLAGS)

$(OBJ)/test_malef.o: $(OBJ) src/test_malef.c
	gcc -c src/test_malef.c -o $(OBJ)/test_malef.o $(FLAGS)



###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
