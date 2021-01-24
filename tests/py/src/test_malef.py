#!/usr/bin/env  python3
# *-* encoding=utf8 *-*
#=============================================================================#
 #                                                                           # 
 #                         T E S T _ M A L E F . P Y                         # 
 #                                                                           # 
 #                                 M A L E F                                 # 
 #                                                                           # 
 #                         P Y T H O N   S C R I P T                         # 
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

import malef
import tests


def initialize () -> str:
    malef.initialize ()
    return "<>"


def finalize () -> str:
    malef.finalize ()
    return "<>"


def isInitialized () -> str:
    return str(malef.isInitialized ())


def getHeight () -> str:
    if malef.isInitialized ():
        tests.wait ("Set the terminal height to 20")
        if malef.updateTerminalSize ():
            return "$No automatic update"

    return "@%d" % malef.getHeight ()


def getWidth () -> str:
    if malef.isInitialized ():
        tests.wait ("Set the terminal width to 50")
        if malef.updateTerminalSize ():
            return "$No automatic update"

    return "@%d" % malef.getWidth ()


def newPage () -> str:
    malef.newPage ()
    return "<>"


def setTitle () -> str:
    malef.setTitle ("Testing Malef in Python3!")
    return "<>"



def main ():
    tests.test (initialize, "initialize", "<>")
    tests.test (initialize, "initialize", "InitializationError")
    tests.test (finalize,   "finalize",   "<>")
    tests.test (finalize,   "finalize",   "InitializationError")
    tests.test (getHeight,  "getHeight",  "InitializationError")
    tests.test (getWidth,   "getWidth",   "InitializationError")
    tests.test (newPage,    "newPage",    "InitializationError")
    tests.test (setTitle,   "setTitle",   "InitializationError")

    tests.safe_run (initialize)

    tests.test (getHeight, "getHeight", "@20", False)
    tests.test (getWidth,  "getWidth",  "@50", False)
    tests.test (newPage,   "newPage",   "<>")
    tests.test (setTitle,  "setTitle",  "<>")


###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
