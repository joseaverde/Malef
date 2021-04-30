#!/usr/bin/env  python3
# *-* encoding=utf8 *-*
#=============================================================================#
 #                                                                           # 
 #                              U T I L S . P Y                              # 
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

import sys


def todo ():
    print ("\033[33;2m[TODO]\033[0m", file=sys.stderr)

def debug (message : str,
           depth   : int = 0) -> None:
    """
    This function prints debug information onto the screen if debug mode is
    enabled this makes it simple to debug without having to remove debug
    information every single time something starts working again.
    """
    print ("\033[36;1m[DBG#]\033[0m %s\033[2m%s\033[0m" % (' '*depth, message))


def error (message  : str,
           error_id : int = 0,
           depth    : int = 0) -> None:
    """
    This function prints an error to stderr and exists with a number if it's
    different than zero. Each section has its own error codes.
    The depth is only used for tabulations.
    """
    print ("\033[31;1m[ERR!]\033[0m %s\033[2m%s\033[0m" % (' '*depth, message),
           file=sys.stderr)
    if error_id != 0:
        sys.exit (error_id)


def info (message : str,
          depth   : int = 0) -> None:
    """
    This function prints information onto the screen starting with certain
    tabulation (depth).
    """
    print ("\033[33;1m[INFO]\033[0m %s\033[2m%s\033[0m" % (' '*depth, message))


def get_version ():
    file = open ("../VERSION", 'r')
    version = tuple(map(int, file.read().rstrip('\n').split('.')))
    file.close ()
    del file
    return version


def get_description ():
    file = open ("../alire.toml", 'r')
    line = file.readline ()
    while line != '':
        line = line.split()
        if len(line)!= 0:
            if line[0] == "long-description":
                file.close()
                del file
                return str(" ".join(line[2:]).rstrip('"').lstrip('"'))
        line = file.readline ()

    return None

RAW_VERSION = get_version ()
DEB_VERSION = "%d.%d-%d" % RAW_VERSION if RAW_VERSION[-1] != 0 else \
              "%d-%d" % RAW_VERSION[:-1]
VERSION     = "%d.%d.%d" % RAW_VERSION

DESCRIPTION = get_description ()

AUTHOR = "José Antonio Verde Jiménez"
EMAIL  = "joseaverde@pm.me"


###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
