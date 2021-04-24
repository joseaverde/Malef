#!/usr/bin/env  python3
# *-* encoding=utf8 *-*
#=============================================================================#
 #                                                                           # 
 #                              S E T U P . P Y                              # 
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


from distutils.core import setup, Extension
import os
import sys


def _get_version () -> str:
    try:
        file = open("../VERSION", 'r');
    except FileNotFoundError:
        return "0.0.0";
    else:
        data = file.read()
        file.close()
        del file;

        return data.rstrip('\n');


def _get_description () -> str:
    description = "Malef is a TUI library."
    try:
        file = open("../alire.toml", 'r')
    except FileNotFoundError:
        return description
    else:
        line = file.readline()
        while line != '':
            line = line.split()
            try:
                if line[0] == "long-description":
                    description = line[2]
                    break;
            except IndexError:
                pass;
            line = file.readline()
        file.close()
        del file;

        return description;


def main ():
    if sys.platform == "win32":
        system = "windows"
    elif sys.platform == "linux":
        system = "linux"
    else:
        system = "unix"
    runtime_library_dirs = ["../alire/build/lib-"+system+"."+subsystem
                            for subsystem in ["ansi", "cmd"]]

    if system == "windows":
        # TODO: Fix this
        runtime_library_dirs = []

    src_base = Extension(name         = "malef",
                         sources      = ["src-base/py_malef.c"],
                         include_dirs = ["../c-malef/include"],
                         depends      = [os.path.join("src-base", f)
                                            for f in os.listdir("src-base")
                                            if f.endswith(".h")],
                         extra_compile_args = ["-Wall", "-Werror",
                                               "-std=c99", "-pedantic"],
                         library_dirs = ["../alire/build/lib-"+system],
                         libraries    = ["Malef"],
                         runtime_library_dirs = runtime_library_dirs,
                         )
    setup(name         = "malef",
          version      = _get_version(),
          description  = _get_description(),
          author       = "José Antonio Verde Jiménez",
          author_email = "joseaverde@pm.me",
          ext_modules  = [src_base])


if __name__ == "__main__":
    main()


###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
