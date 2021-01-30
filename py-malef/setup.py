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


import os
from distutils.core import setup, Extension

def _get_version () -> str:
    try:
        file = open("../VERSION");
    except FileNotFoundError:
        return "0.0.0";
    else:
        data = file.read()
        file.close()
        del file;

        return data.rstrip('\n');


def main ():
    src_base = Extension(name         = "malef",
                         sources      = ["src-base/py_malef.c"],
                         include_dirs = ["../c-malef/include"],
                         depends      = [os.path.join("src-base", f)
                                            for f in os.listdir("src-base")
                                            if f.endswith(".h")],
                         extra_compile_args = ["-Wall", "-Werror"],
                         #define_macros = [(name, value)]
                         #undef_macros = []
                         library_dirs = ["../alire/build/lib-linux"],
                         libraries    = ["Malef"],
                         runtime_library_dirs = ["../alire/build/lib-linux"])
    setup(name         = "malef",
          version      = _get_version(),
          description  = "MALEF", # TODO
          author       = "José Antonio Verde Jiménez",
          author_email = "joseaverde@pm.me",
          ext_modules  = [src_base])


if __name__ == "__main__":
    main()


###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
