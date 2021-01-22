#!/usr/bin/env  python3
# *-* encoding=utf8 *-*
#=============================================================================#
 #                                                                           # 
 #                            S R C - G E N . P Y                            # 
 #                                                                           # 
 #                                 M A L E F                                 # 
 #                                                                           # 
 #                         P Y T H O N   S C R I P T                         # 
 #                                                                           # 
 #---------------------------------------------------------------------------# 
 #  Copyright (c) 2020-2021 José Antonio Verde Jiménez All Rights Reserved   # 
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

#=============================================================================#
 #                                                                           #
 #  This script is used to generate the source files for the Malef project,  #
 #       because writting and copy-pasting the license is very boring.       #
 #                                                                           #
#=============================================================================#

HEADER = """
                                                                           
{filename}
                                                                           
                                 M A L E F                                 
                                                                           
{kind}
                                                                           
---------------------------------------------------------------------------
     Copyright (c) 2021 José Antonio Verde Jiménez All Rights Reserved     
---------------------------------------------------------------------------
 This file is part of Malef.                                               
                                                                           
 This program is free software:  you  can redistribute it and/or modify it 
 under  the terms  of the  GNU  General License  as published by the  Free 
 Software  Foundation,  either  version 3  of  the  License,  or  (at your 
 opinion) any later version.                                               
                                                                           
 This  program  is distributed  in the  hope that  it will be  useful, but 
 WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of 
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General 
 Public License for more details.                                          
                                                                           
 You should have received  a copy of the  GNU General Public License along 
 with this program. If not, see <https://www.gnu.org/licenses/>.           
                                                                           
"""[1:-1]


EOF = """
---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
"""[1:-1]

import os
import sys


def center(text, line=79 - 4):
    spaces = (line - len(text)) // 2

    return spaces*' ' + text + ' '*spaces

def main():
    if len(sys.argv) != 2:
        print("USAGE: `" + sys.argv[0] + " [FILENAME]'",
              file=sys.stderr)
        sys.exit(1)

    filename = sys.argv[1]

    if os.path.exists(filename):
        print(filename + ": File exists!",
              file=sys.stderr)
        sys.exit(2)

    ext = filename.split('.')[-1]
    base_name = filename.replace('\\', '/').split('/')[-1]

    _ = ""
    for c in base_name.upper():
        _ += c + ' '
    header_filename = center(_[:-1])

    if ext in ["adb", "ads", "gpr", "ada"]:
        if ext == "adb":
            header_kind = center("B O D Y")
            body = "package body {pkg} is\n" \
                   "\n"                      \
                   "end {pkg};\n";
        elif ext == "ads":
            header_kind = center("S P E C")
            body = "--\n-- @summary\n--\n--\n-- @description\n--\n" \
                   "package {pkg} is\n" \
                   "\n"                      \
                   "end {pkg};\n";
        elif ext == "gpr":
            header_kind = center("G P R")
            body = "project {pkg} is\n" \
                   "\n"                 \
                   "end {pkg};\n"
        elif ext == "ada":
            header_kind = center("M A I N");
            body = "procedure {pkg} is\n" \
                   "\n"                   \
                   "begin\n"              \
                   "\n"                   \
                   "end {pkg};\n"

        _ = base_name.split('.')[0]
        pkg = ".".join([s.capitalize() for s in _.split('-')])
        body = body.format(pkg=pkg)
        comment = '-'
        wrapper = "--"
        top    = "-" * 79
        bottom = top
        shebang = ""

    elif ext in ["c", "h"]:
        if ext == "c":
            header_kind = center("C   S O U R C E")
            body = ""
        elif ext == "h":
            header_kind = center("C   H E A D E R")
            body = "#ifndef MALEF_{pkg}_H\n" \
                   "#define MALEF_{pkg}_H\n" \
                   "\n"                \
                   "#endif//MALEF_{pkg}_H\n"
        pkg = base_name.split('.')[0]
        pkg = pkg.upper().replace('-', '_')
        body = body.format(pkg=pkg)
        comment = '/'
        wrapper = " *"
        top    = '/' + '*'*77 + '\\'
        bottom = '\\' + '*'*77 + '/'
        shebang = ""

    elif ext in ["py", "awk", "sh"]:
        if ext == "py":
            header_kind = center("P Y T H O N   S C R I P T")
            body = """"""
            shebang = "#!/usr/bin/env  python3\n" \
                      "# *-* encoding=utf8 *-*\n"
        elif ext == "awk":
            header_kind = center("A W K   S C R I P T")
            body = "BEGIN {\n\n" \
                   "}\n\n\n" \
                   "END {\n\n"   \
                   "}\n\n"
            shebang = "#!/usr/bin/awk -f\n"
        elif ext == "sh":
            header_kind = center("B A S H   S C R I P T")
            body = ""
            shebang = "#!/bin/bash\n"
        comment = '#'
        wrapper = " #"
        top    = '#' + '='*77 + '#'
        bottom = top


    else:
        print(ext + ": Unknown extension!",
              file=sys.stderr)
        sys.exit(3)

    header = top + '\n'
    for line in HEADER.format(filename = header_filename,
                              kind     = header_kind).split('\n'):
        header += wrapper + line + wrapper[::-1] + '\n'
    header += bottom + '\n'
    eof = EOF.replace('-', comment)
    

    file = open(filename, 'w')
    file.write(shebang)
    file.write(header)
    file.write('\n')
    file.write(body)
    file.write('\n')
    file.write(eof)
    file.write('\n')
    file.close()

    sys.exit(0)


if __name__ == "__main__":
    main()

###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
