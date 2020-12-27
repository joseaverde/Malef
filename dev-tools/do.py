#!/usr/bin/env  python3
# *-* encoding=utf8 *-*
#=============================================================================#
 #                                                                           # 
 #                                 D O . P Y                                 # 
 #                                                                           # 
 #                                 M A L E F                                 # 
 #                                                                           # 
 #                         P Y T H O N   S C R I P T                         # 
 #                                                                           # 
 #---------------------------------------------------------------------------# 
 #     Copyright (c) 2020 José Antonio Verde Jiménez All Rights Reserved     # 
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
 #  This script is used to avoid writing the same commands again and again.  #
 #                                                                           #
#=============================================================================#


import os
import sys


dirs = ["ada-malef"]
temp = dirs.copy()

while len(temp) != 0:
    d = temp.pop(0)
    _ = filter(os.path.isdir, [os.path.join(d, f) for f in os.listdir(d)])
    dirs += _
    temp += _

del temp

def _todo():
    TODO = {}
    files = []
    for d in dirs:
        files += filter(os.path.isfile,
                        [os.path.join(d, f) for f in os.listdir(d)])

    for file in files:
        _TODO = []
        fp = open(file, 'r')
        data = fp.read()
        fp.close()
        del fp
        l = 0
        for line in data.split('\n'):
            l += 1
            if "TODO:" in line:
                _TODO.append([l, line])

        if len(_TODO) != 0:
            TODO.update({file: _TODO})

    for file in TODO.keys():
        print("\033[36;1m%s\033[0m:" % file)
        for line, data in TODO[file]:
            l = str(line)
            l = l + ' '*(4 - len(l))
            data = data.lstrip(' ').replace("TODO",
                                            "\033[31mTODO\033[0m\033[2m")
            print("   \033[33mline \033[32;1m%s\033[0m: \033[2m%s\033[0m" %
                        (l, data.lstrip(' ')))


def _commit():
    def read (filename):
        file = open(filename, 'r')
        data = file.read()
        file.close()
        del file;
        return data;

    def status (filename, status, description=""):
        print("\033[36;1m%s\033[0m: " % filename, end="")
        if status:
            print("\033[32;1m[DONE]\033[0m")
        else:
            print("\033[31;1m[FAIL]\033[0m \033[2m%s\033[0m" % description)

    version = read("VERSION").replace('\n', "")
    done = True

    # alire.toml
    alire_toml = read("alire.toml").split('\n')
    for line in alire_toml:
        if '=' in line:
            line = line.split(" = ")
            if line[0] == "version":
                if line[1] != '"%s"' % version:
                    status("alire.toml", False,
                            'Change: version = "%s"' % version)
                    done = False
                else:
                    status("alire.toml", True)
                break;


    # CHANGELOG.md
    changelog_md = read("CHANGELOG.md").split('\n')
    for line in changelog_md:
        if line[:3] == "## ":
            line = line.split(' ')[1]
            if len(line.split('.')) != 3:
                continue;
            if line != "[%s]" % version:
                status("CHANGELOG.md", False, "Add new version [%s]" % version)
                done = False
            else:
                status("CHANGELOG.md", True)
            break;

    if not done:
        print("\033[31mYou can't commit just yet!\033[0m")
    else:
        print("You can commit!")
        print("Remember to check:")
        for t in ["It compiles for all systems",
                  "You've added a description"]:
            print(" \033[33m+\033[0m %s" % t)


commands = {
        "linux": "gprbuild -p -Pmalef",
        "windows": "wine gprbuild -p -Pmalef "
                   "-XMALEF_OPERATING_SYSTEM=windows",
        "tests-linux": "gprbuild -p -Ptests/ada/tests.gpr",
        "tests-windows": ["wine gprbuild -p -Ptests/ada/tests.gpr "
                          "-XMALEF_OPERATING_SYSTEM=windows",
                          "cp alire/build/lib-windows/libMalef.dll "
                          "tests/ada/bin/"],
        "docs": "gnatdoc -bplwPmalef --enable-build",
        "todo": _todo,
        "commit": _commit,
}


def usage():
    print("USAGE: `%s [OPERATION]'" % sys.argv[0])
    print("")
    print("OPERATIONS:")
    for op in commands.keys():
        print(" *", op)


def main():
    if len(sys.argv) == 1:
        usage()
        return 1;

    if sys.argv[1] not in commands.keys():
        usage()
        return 2;

    cmd = commands[sys.argv[1]]

    if isinstance(cmd, str):
        print("\033[2m%s\033[0m" % cmd)
        os.system(cmd)
    elif isinstance(cmd, list):
        for c in cmd:
            if isinstance(c, str):
                print("\033[2m%s\033[0m" % c)
                os.system(c)
            else:
                c()
    else:
        cmd()

    return 0;


if __name__ == "__main__":
    sys.exit(main())


###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
