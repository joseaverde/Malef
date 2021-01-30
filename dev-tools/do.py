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
 #  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  # 
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


dirs = ["ada-malef", "c-malef", "py-malef", "tests"]
temp = dirs.copy()

while len(temp) != 0:
    d = temp.pop(0)
    _ = filter(lambda f: os.path.isdir(f)
                    and os.path.basename(f) not in ["obj", "bin"],
               [os.path.join(d, f) for f in os.listdir(d)])
    _ = list(_)
    temp += _.copy()
    dirs += _.copy()

del temp

def _lines():
    sources = []
    for d in dirs:
        sources += [os.path.join(d, f) for f in os.listdir (d)
                                       if  os.path.isfile(os.path.join(d, f))
                                       and f.split('.')[-1] in
                                            ["ads", "adb", "c",
                                             "h", "py", "gpr",
                                             "awk", "sh"]]

    data = {}
    max_size = 0
    max_lines = 0
    max_len = 0
    for source in sources:
        size = os.stat(source).st_size
        file = open(source, 'r')
        lines = file.read().count('\n')
        file.close()
        del file
        data.update({source: (size, lines)})

        if size > max_size:
            max_size = size
        if lines > max_lines:
            max_lines = lines
        if len(source) > max_len:
            max_len = len(source)

    total_size = 0
    total_lines = 0
    _max_size = len(str(max_size)) + 1
    _max_lines = len(str(max_lines)) + 1
    _max_len = max_len + 1
    for source in data:
        size, lines = data[source]
        total_size += size
        total_lines += lines

        source = source + (_max_len - len(source)) * ' '
        print("\033[36;1m%s\033[0m: " % source, end="")

        size = str(size)
        lines = str(lines)
        size = (_max_size - len(size)) * ' ' + size
        lines = (_max_lines - len(lines)) * ' ' + lines
        print("size =\033[31m%s\033[0mB  "\
              "lines=\033[32m%s\033[0m" % (size, lines))

    print()
    print("\033[36;1mTOTAL\033[0m:")
    
    def get_size (size):
        n = 0
        while size / (1024**n) > 1:
            n += 1
        if n != 0:
            n -= 1

        units = ["B", "KiB", "MiB", "GiB"]
        return (size / (1024 ** n), units[n])

    print("\tSIZE  = \033[31m%.4f\033[0m%s" % get_size(total_size))
    print("\tLINES = \033[32m%d\033[0m" % total_lines)


def _todo():
    TODO = {}
    files = []
    for d in dirs:
        files += [os.path.join(d, f) for f in os.listdir (d)
                                     if  os.path.isfile(os.path.join(d, f))
                                     and f.split('.')[-1] in
                                            ["ads", "adb", "c",
                                             "h", "py", "gpr",
                                             "awk", "sh"]]


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
                  "You've added a description",
                  "The date range has been updated in all modified files",]:
            print(" \033[33m+\033[0m %s" % t)


_ = ",".join(filter(lambda f: f.split('.')[-1] == "ads",
             os.listdir("c-malef/src-base")))

commands = {
        "linux": "gprbuild -p -Pmalef",
        "windows": "wine gprbuild -p -Pmalef "
                   "-XMALEF_OPERATING_SYSTEM=windows",
        "tests-linux": ["cd tests && ./run-tests.sh && cd .."],
        "tests-windows": ["wine gprbuild -p -Ptests/ada/tests.gpr "
                          "-XMALEF_OPERATING_SYSTEM=windows",
                          "cp alire/build/lib-windows/libMalef.dll "
                          "tests/ada/bin/"],
        "docs": "gnatdoc -bplwPmalef --enable-build --ignore-files=%s"%_,
        "todo": _todo,
        "commit": _commit,
        "lines": _lines,
        "install": "gprinstall -f -p -Pmalef --prefix=./alire/opt",
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

    for arg in sys.argv[1:]:
        if arg not in commands.keys():
            usage()
            return 2;

        cmd = commands[arg]

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
