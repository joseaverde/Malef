#!/usr/bin/env  python3
# *-* encoding=utf8 *-*
#=============================================================================#
 #                                                                           # 
 #                           M A L E F - D B . P Y                           # 
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

import json
import os
import sys


fields = {
    "general" : {
        "subsystem"    : "ANSI",
        "config-files" : [],
        "syntax"       : {
            "black"   : ["", "string"],
            "red"     : ["", "string"],
            "yellow"  : ["", "string"],
            "green"   : ["", "string"],
            "blue"    : ["", "string"],
            "magenta" : ["", "string"],
            "cyan"    : ["", "string"],
            "white"   : ["", "string"],

            "bright-black"   : ["", "string"],
            "bright-red"     : ["", "string"],
            "bright-yellow"  : ["", "string"],
            "bright-green"   : ["", "string"],
            "bright-blue"    : ["", "string"],
            "bright-magenta" : ["", "string"],
            "bright-cyan"    : ["", "string"],
            "bright-white"   : ["", "string"],

            "foreground" : ["", "string"],
            "background" : ["", "string"]
        }
    },
    "defaults" : {
        "black"   : (0, 0, 0),
        "red"     : (0, 0, 0),
        "yellow"  : (0, 0, 0),
        "green"   : (0, 0, 0),
        "blue"    : (0, 0, 0),
        "magenta" : (0, 0, 0),
        "cyan"    : (0, 0, 0),
        "white"   : (0, 0, 0),

        "bright-black"   : (0, 0, 0),
        "bright-red"     : (0, 0, 0),
        "bright-yellow"  : (0, 0, 0),
        "bright-green"   : (0, 0, 0),
        "bright-blue"    : (0, 0, 0),
        "bright-magenta" : (0, 0, 0),
        "bright-cyan"    : (0, 0, 0),
        "bright-white"   : (0, 0, 0),

        "foreground" : "black",
        "background" : "white"
    }
    "styles" : {
        "bold"             : False,
        "faint"            : False,
        "italic"           : False,
        "underline"        : False,
        "slow-blink"       : False,
        "rapid-blink"      : False,
        "reverse-video"    : False,
        "conceal"          : False,
        "crossed-out"      : False,
        "doubly-underline" : False,
    },
    "colors" : {
        "3-bits"  : False,
        "4-bits"  : False,
        "8-bits"  : False,
        "24-bits" : False,
    },
}


versions = [(0, 2, 4)]
subsystems = ["ANSI", "CMD"]
syntax_fields = list(fields["general"]["syntax"].keys())
syntax_types = ["string", "integer", "hash-hexadecimal"]


def get_version ():
    for i in range(512):
        try:
            file = open(i*"../" + "VERSION", 'r')
        except FileNotFoundError:
            pass;
        else:
            raw = file.read().rstrip('\n').split('.')
            file.close ()
            return tuple (map (int, raw))

    print ("Couldn't find VERSION file!", file=sys.stderr)
    sys.exit (7)


def to_bytes (item, size=2):
    arr = []
    n = 0
    while item // 256**n > 0:
        arr.append(item // 256**n % 256)
        n += 1
    for i in range(n, size):
        arr.append(0)
    return bytes(arr)


def path_to_bytes (item):
    _path = filter(lambda x: x != "", item.replace('\\', '/').split('/'))
    path = []
    for dir in _path:
        if dir.startswith('$'):
            path.append("\0%s\0" % dir.lstrip('$'))
        else:
            path.append(dir)
    path = ("/" if item[0] in ['/', '\\'] else "") + "/".join(path)

    return bytes(path, "ascii")


def compile (raw):
    # HEADER #
    try:
        subsys = subsystems.index(raw["general"]["subsystem"].upper())
    except ValueError:
        print ("Invalid subsystem `%s'!" % raw["general"]["subsystem"])
        return 8
    header = b"mdb" + bytes(get_version()) + bytes(versions[-1]) + bytes([subsys])

    # POST HEADER #
    # Configuration files
    config_files = raw["general"]["config-files"]
    post_header = to_bytes(len(config_files), 1)
    for file in config_files:
        path = path_to_bytes(file)
        post_header += to_bytes(len(path)) + path
    # Regular Expressions
    syntax = raw["general"]["syntax"]
    for field in syntax_fields:
        field = syntax[field]
        if field[0] == '':
            post_header += bytes([0])
        else:
            regex = bytes(field[0], "ascii")
            post_header += to_bytes(len(regex), 1) + regex + \
                    to_bytes(syntax_types.index(field[1]), 1)
    # TODO: Defaults

    # BODY #
    body = b""
    for field in ["styles", "colors"]:
        for style in fields[field]:
            body += bytes([1]) if raw[field][style] else bytes([0])

    return header + to_bytes(len(post_header)) + post_header + body


def update (data, fields):
    for field in fields.keys():
        if field not in data:
            data.update ({field : fields[field]})
        else:
            if isinstance (data[field], dict):
                update (data[field], fields[field])



def main () -> int:
    options = ["create", "update", "compile"]
    if len(sys.argv) == 1 or sys.argv[1] not in options:
        print ("USAGE: `%s [OPTION=%s] [ARGS]...'" %
                (sys.argv[0], " ".join(options)),
               file=sys.stderr)
        return 1

    option = sys.argv[1]

    if option == "create":
        if len(sys.argv) != 3:
            print ("USAGE `%s create [PATH]'" % sys.argv[0],
                   file=sys.stderr)
            return 2
        path = sys.argv[2]

        if path.split('.')[-1] != "mdbraw":
            path += ".mdbraw"
        leading_path = '/'.join(path.replace('\\', '/').split('/')[:-1])
        leading_path = '/' + leading_path if path[0] == '/' else leading_path
        if leading_path != '' and not os.path.exists (leading_path):
            os.makedirs (leading_path)

        file = open (path, 'w')
        json.dump (fields, file, indent=3)
        file.close ()

    elif option == "update":
        if len(sys.argv) != 3:
            print ("USAGE `%s update [PATH]'" % sys.argv[0],
                   file=sys.stderr)
            return 3
        path = sys.argv[2]

        if path.split('.')[-1] != "mdbraw":
            path += ".mdbraw"

        if not os.path.isfile (path):
            print ("%s: Not such file!" % path, file=sys.stderr)
            return 4

        file = open (path, 'r')
        old_data = json.load (file)
        file.close()

        update (old_data, fields)
        file = open (path, 'w')
        json.dump (old_data, file, indent=3)
        file.close ()

    elif option == "compile":
        if len(sys.argv) == 3:
            output = sys.argv[2].replace('\\', '/').split('/')[-1]
            if output.split('.')[-1] == "mdbraw":
                output = output[:-3]
            elif output.split('.')[-1] != "mdb":
                output += ".mdb"
            sys.argv.append(output)
        elif len(sys.argv) != 4:
            print ("USAGE: `%s compile [SOURCE] [OUTPUT]'" % sys.argv[0],
                   file=sys.stderr)
            return 5

        source = sys.argv[2]
        output = sys.argv[3]

        if source.split('.')[-1] != "mdbraw":
            source += ".mdbraw"
        if output.split('.')[-1] != "mdb":
            output += ".mdb"

        if not os.path.isfile (source):
            print ("%s: Not such file!" % source, file=sys.stderr)
            return 6

        leading_path = '/'.join(output.replace('\\', '/').split('/')[:-1])
        leading_path = '/' + leading_path if output[0] == '/' else leading_path
        if leading_path != '' and not os.path.exists (leading_path):
            os.makedirs (leading_path)

        file = open (source, 'r')
        data = json.load (file)
        file.close ()

        try:
            data = compile (data)
        except KeyError:
            print ("Invalid data, try updating the file!", file=sys.stderr)
            return 254
        if isinstance (data, int):
            return data

        file = open (output, 'wb')
        file.write (data)
        file.close ()

    else:
        return 255

    return 0


if __name__ == "__main__":
    sys.exit(main())


###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
