#!/usr/bin/env  python3
# *-* encoding=utf8 *-*
#=============================================================================#
 #                                                                           # 
 #                                D E B . P Y                                # 
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

import datetime
import os
import shutil
import tarfile
import utils


def format_date (date):
    year, month, day = map(int, date.split('-'))
    add_zero = lambda n : "0" + n if len(n) == 1 else n
    if day == 0:
        day = add_zero(str(datetime.date.today().day))
        date = "-".join([str(year), add_zero(str(month)), day])
        day = int(day)
    weekday = ["Mon", "Tue", "wed", "Thu", "Fri", "Sat", "Sun"][
                    datetime.date.fromisoformat(date).weekday()]
    month = ["Jan", "Feb", "Mar", "Apr",
             "May", "Jun", "Jul", "Aug",
             "Sep", "Oct", "Nov", "Dec"][month - 1]

    return "%s, %d %s %d 00:00:00 +0000" % (weekday, day, month, year)


def format_description (desc):
    new_desc = ["Description: "]
    old_desc = desc.split()
    for word in old_desc:
        if len(new_desc[-1]) + len(word) >= 66:
            new_desc[-1].rstrip(' ')
            new_desc.append(" ")
        new_desc[-1] += word + ' '
    new_desc[0] = new_desc[0][len("Description: "):]

    return '\n'.join(new_desc)


def gen_changelog (path):
    utils.info ("Generating debian/changelog", 3)
    out_file = open (path, 'w')
    in_file = open ("../CHANGELOG.md", 'r')

    data = {}
    line = in_file.readline ()
    while line != "":
        line = line.split()
        if len(line) != 0:
            if line[0] == "##":
                try:
                    list(map(int, line[1].lstrip('[').rstrip(']').split('.')))
                    version = "%s.%s-%s" % \
                            tuple(line[1].lstrip('[').rstrip(']').split('.'))
                except ValueError or TypeError:
                    release = line[1].upper().rstrip(']').lstrip('[')
                else:
                    date = line[3]
                    data.update ({version : {"date"    : date,
                                             "release" : release,
                                             "added"   : [],
                                             "fixed"   : []}})
            elif line[0] == "###":
                action = line[1].lower()
            elif line[0] == '-':
                data[version][action].append(" ".join(line[1:]))
        line = in_file.readline ()

    format_changes = lambda l : "  * " + "\n  * ".join(l) if l != [] else ""
    for version in data.keys ():
        _ = version.split('.')
        _ = tuple(map(int, ([_[0]] + _[1].split('-'))))
        if _ > utils.RAW_VERSION:
            continue
        out_file.write("""malef ({version}) {release}; urgency=low\n
  
  [ Added ]
{added}

  [ Fixed ]
{fixed}

 -- {author} <{email}>  {date}

""".format(
        version = version,
        release = data[version]["release"],
        added   = format_changes(data[version]["added"]),
        fixed   = format_changes(data[version]["fixed"]),
        author  = utils.AUTHOR,
        email   = utils.EMAIL,
        date    = format_date(data[version]["date"])
    ))

    out_file.close ()
    in_file.close ()
    del out_file, in_file


def gen_compat (path):
    utils.info ("Generating debian/compat", 3)
    file = open (path, 'w')
    # TODO: Research more
    file.write ("9\n")
    file.close ()
    del file

def gen_control (path):
    # ${misc:Depends}
    utils.info ("Generating debian/control", 3)
    data = """Source: malef
Maintainer: {author} <{email}>
Section: libdevel
Priority: optional
Standards-Version: {version}
Build-Depends: gprbuild,
 gcc  (>= 8) | gcc-8  | gcc-9  | gcc-10,
 gnat (>= 8) | gnat-8 | gnat-9 | gnat-10,
 python3-dev,
 gnat-gps,
 debhelper (>= 7)
Homepage: https://github.com/joseaverde/Malef
Vcs-Git: https://github.com/joseaverde/Malef.git

Package: malef
Architecture: any
Depends: ${{misc:Depends}}, ${{shlibs:Depends}},
 libgnat (>= 8) | libgnat-8 | libgnat-9 | libgnat-10
Recommends: malef-ansi
Description: _{description}
 .
 This package is the original implementation in Ada.

Package: c-malef
Architecture: any
Depends: ${{misc:Depends}}, malef
Recommends: malef-ansi
Description: {description}
 .
 This package is a binding to run and create programs written in
 C for Malef.

Package: py-malef
Architecture: any
Depends: ${{misc:Depends}}, python3 (>= 3.6), c-malef
Recommends: malef-ansi
Description: {description}
 .
 This package is a binding to write programs with Python3 and Malef.

Package: malef-doc-html
Section: doc
Architecture: all
Description: {description}
 .
 This package contains the documentation for the original
 implementation of Malef in Ada. It can be viewed on any web
 browser.

Package: malef-doc
Section: doc
Architecture: all
Description: {description}
 .
 This package contains the documentatation for Malef in general.

Package: malef-ansi
Section: lib
Depends: ${{misc:Depends}}, malef
Architecture: all
Description: {description}
 .
 This package contains one of the many subsystems of Malef. It's
 meant to control the behaviour of ANSI-compliant terminals, like
 the ones available on GNU/Linux.
""".format(author = utils.AUTHOR,
           email  = utils.EMAIL,
           version = utils.VERSION,
           description = format_description (utils.DESCRIPTION))
    file = open (path, 'w')
    file.write (data)
    file.close ()
    del file


def gen_copyright (path):
    utils.info ("Generating debian/copyright", 3)
    data = """
Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/
Upstream-Name: Malef
Upstream-Contact: {author} <{email}>
Source: https://github.com/joseaverde/Malef.git
License: GPL-3
Copyright: 2020-2021 José Antonio Verde Jiménez


Files: *
Copyright: 2020-2021 José Antonio Verde Jiménez
License: GPL3
 This program is free software: you can redistribute it and/or modify it
 under the terms of the GNU General License as published by the Free
 Software Foundation, either version 3 of the License, or (at your
 opinion) any later version.
 .
 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 Public License for more details.                                         
 .
 You should have received a copy of the GNU General Public License along
 with this program. If not, see <https://www.gnu.org/licenses/>.
 .
 On Debian systems, the full text of the GNU General Public License version
 can be found int the file
 `/usr/share/common-licenses/GPL-3`


Files: *.tex
Copyright: 2020-2021 José Antonio Verde Jiménez
License: GFDL-NIV-1.3
 Permission is granted to copy, distribute and/or modify this document
 under the terms of the GNU Free Documentation License, Version 1.3 or any
 later version published by the Free Software Foundation; with no
 Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
 A copy of the license is included in the section entitled "GNU Free
 Documentation License" or the files "fdl.tex" or "LICENSE.fdl-v1.3".
"""[1:].format(author = utils.AUTHOR,
           email  = utils.EMAIL)
    file = open (path, 'w')
    file.write (data)
    file.close ()
    del file


def gen_rules (path):
    utils.info ("Generating debian/rules", 3)
    utils.todo()
    shutil.copy ("deb-rules", path)


def gen_sources_format (path):
    utils.info ("Generating debian/sources/format", 3)
    file = open (path, 'w')
    file.write ("3.0 (quilt)\n")
    file.close ()
    del file


def gen_upstream_tarball ():
    utils.info ("Generating upstream tarball")
    BUILD   = os.path.join (os.getcwd(), "build", "deb")
    ORIGIN  = os.path.join (BUILD, "malef-"+utils.DEB_VERSION)
    DEBIAN  = os.path.join (ORIGIN, "debian")
    SOURCES = os.path.join (DEBIAN, "sources")
    TARNAME = os.path.join (BUILD, "malef_%s.orig.tar.xz" %
                ".".join(utils.VERSION.split('.')[:-1]))

    if not os.path.isdir (BUILD):
        os.makedirs (BUILD)

    if not os.path.isdir (ORIGIN):
        utils.info ("Cloning package", 3)
        # utils.debug ("Using a copy command instead of git clone for debugging")
        os.system ("git clone .. " + ORIGIN)
        #shutil.copytree ("..", "/tmp/malef")
        # shutil.move("/tmp/malef", ORIGIN)

    if not os.path.isdir (DEBIAN):
        os.makedirs (DEBIAN)

    if not os.path.isdir (SOURCES):
        os.makedirs (SOURCES)

    if os.path.isdir (os.path.join(ORIGIN, ".git")):
        shutil.rmtree (os.path.join(ORIGIN, ".git"))

    gen_changelog (os.path.join(DEBIAN, "changelog"))
    gen_compat (os.path.join(DEBIAN, "compat"))
    gen_control (os.path.join(DEBIAN, "control"))
    gen_copyright (os.path.join(DEBIAN, "copyright"))
    gen_rules (os.path.join(DEBIAN, "rules"))
    gen_sources_format (os.path.join(SOURCES, "format"))

    utils.info ("Compressing", 3)
    if os.path.isfile (TARNAME):
        os.remove (TARNAME)
    tar = tarfile.open (TARNAME, "x:gz")
    tar.add (ORIGIN, "malef-"+utils.DEB_VERSION)
    tar.close ()
    utils.info ("Finished!")


###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
