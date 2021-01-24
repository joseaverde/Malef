#!/usr/bin/env  python3
# *-* encoding=utf8 *-*
#=============================================================================#
 #                                                                           # 
 #                              T E S T S . P Y                              # 
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
import time

global avoid_delays
avoid_delays = False


def eprint (*args) -> None:
    print ('\t'.join(args), file=sys.stderr)


def safe_run (unit) -> None:
    try:
        unit ()
    except TypeError:
        raise TypeError
    except:
        pass;


def start (pkg="<>") -> None:
    if pkg == "<>":
        eprint ("language", "py")
    else:
        eprint ("package", pkg)


def test (unit,
          name     : str,
          expected : str  = "<>",
          time_it  : bool = True) -> None:

    if avoid_delays and not time_it:
        eprint ("skipped", "name")
    start = time.time ()
    try:
        output = unit ()
    except TypeError:
        raise TypeError
    except NameError:
        raise NameError
    except Exception as exception:
        got = type(exception).__name__
    else:
        got = output
    finally:
        finish = time.time ()
        if expected == got:
            eprint ("passed", name, "%.9f" % (finish-start) if time_it else
                                    "<null>")
        else:
            eprint ("failed", name, expected, got)


def wait (text: str) -> None:
    print (text + ", press ENTER to continue...")
    input ()


def wrap (proc,
          name : str) -> None:
    start (name)
    try:
        proc ()
    except NameError:
        raise NameError
    except TypeError:
        raise TypeError
    except Exception as exception:
        print (type(exception).__name__)
        eprint ("fatal", name)


###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
