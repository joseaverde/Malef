#!/bin/bash

if [ "$1" = "--compile" ]; then
   python3 setup.py build --force
   exit 0
fi

if [ "$1" = "--win32" ]; then
   wine ~/.wine/drive_c/Program\ Files\ \(x86\)/Python39-32/python.exe \
      setup.py build -v -f --compile=mingw32
   cp ~/.wine/drive_c/GNAT/2017/bin/libgcc_s_dw2-1.dll build/lib.win32-3.9
   cp ~/.wine/drive_c/GNAT/2017/bin/libgnat-2017.dll build/lib.win32-3.9
   cp ~/.wine/drive_c/GNAT/2017/bin/libgnarl-2017.dll build/lib.win32-3.9
   cp ../alire/build/lib-windows/*.dll build/lib.win32-3.9
   exit 0
fi

if [ "$1" = "--run-win32" ]; then
   cd build/lib.win32-3.9 && wine python
   exit 0
fi

cd build/$(python3 -c \
'import os, sys
print("lib.%s-%s-%d.%d" % (sys.platform,
                           os.uname().machine,
                           sys.version_info.major,
                           sys.version_info.minor))
') || exit 1

export LD_LIBRARY_PATH=$(readlink -f ../../../alire/build/lib-linux) && \
   echo $LD_LIBRARY_PATH && python3

