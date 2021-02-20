#!/bin/bash

if [ "$1" = "--compile" ]; then
   python3 setup.py build --force
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

