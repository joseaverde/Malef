#!/bin/bash

if [ "$1" = "--compile" ]; then
   python3 setup.py build --force
   exit 0
fi

cd build/lib*
export LD_LIBRARY_PATH=/home/jose/Development/Malef/alire/build/lib-linux && \
   echo $LD_LIBRARY_PATH && python3

