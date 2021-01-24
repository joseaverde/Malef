#!/bin/bash

OS=$(echo "$(uname -s)" | awk '{print tolower($0)}')
LIB=$(readlink -f ../alire/build/lib-$OS)
PYLIB=$(readlink -f ../py-malef/build/)

cd py/src
export LD_LIBRARY_PATH=$LIB:$LD_LIBRARY_PATH && python3 main.py $PYLIB

# EOF
