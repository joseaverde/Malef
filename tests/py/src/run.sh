#!/bin/bash

SYS=$(echo "$(uname -s)" | awk '{print tolower($0)}')

# case OS in
#    linux)
#       SUB=ansi
#    ;;
#    windows)
#       SUB=cmd
#    ;;
#    *)
#       SUB=ansi
#    ;;
# esac

LIB=$(readlink -f ../alire/build/lib-$SYS)

cd py/src
export LD_LIBRARY_PATH=$LIB:$LD_LIBRARY_PATH && python3 main.py $LIB

# EOF
