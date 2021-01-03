#!/bin/bash

cd build/lib*
export LD_LIBRARY_PATH=/home/jose/Development/Malef/alire/build/lib-linux && \
   echo $LD_LIBRARY_PATH && python3

