#!/bin/bash

# We create the missing directories.
# This is not technically required because `gprbuild' will create them
# automatically but I prefer to make them in advance.
if [ ! -d obj/ ]; then
   mkdir obj
fi

# We run `gprbuild'
gprbuild -p -Ptests

# EOF
