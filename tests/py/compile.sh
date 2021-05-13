#!/bin/bash

# The implementation of Python3 I'm using is interpreted, thus, I copy bash
# file to run the tests.

cp src/run.sh "../bin-$1/tests-py"
chmod +x "../bin-$1/tests-py"

# EOF
