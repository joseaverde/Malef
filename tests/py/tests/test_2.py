#!/usr/bin/env python3

import utils
import malef


def no_args_function ():
    print("This function has no args.")

def args_function(args):
    print("This function has args which are:", args)


malef.wrapper(no_args_function)
malef.wrapper(args_function, ("hola", 1, 2, 3))

