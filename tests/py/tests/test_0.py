import utils
import malef
import time
from sys import exit


malef.initialize ()
try:
    malef.initialize ()
except malef.InitializationError:
    print("An InitializationError has been raised!")
    malef.finalize()
except:
    print("An unknown exception has been raised!")
    exit(1)
else:
    print("No exception has been raised, there might be a problem with",
          "the initialization library!")
    exit(2)
