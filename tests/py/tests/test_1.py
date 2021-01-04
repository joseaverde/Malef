import time
import utils
import malef

malef.initialize()
malef.set_title("TESTING in PYTHON3")
print("height = %d" % malef.get_height())
print("width  = %d" % malef.get_width())
time.sleep(2)
malef.finalize()
