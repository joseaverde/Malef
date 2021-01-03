#include "malef.h"
#include <stdio.h>
#include "utils.h"

int main() {

   malef_row_t height;
   malef_col_t width;

   malef_initialize();
   malef_newPage();
   malef_setTitle ("test_1.c: Testing Malef!");
   height = malef_getHeight();
   width  = malef_getWidth();
   // This part won't be executed because everytime the terminal is resized a
   // signal is raised and catched by a handler which changes the current size.
   if (malef_updateTerminalSize()) {
      printf("The terminal has been resized!\n");
   }
   malef_finalize();

   printf("The final size was: height=%hd, width=%hd\n", height, width);

   return 0;
}
