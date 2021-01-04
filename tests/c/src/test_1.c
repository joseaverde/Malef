#include "malef.h"
#include <stdio.h>
#include "utils.h"

int main() {

   malef_row_t height;
   malef_row_t width;

   malef_initialize();
   malef_newPage();
   malef_setTitle ("test_1.c: Testing Malef!");
   if (malef_getHeight (&height) != malef_NO_ERROR) return 1;
   if (malef_getWidth (&width) != malef_NO_ERROR) return 2;
   // This part won't be executed because everytime the terminal is resized a
   // signal is raised and catched by a handler which changes the current size.

   bool is_updated;
   if (malef_updateTerminalSize(&is_updated) != malef_NO_ERROR) return 3;
   if (is_updated) {
      printf("The terminal has been resized!\n");
   }
   malef_finalize();

   printf("The final size was: height=%hd, width=%hd\n", height, width);

   return 0;
}
