#include "malef.h"
#include <stdio.h>

int main (void) {

   printf(" * Declaring...\n");
   malef_declareSurface (mySurface);
   printf(" * Declared!\n\n");

   printf(" * Creating...\n");
   malef_createSurface (10, 10, mySurface);
   printf(" * Created!\n\n");

   malef_initialize ();
   malef_newPage ();
   _malef_debugPutSurface (mySurface);
   malef_finalize ();

   printf(" * Destroying...\n");
   malef_destroySurface (mySurface);
   printf(" * Destroyed!\n\n");

}
