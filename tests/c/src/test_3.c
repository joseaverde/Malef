#include "malef.h"

int main (void) {

   malef_declareSurface (mySurface);
   mySurface = malef_createSurface (10, 10);

   malef_initialize ();
   malef_newPage ();
   _malef_debugPutSurface (mySurface);
   malef_finalize ();

   malef_destroySurface (mySurface);

}
