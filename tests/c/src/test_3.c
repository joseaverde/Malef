#include "malef.h"

int main (void) {

   malef_surface_t mySurface = malef_createSurface (10, 10);

   malef_initialize ();
   malef_newPage ();
   _malef_debugPutSurface (mySurface);
   malef_finalize ();

   malef_destroySurface (mySurface);

}
