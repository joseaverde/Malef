#include "malef.h"
#include <stdio.h>
#include "utils.h"

int main (void) {

   malef_declareSurface (surface);
   malef_declareSurface (colours);
   malef_palette_t palette;
   malef_col_t     col;

   malef_getPaletteKind (malef_UBUNTU, palette);
   malef_createSurface (10, 10, surface);

   malef_setSurfaceForeground (surface,  1, 10,  1, 10,
                               palette[true][malef_GREEN]);
   malef_setSurfaceBackground (surface,  1, 10,  1, 10,
                               palette[false][malef_GREEN]);
   malef_setSurfaceForeground (surface,  2,  5,  3,  7,
                               palette[true ][malef_BLACK]);
   malef_setSurfaceBackground (surface,  1,  4,  2,  6,
                               palette[false][malef_WHITE]);
   malef_setSurfaceForeground (surface, 10, 10, 10, 10,
                               palette[true ][malef_RED]);
   malef_setSurfaceBackground (surface, 10, 10, 10, 10,
                               palette[true ][malef_BLUE]);

   malef_createSurface (2, 16, colours);
   col = 1;
   for (int brightness =  false;
            brightness <= true;
            brightness++) {
      for (malef_colorKind_t colour =  malef_BLACK;
                             colour <= malef_WHITE;
                             colour++) {
         malef_setSurfaceForeground(colours, 1, 1, col, col,
                                    palette[brightness][colour]);
         col++;
      }
   }
   
   col = 1;
   for (int brightness =  false;
            brightness <= true;
            brightness++) {
      for (malef_colorKind_t colour =  malef_BLACK;
                             colour <= malef_WHITE;
                             colour++) {
         malef_setSurfaceBackground(colours, 2, 2, col, col,
                                    palette[brightness][colour]);
         col++;
      }
   }

   malef_initialize ();
   malef_newPage ();
   _malef_debugPutSurface (surface);
   delay (2000);
   _malef_debugPutSurface (colours);
   malef_newPage ();
   malef_finalize ();

   return 0;
}
