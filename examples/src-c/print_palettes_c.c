#include <Malef.h>
#include <stdio.h>
#include <stdlib.h>

void
print_error ( void ) {
   char *err_name, *err_msg ;
   err_name = malef_getErrorName () ;
   err_msg  = malef_getErrorMessage () ;
   fprintf ( stderr, "%s: %s", err_name, err_msg ) ;
   free ( err_name ) ;
   free ( err_msg ) ;
}

#define EXIT_ON_ERROR(err) if ( err ) { print_error () ; return err ; }

int main () {

   malef_declareSurface ( surface ) ;
   malef_declareSurface ( nullSurface ) ;
   malef_col_t col ;
   malef_error_t err ;
   malef_color_t _color ;
   char ignore = 0;

   err = malef_createSurface ( 2, 16, surface ) ;
   EXIT_ON_ERROR(err)

   err = malef_initialize () ;
   EXIT_ON_ERROR(err) ;

   err = malef_newPage () ;
   EXIT_ON_ERROR(err) ;

   for ( malef_paletteKind_t pal = malef_MALEF_PALETTE ;
         pal <= malef_UBUNTU ;
         pal++ ) {
      malef_setPaletteKind ( pal ) ;
      col = 1 ;
      for ( int bright = 0 ; bright <= 1 ; bright++ ) {
         for ( malef_colorKind_t color = malef_BLACK ;
               color <= malef_WHITE ;
               color++ ) {
            malef_getColor ( &_color, color, bright ) ;
            malef_setSurfaceForeground ( surface, 1, 1, col, col, _color ) ;
            malef_setSurfaceBackground ( surface, 2, 2, col, col, _color ) ;
            col++ ;
         }
      }
      malef_debugPutSurface ( surface ) ;
      malef_debugPutSurface ( nullSurface ) ;
      while ( ignore != '\n' && ignore != '\r' ) {
         ignore = getchar () ;
      }
      ignore = 0 ;
   }

   err = malef_finalize () ;
   EXIT_ON_ERROR (err) ;

   return 0 ;
}
