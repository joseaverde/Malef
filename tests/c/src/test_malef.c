/*****************************************************************************\
 *                                                                           * 
 *                          T E S T _ M A L E F . C                          * 
 *                                                                           * 
 *                                 M A L E F                                 * 
 *                                                                           * 
 *                              C   S O U R C E                              * 
 *                                                                           * 
 *---------------------------------------------------------------------------* 
 *     Copyright (c) 2021 José Antonio Verde Jiménez All Rights Reserved     * 
 *---------------------------------------------------------------------------* 
 * This file is part of Malef.                                               * 
 *                                                                           * 
 * This program is free software:  you  can redistribute it and/or modify it * 
 * under  the terms  of the  GNU  General License  as published by the  Free * 
 * Software  Foundation,  either  version 3  of  the  License,  or  (at your * 
 * opinion) any later version.                                               * 
 *                                                                           * 
 * This  program  is distributed  in the  hope that  it will be  useful, but * 
 * WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of * 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General * 
 * Public License for more details.                                          * 
 *                                                                           * 
 * You should have received  a copy of the  GNU General Public License along * 
 * with this program. If not, see <https://www.gnu.org/licenses/>.           * 
 *                                                                           * 
\*****************************************************************************/

#include "test_malef.h"
#include "tests.h"

#include "Malef.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


static char* 
test_malef_initialize ( void ) {

   malef_error_t err = malef_initialize () ;

   return tests_newString ( err ? "<error>" : "<>" ) ;
}


static char*
test_malef_finalize ( void ) {

   malef_error_t err = malef_finalize () ;

   return tests_newString ( err ? "<error>" : "<>" ) ;
}


static char*
test_malef_isInitialized ( void ) {

   return tests_newString ( malef_isInitialized () ? "True" : "False" ) ;
}


static char*
test_malef_getHeight ( void ) {

   bool status ;
   if ( malef_isInitialized () ) {
      tests_wait ( "Set the terminal height to 20, and press any key to "
                   "continue..." ) ;
      malef_updateTerminalSize ( &status ) ;
      if ( status ) {
         return tests_newString ( "$No automatic update" ) ;
      }
   }

   malef_row_t height ;
   malef_error_t err = malef_getHeight ( &height ) ;

   if ( err ) {
      return tests_newString ( "<error>" ) ;
   }

   char num[256] ;
   sprintf ( num, "%d", height ) ;
   char* str = (char*)malloc ( ( 1 + strlen(num) ) * sizeof(char) ) ;
   strcpy ( str, "@" ) ;
   strcat ( str, num ) ;

   return str ;
}


static char*
test_malef_getWidth ( void ) {

   bool status ;
   if ( malef_isInitialized () ) {
      tests_wait ( "Set terminal width to 50, and press any key to continue"
                   "..." ) ;
      malef_updateTerminalSize ( &status ) ;
      if ( status ) {
         return tests_newString ( "$No automatic update" ) ;
      }
   }

   malef_col_t width ;
   malef_error_t err = malef_getWidth ( &width ) ;

   if ( err ) {
      return tests_newString ( "<error>" ) ;
   }

   char num[256] ;
   sprintf ( num, "%d", width ) ;
   char* str = (char*)malloc ( ( 1 + strlen(num) ) * sizeof (char) ) ;
   strcpy ( str, "@" ) ;
   strcat ( str, num ) ;

   return str ;
}


static char*
test_malef_newPage ( void ) {

   malef_error_t err = malef_newPage () ;

   return tests_newString ( err ? "<error>" : "<>" ) ;
}


static char*
test_malef_setTitle ( void ) {

   malef_error_t err = malef_setTitle ( "Testing Malef from C !" ) ;

   return tests_newString ( err ? "<error>" : "<>" ) ;
}



void
test_malef_main ( void ) {
   tests_test ( test_malef_initialize, "malef_initialize",
                "<>", true ) ;
   tests_test ( test_malef_initialize, "malef_initialize",
                "MALEF.EXCEPTIONS.INITIALIZATION_ERROR", true ) ;
   tests_test ( test_malef_finalize, "malef_finalize",
                "<>", true ) ;
   tests_test ( test_malef_finalize, "malef_finalize",
                "MALEF.EXCEPTIONS.INITIALIZATION_ERROR", true ) ;
   tests_test ( test_malef_getHeight, "malef_getHeight",
                "MALEF.EXCEPTIONS.INITIALIZATION_ERROR", true ) ;
   tests_test ( test_malef_getWidth, "malef_getWidth",
                "MALEF.EXCEPTIONS.INITIALIZATION_ERROR", true ) ;
   tests_test ( test_malef_newPage, "malef_newPage",
                "MALEF.EXCEPTIONS.INITIALIZATION_ERROR", true ) ;
   tests_test ( test_malef_setTitle, "malef_setTitle",
                "MALEF.EXCEPTIONS.INITIALIZATION_ERROR", true ) ;
   // TODO: Check if title changed?
   // TODO: Prepend error to tests that will output an error.

   malef_initialize () ;

   tests_test ( test_malef_getHeight, "malef_getHeight",
                "@20", false ) ;
   tests_test ( test_malef_getWidth, "malef_getWidth",
                "@50", false ) ;
   tests_test ( test_malef_newPage, "malef_newPage",
                "<>", true ) ;
   tests_test ( test_malef_setTitle, "malef_setTitle",
                "<>", true ) ;
}

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
