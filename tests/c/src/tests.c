/*****************************************************************************\
 *                                                                           * 
 *                               T E S T S . C                               * 
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


#include "tests.h"

#include "Malef.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

bool tests_avoid_delays = false ;

static inline void
eprint ( int n, ... ) {

   va_list args ;
   va_start ( args, n ) ;

   for ( int i = 0; i < n; i++ ) {
      fputs ( va_arg ( args, char* ),
              stderr ) ;
      if ( i < n - 1 ) {
         fputs ( "\t",
                 stderr ) ;
      } else {
         fputs ( "\n",
                 stderr ) ;
      }
   }

   va_end ( args ) ;
}



char*
tests_newString ( const char* old ) {
   int size = 0 ;
   while ( old[size] != '\0' ) size++ ;
   char *new = (char*)malloc ( ++size * sizeof(char) ) ;
   strcpy ( new, old ) ;
}


void
tests_start ( const char* pkg ) {

   if ( ! strcmp ( pkg, "<>" ) ) {
      eprint ( 2, "language", "c" ) ;
   } else {
      eprint ( 2, "package", pkg ) ;
   }
}

#ifdef CLOCK_PROCESS_CPUTIME_ID
#  define CLOCKTYPE  CLOCK_PROCESS_CPUTIME_ID
#else
#  define CLOCKTYPE  CLOCK_MONOTONIC
#endif

void
tests_test ( char *(*unit)(void),
             const char *name,
             const char *expected,
             bool        time_it ) {
   struct timespec start ;
   struct timespec finish ;

   if ( tests_avoid_delays && ! time_it ) {
      eprint ( 2, "skipped", name ) ;
      return ;
   }

   clock_gettime ( CLOCKTYPE, &start ) ;
   char* output = unit () ;
   clock_gettime ( CLOCKTYPE, &finish ) ;

   if ( ! strcmp ( output, "<error>" ) ) {
      free ( output ) ;
      if ( malef_isFlyingError () ) {
         output = malef_getErrorName () ;
      } else {
         eprint ( 2, "fatal", name ) ;
         return ;
      }
   }

   float elaps_s  = difftime ( finish.tv_sec, start.tv_sec ) ;
   long  elaps_ns = finish.tv_nsec - start.tv_nsec ;

   char str_time[30] ;
   sprintf ( str_time, "%f", elaps_s + ((float)elaps_ns) / 1.0e9 );
   if ( strcmp ( output, expected ) ) {
      eprint ( 4, "failed", name, expected, output ) ;
   } else {
      eprint ( 3, "passed", name, time_it? str_time : "<null>" ) ;
   }
   free ( output ) ;
}


void
tests_wait ( const char* text ) {
   puts ( text ) ;
   getchar () ;
   puts ( "\n" ) ;
}

void
wrap ( void (*proc)(void),
       const char* name ) {
   tests_start ( name ) ;
   proc () ;
}




///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
