/*****************************************************************************\
 *                                                                           * 
 *                 P Y _ M A L E F - E X C E P T I O N S . H                 * 
 *                                                                           * 
 *                                 M A L E F                                 * 
 *                                                                           * 
 *                              C   H E A D E R                              * 
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

#ifndef PY_MALEF_EXCEPTIONS_H
#define PY_MALEF_EXCEPTIONS_H

#include "Malef.h"
#include "py_malef-utils.h"
#include <string.h>



/*###########################################################################*\
 *##########################  E X C E P T I O N S  ##########################*
\*###########################################################################*/

// This is the number of exceptions there are.
#define _pyMalef_ERROR_COUNT  4

/*
 * This exception is raised when there is a fatal error or an unknown error in
 * the library.
 */
static PyObject *pyMalef_LibraryError ;

/*
 * This exception is raised when trying to access uninitialized functions or
 * variables. Also when trying to initialize an initialized library or
 * finalize an uninitialized library. It can also be raised if there is a
 * problem during initialization.
 */
static PyObject *pyMalef_InitializationError ;

/*
 * This exception is raised when trying to access any position out of bounds.
 */
static PyObject *pyMalef_BoundsError ;

/*
 * This exception is raised when trying to modify a null surface.
 */
static PyObject *pyMalef_NullSurfaceError ;



/*###########################################################################*\
 *###########################  F U N C T I O N S  ###########################*
\*###########################################################################*/


/*
 * This function gets the C error exit status and returns the python exception
 * equivalent to it.
 *
 * @param err
 * The C error returned by a function.
 */
static inline PyObject*
_pyMalef_err2exception ( malef_error_t err ) {

   switch ( err ) {
      case malef_NO_ERROR:
         return NULL ;                        break ;
      case malef_INITIALIZATION_ERROR:
         return pyMalef_InitializationError ; break ;
      case malef_BOUNDS_ERROR:
         return pyMalef_BoundsError ;         break ;
      case malef_NULL_SURFACE_ERROR:
         return pyMalef_NullSurfaceError ;    break ;
      case malef_ADA_ERROR:
      default:
         return pyMalef_LibraryError ;        break ;
   }
}


/*
 * This function is used to raise a Python exception using a C error type
 * parameter.
 *
 * @param err
 * The C error returned by a function from the C API.
 */
static bool
_pyMalef_raiseException ( malef_error_t err ) {

   PyObject *exception = _pyMalef_err2exception ( err ) ;
   char *name ;
   char *message ;
   char *long_message ;

   if ( exception == NULL ) {
      // It means no exception has been raised.
      return false ;
   } else {
      if ( exception == pyMalef_LibraryError ) {
         // If it's a library error, we make sure to put all the information
         // retrieved from the error in a longer message with the ada exception
         // name.
         name = malef_getErrorName () ;
         message = malef_getErrorMessage () ;
         long_message = (char*)malloc (
                              ( strlen ( name ) + strlen ( message ) + 8 ) *
                              sizeof(char) ) ;

         strcpy ( long_message, name ) ;
         strcat ( long_message, ": " ) ;
         strcat ( long_message, message ) ;

         PyErr_SetString ( exception, message ) ;

         free ( name ) ;
         free ( message ) ;
         free ( long_message ) ;
      } else {
         // Otherwise, we only copy the error message.
         message = malef_getErrorMessage () ;
         PyErr_SetString ( exception, message ) ;
         free ( message ) ;
      }
      // Finally we `catch' the error from C and return there has been an
      // error.
      malef_catchError () ;

      return true ;
   }
}



/*
 * This function is used to add an exception to the module, so we can make
 * sure that everything is finalized if an error happens.
 *
 * @param module
 * The Python module object.
 *
 * @param exception
 * The exception to add to the module.
 *
 * @param name
 * The name of the exception to add to the module.
 */
static bool
_pyMalef_addException ( PyObject*   module,
                        PyObject*   exception,
                        const char* name ) {
   // We keep track of the added exceptions so far, so we can unreference them
   // in case anything goes wrong.
   static PyObject *referenced_exceptions[_pyMalef_ERROR_COUNT] = {NULL} ;
   static int referenced_exceptions_number = 0 ;

   // We add the new exception and increase the number of exceptions.
   referenced_exceptions[referenced_exceptions_number++] = exception ;

   if ( PyModule_AddObject ( module, name, exception ) < 0 ) {
      // If an error occurs during the implementation of the exception into the
      // module, we have to unreference and clear them to free memory.
      for ( int e = 0; e < referenced_exceptions_number; e++ ) {
         Py_XDECREF ( referenced_exceptions[e] ) ;
         Py_CLEAR ( referenced_exceptions[e] ) ;
         referenced_exceptions[e] = NULL ;
      }
      // We also restore the original values of the static functions to allow
      // the module to be imported again if anything goes wrong *AGAIN*.
      referenced_exceptions_number = 0 ;
      // We finally finalize the utils and return false.
      _pyMalef_finalizeUtils () ;
      return false ;
   }

   return true ;
}


/*
 * This function initializes the exceptions into the module.
 *
 * @param module
 * The module where the exceptions will be added.
 */
static inline bool
_pyMalef_initializeExceptions ( PyObject *module ) {

   // LibraryError //
   pyMalef_LibraryError = PyErr_NewException( "malef.LibraryError",
                                              NULL, NULL ) ;
   if ( ! _pyMalef_addException ( module, pyMalef_LibraryError,
                                  "LibraryError" ) ) return false ;

   // InitializationError //
   pyMalef_InitializationError = PyErr_NewException (
                                                "malef.InitializationError",
                                                NULL, NULL ) ;
   if ( ! _pyMalef_addException ( module, pyMalef_InitializationError,
                                  "InitializationError" ) ) return false ;

   // BoundsError //
   pyMalef_BoundsError = PyErr_NewException ( "malef.BoundsError",
                                              NULL, NULL ) ;
   if ( ! _pyMalef_addException ( module, pyMalef_BoundsError,
                                  "BoundsError" ) ) return false ;

   // NullSurfaceError //
   pyMalef_NullSurfaceError = PyErr_NewException ( "malef.NullSurfaceError",
                                                   NULL, NULL ) ;
   if ( ! _pyMalef_addException ( module, pyMalef_NullSurfaceError,
                                  "NullSurfaceError" ) ) return false ;

   return true ;
}


#endif//PY_MALEF_EXCEPTIONS_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
