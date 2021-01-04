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

#include "malef.h"
#include "py_malef-utils.h"
#include <strlib.h>

static PyObject *pyMalef_LibraryError;
static PyObject *pyMalef_InitializationError;
static PyObject *pyMalef_BoundsError;
static PyObject *pyMalef_NullSurfaceError;

#define _pyMalef_ERROR_COUNT 4

static inline PyObject* _pyMalef_err2exception (malef_error_t err) {

   switch (err) {
      case malef_NO_ERROR:
         return NULL;
         break;
      case malef_INITIALIZATION_ERROR:
         return pyMalef_InitializationError;
         break;
      case malef_BOUNDS_ERROR:
         return pyMalef_BoundsError;
         break;
      case malef_NULL_SURFACE_ERROR:
         return pyMalef_NullSurfaceError;
         break;
      case malef_ADA_ERROR:
      default:
         return pyMalef_LibraryError;
         break;
   }
}


static bool _pyMalef_raiseException (malef_error_t err) {

   PyObject *exception = _pyMalef_err2exception (err);
   char* name;
   char* message;
   char* long_message;

   if (exception == NULL) {
      return false;
   } else {
      if (exception == pyMalef_LibraryError) {
         name = malef_getErrorName ();
         message = malef_getErrorMessage ();
         long_message = (char*)malloc ((strlen(name) + strlen(message) + 8) *
                                       sizeof(char));
         strcpy (long_message, name);
         strcat (long_message, ": ");
         strcat (long_message, message);
         PyErr_SetString (exception, long_message);
         free (name);
         free (message);
         free (long_message);
      } else {
         message = malef_getErrorMessage ();
         PyErr_SetString (exception, message);
         free (message);
      }
      malef_catchError ();

      return true;
   }

   return false; // TODO
}


static inline bool _pyMalef_addException (PyObject*   module,
                                          PyObject*   object,
                                          const char* name) {
   // We keep track of the referenced exceptions so far, so we can unreference
   // them if anything goes wrong.
   static PyObject * _pyMalef_referencedExceptions[_pyMalef_ERROR_COUNT] =
                     {NULL};
   static int _pyMalef_numberReferencedExceptions = 0;

   _pyMalef_referencedExceptions[_pyMalef_numberReferencedExceptions] = object;
   _pyMalef_numberReferencedExceptions++;
   if (PyModule_AddObject (module, name, object) < 0) {
      for (int exception = 0;
               exception < _pyMalef_numberReferencedExceptions;
               exception++) {
         Py_XDECREF (_pyMalef_referencedExceptions[exception]);
         Py_CLEAR   (_pyMalef_referencedExceptions[exception]);
         _pyMalef_referencedExceptions[exception] = NULL;
      }
      Py_DECREF (module);
      _pyMalef_numberReferencedExceptions = 0;
      _pyMalef_finalizeUtils ();
      return false;
   }

   return true;
}


static inline bool _pyMalef_initializeExceptions (PyObject* module) {

   pyMalef_LibraryError = PyErr_NewException("malef.LibraryError", NULL, NULL);
   if (!_pyMalef_addException(module, pyMalef_LibraryError, "LibraryError"))
      return false;

   pyMalef_InitializationError = PyErr_NewException
                                    ("malef.InitializationError", NULL, NULL);
   if (!_pyMalef_addException(module,
                              pyMalef_InitializationError,
                              "InitializationError")) return false;

   pyMalef_BoundsError = PyErr_NewException ("malef.BoundsError", NULL, NULL);
   if (!_pyMalef_addException(module, pyMalef_BoundsError, "BoundsError"))
      return false;

   pyMalef_NullSurfaceError = PyErr_NewException
                                    ("malef.NullSurfaceError", NULL, NULL);
   if (!_pyMalef_addException(module,
                              pyMalef_NullSurfaceError,
                              "NullSurfaceError")) return false;

   return true;

}



#endif//PY_MALEF_EXCEPTIONS_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
