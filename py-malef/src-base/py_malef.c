 /*****************************************************************************\
 *                                                                           * 
 *                            P Y _ M A L E F . C                            * 
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

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "Malef.h"

#include "py_malef-exceptions.h"
#include "py_malef-enum_iterators.h"
#include "py_malef-palettes.h"
#include "py_malef-palette_enums.h"
#include "py_malef-surfaces.h"
#include "py_malef-colors.h"
#include "py_malef-color_enums.h"



/*###########################################################################*\
 *###########################  F U N C T I O N S  ###########################*
\*###########################################################################*/


/* *** malef.initialize *** */
PyDoc_STRVAR ( pyMalef_initialize_doc,
"This function initializes the Malef library. It must be initialized in order"\
" to run certain IO functions or functions that requiere it to be initialized"\
". In case it wasn't initialized, an error would be raised." ) ;
static PyObject*
pyMalef_initialize ( PyObject *self, PyObject *args ) {

   malef_error_t err = malef_initialize () ;

   if ( _pyMalef_raiseException (err) ) {
      return NULL ;
   } else {
      Py_RETURN_NONE ;
   }
}
#define pyMalef_initialize_method {                                           \
   "initialize",                                                              \
   pyMalef_initialize,                                                        \
   METH_VARARGS,                                                              \
   pyMalef_initialize_doc                                                     \
}


/* *** malef.finalize *** */
PyDoc_STRVAR ( pyMalef_finalize_doc,
"This function finalizes and restores the terminal. It raises an exception in"\
" case the library wasn't already initialized or if there was a fatal error " \
"in the library." ) ;
static PyObject*
pyMalef_finalize ( PyObject *self, PyObject *args ) {

   malef_error_t err = malef_finalize () ;

   if ( _pyMalef_raiseException (err) ) {
      return NULL ;
   } else {
      Py_RETURN_NONE ;
   }
}
#define pyMalef_finalize_method {                                             \
   "finalize",                                                                \
   pyMalef_finalize,                                                          \
   METH_VARARGS,                                                              \
   pyMalef_finalize_doc                                                       \
}


/* *** malef.isInitialized *** */
PyDoc_STRVAR ( pyMalef_isInitialized_doc,
"This function returns whether the Malef library has been initialized." ) ;
static PyObject*
pyMalef_isInitialized ( PyObject *self, PyObject *args ) {

   // This function will never raise an error.
   if ( malef_isInitialized () ) {
      Py_RETURN_TRUE ;
   } else {
      Py_RETURN_FALSE ;
   }
}
#define pyMalef_isInitialized_method {                                        \
   "isInitialized",                                                           \
   pyMalef_isInitialized,                                                     \
   METH_VARARGS,                                                              \
   pyMalef_isInitialized_doc                                                  \
}


/* *** malef.getHeight *** */
PyDoc_STRVAR ( pyMalef_getHeight_doc,
"This function returns the height of the terminal/console. It must have been "\
"initialized (the library) in order for it to work." ) ;
static PyObject*
pyMalef_getHeight ( PyObject *self, PyObject *args ) {

   malef_row_t height ;
   malef_error_t err = malef_getHeight ( &height ) ;

   if ( _pyMalef_raiseException (err) ) {
      return NULL;
   } else {
      return PyLong_FromUnsignedLong ( height ) ;
   }
}
#define pyMalef_getHeight_method {                                            \
   "getHeight",                                                               \
   pyMalef_getHeight,                                                         \
   METH_VARARGS,                                                              \
   pyMalef_getHeight_doc                                                      \
}


/* *** malef.getWidth *** */
PyDoc_STRVAR ( pyMalef_getWidth_doc,
"This function returns the width of the terminal/console. The library must "  \
"have been initialized in order for it to work." ) ;
static PyObject*
pyMalef_getWidth ( PyObject *self, PyObject *args ) {

   malef_col_t width ;
   malef_error_t err = malef_getWidth ( &width ) ;

   if ( _pyMalef_raiseException (err) ) {
      return NULL ;
   } else {
      return PyLong_FromUnsignedLong ( width ) ;
   }
}
#define pyMalef_getWidth_method {                                             \
   "getWidth",                                                                \
   pyMalef_getWidth,                                                          \
   METH_VARARGS,                                                              \
   pyMalef_getWidth_doc                                                       \
}


/* *** malef.newPage *** */
PyDoc_STRVAR ( pyMalef_newPage_doc,
"This function prepares a clean environment for the terminal application by " \
"moving the old text up. It raises an error if the library hasn't been "      \
"initialized." ) ;
static PyObject*
pyMalef_newPage ( PyObject *self, PyObject *args ) {

   malef_error_t err = malef_newPage () ;

   if ( _pyMalef_raiseException (err) ) {
      return NULL ;
   } else {
      Py_RETURN_NONE ;
   }
}
#define pyMalef_newPage_method {                                              \
   "newPage",                                                                 \
   pyMalef_newPage,                                                           \
   METH_VARARGS,                                                              \
   pyMalef_newPage_doc                                                        \
}


/* *** malef.setTitle *** */
PyDoc_STRVAR ( pyMalef_setTitle_doc,
"This function changes the title of the terminal/console. This new title will"\
" remain even if the library has been finalized. It will return when the "    \
"program is terminated. It raises an exception if the library hasn't been "   \
"initialized." ) ;
static PyObject*
pyMalef_setTitle ( PyObject *self, PyObject *args, PyObject *kwargs ) {

   static char *keyword_list[] = { "title", NULL } ;
   const char *title_name ;

   // We parse the arguments to get a string.
   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "s", keyword_list,
                                        &title_name ) ) {
      // We raise an exception if it couldn't parse the string from the tuple.
      return NULL ;
   }

   malef_error_t err = malef_setTitle ( title_name ) ;

   if ( _pyMalef_raiseException (err) ) {
      return NULL ;
   } else {
      Py_RETURN_NONE ;
   }
}
#define pyMalef_setTitle_method {                                             \
   "setTitle",                                                                \
   (PyCFunction)pyMalef_setTitle,                                             \
   METH_VARARGS | METH_KEYWORDS,                                              \
   pyMalef_setTitle_doc                                                       \
}


/* *** malef.updateTerminalSize *** */
PyDoc_STRVAR ( pyMalef_updateTerminalSize_doc,
"This function updates the terminal size and it returns whether the size has "\
"changed. However in systems like Linux, the size will be automatically "     \
"updated, so it will always return `False'." ) ;
static PyObject*
pyMalef_updateTerminalSize ( PyObject *self, PyObject *args ) {

   bool is_updated ;
   malef_error_t err = malef_updateTerminalSize ( &is_updated ) ;

   if ( _pyMalef_raiseException (err) ) {
      return NULL ;
   } else {
      if ( is_updated ) {
         Py_RETURN_TRUE ;
      } else {
         Py_RETURN_FALSE ;
      }
   }
}
#define pyMalef_updateTerminalSize_method {                                   \
   "updateTerminalSize",                                                      \
   pyMalef_updateTerminalSize,                                                \
   METH_VARARGS,                                                              \
   pyMalef_updateTerminalSize_doc                                             \
}


/* *** malef.wrapper *** */
PyDoc_STRVAR ( pyMalef_wrapper_doc,
"This function wraps another function to make sure everything is cleaned if " \
"any error was raised during the execution of the function. After clean up "  \
"the same errors will be raised again. You can pass one (or none) argument "  \
"to your wrapped function -- whose return value will be returned as well." ) ;
static PyObject*
pyMalef_wrapper ( PyObject *self, PyObject *args, PyObject *kwargs ) {
   // Due to the types of the Python objects and many other problems, this
   // function will NOT call the malef_wrapper function from the C API. It will
   // be completely written in C with Python types and function calls.
   // However I will try to avoid Python calls as much as possible throughout
   // this implementation, because C functions are way faster than Python ones.

   static char *keyword_list[] = { "func", "args", NULL } ;
   PyObject *function_to_wrap ;
   PyObject *arguments_to_wrap ;
   PyObject *return_value ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "O|O", keyword_list,
                                        &function_to_wrap,
                                        &arguments_to_wrap ) ) {
      return NULL ;
   }

   if ( pyMalef_initialize ( NULL, _pyMalef_sharedEmptyTuple) == NULL ) {
      // Initialization_Error must have been raised by this function.
      // It could have been faster to check whether it has been initialized,
      // but in that case we wouldn't be able to catch wrong initialization
      // exceptions.
      return NULL ;
   }

   // We call the function with its parameters.
   if ( arguments_to_wrap == NULL ) {
      arguments_to_wrap = _pyMalef_sharedEmptyTuple ;
   }
   return_value = PyObject_Call ( function_to_wrap, arguments_to_wrap, NULL ) ;

   // We try to finalize the library.
   if ( pyMalef_finalize ( NULL, _pyMalef_sharedEmptyTuple ) == NULL ) {
      if ( return_value == NULL ) {
         // This means there has been a fatal error in the library, we raise
         // a new exception.
         _pyMalef_raiseException ( malef_ADA_ERROR ) ;
         return NULL ;
      } else {
         // The initialization exception must have been raised in the other
         // function.
         return NULL ;
      }
   }

   // Finally we return the value.
   // NOTE: I'm not increasing the reference because they wrapped function is
   // supposed to increase it because it's a function declared maybe in Python.
   // This function acts as wrapper only, and Python can't perform any kind of
   // INCREF or DECREF inside a C function. So increasing it may mess up the
   // Python internal mechanism and not free the object once it's out of the
   // scope.
   return return_value ;
}
#define pyMalef_wrapper_method {                                              \
   "wrapper",                                                                 \
   (PyCFunction)pyMalef_wrapper,                                              \
   METH_VARARGS,                                                              \
   pyMalef_wrapper_doc                                                        \
}



/*###########################################################################*\
 *########################  M E T H O D   T A B L E  ########################*
\*###########################################################################*/

// We add all the declared methods to the table.
static PyMethodDef
pyMalefMethods[] = {
   /* MALEF */
   pyMalef_initialize_method,
   pyMalef_finalize_method,
   pyMalef_isInitialized_method,
   pyMalef_getHeight_method,
   pyMalef_getWidth_method,
   pyMalef_newPage_method,
   pyMalef_setTitle_method,
   pyMalef_updateTerminalSize_method,
   pyMalef_wrapper_method,

   /* PALETTES */
   pyMalef_getPalette_method,
   pyMalef_setPalette_method,

   {NULL, NULL, 0, NULL}
} ;



/*###########################################################################*\
 *#######################  D O C U M E N T A T I O N  #######################*
\*###########################################################################*/

PyDoc_STRVAR ( pyMalefDocs,
"This is the Python binding to `Malef', a library which was originally "
"written in Ada. This library is similar to the `curses' library, but it aims "
"to be cross-platform and cross-platform inside the same platform, which means"
" that it will be able to choose the best components during run-time. "
"It will also try to replicate most of the Styles and Colours that aren't "
"available in every Console/Terminal. And it will use 32-bits colours instead "
"of letting the user choose between 3-bits, 4-bits, 8-bits or 24-bits colours "
"which may not work in every platform. The last 8-bits are for transparency, "
"because this library adds `Surfaces' which are able to have transparency." ) ;



/*###########################################################################*\
 *##############################  M O D U L E  ##############################*
\*###########################################################################*/

static struct PyModuleDef
pyMalefModule = {
   PyModuleDef_HEAD_INIT,
   "malef",                // Name of the module.
   pyMalefDocs,            // Module documentation.
   -1,                     // Size of per-interpreter state of the module.
                           // -1 because it keeps state in global variables,
                           // that way Malef is shared among modules.
   pyMalefMethods          // The "METHOD TABLE"
} ;



/*###########################################################################*\
 *################################  I N I T  ################################*
\*###########################################################################*/


PyMODINIT_FUNC
PyInit_malef ( void ) {

   // If any error occurs during initialization we finalize everything
   // initialize so far and also unreference the module so the garbage
   // collector can free the memory when it feels like doing so.
   // TODO: Automate it!

   PyObject *module = PyModule_Create ( &pyMalefModule );
   if ( module == NULL ) {
      Py_DECREF ( module ) ;
      return NULL ;
   }

   if ( ! _pyMalef_initializeEnumIterators ( module ) ) {
      Py_DECREF ( module ) ;
   }

   if ( ! _pyMalef_initializeColors ( module ) ) {
      _pyMalef_finalizeEnumIterators ( module ) ;

      Py_DECREF ( module ) ;
      return NULL ;
   }

   if ( ! _pyMalef_initializeColorEnums ( module ) ) {
      _pyMalef_finalizeEnumIterators ( module ) ;
      _pyMalef_finalizeColors ( module );

      Py_DECREF (module) ;
      return NULL ;
   }

   if ( ! _pyMalef_initializePalettes ( module ) ) {
      _pyMalef_finalizeEnumIterators ( module ) ;
      _pyMalef_finalizeColors ( module ) ;
      _pyMalef_finalizeColorEnums ( module ) ;

      Py_DECREF ( module ) ;
      return NULL ;
   }

   if ( ! _pyMalef_initializePaletteEnums ( module ) ) {
      _pyMalef_finalizeEnumIterators ( module ) ;
      _pyMalef_finalizeColors ( module ) ;
      _pyMalef_finalizeColorEnums ( module ) ;
      _pyMalef_finalizePalettes ( module ) ;

      Py_DECREF ( module ) ;
   }

   if ( ! _pyMalef_initializeSurfaces ( module ) ) {
      _pyMalef_finalizeEnumIterators ( module ) ;
      _pyMalef_finalizeColors ( module ) ;
      _pyMalef_finalizeColorEnums ( module ) ;
      _pyMalef_finalizePalettes ( module ) ;
      _pyMalef_finalizePaletteEnums ( module ) ;

      Py_DECREF ( module ) ;
      return NULL ;
   }

   _pyMalef_initializeUtils () ;
   if ( ! _pyMalef_initializeExceptions ( module ) ) {
      _pyMalef_finalizeEnumIterators ( module ) ;
      _pyMalef_finalizeColors ( module ) ;
      _pyMalef_finalizeColorEnums ( module ) ;
      _pyMalef_finalizePalettes ( module ) ;
      _pyMalef_finalizeSurfaces ( module ) ;
      _pyMalef_finalizePaletteEnums ( module ) ;

      Py_DECREF ( module ) ;
      return NULL ;
   }

   return module ;
}


///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
