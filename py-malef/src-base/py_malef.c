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

#include "malef.h"
#include "py_malef-exceptions.h"
#include "py_malef-utils.h"

#ifndef def
#define def static PyObject *
#endif


PyDoc_STRVAR (pyMalef_initialize_doc,
              "This function intializes Malef.");
def pyMalef_initialize (PyObject *self, PyObject *args) {

   malef_error_t err = malef_initialize ();

   if (_pyMalef_raiseException(err)) {
      return NULL;
   } else {
      Py_RETURN_NONE;
   }
}


PyDoc_STRVAR (pyMalef_finalize_doc,
              "This function finalizes Malef.");
def pyMalef_finalize (PyObject *self, PyObject *args) {

   malef_error_t err = malef_finalize ();

   if (_pyMalef_raiseException(err)) {
      return NULL;
   } else {
      Py_RETURN_NONE;
   }
}


PyDoc_STRVAR (pyMalef_isInitialized_doc,
              "This functions tells whether the Malef library has been " \
              "initialized.");
def pyMalef_isInitialized (PyObject *self, PyObject *args) {
   
   // This function will never raise an error.
   if (malef_isInitialized ()) {
      Py_RETURN_TRUE;
   } else {
      Py_RETURN_FALSE;
   }
}


PyDoc_STRVAR (pyMalef_getHeight_doc,
              "This function returns the height of the screen.");
def pyMalef_getHeight (PyObject *self, PyObject *args) {

   malef_row_t height;
   malef_error_t err = malef_getHeight (&height);

   if (_pyMalef_raiseException(err)) {
      return NULL;
   } else {
      return PyLong_FromUnsignedLong (height);
   }
}


PyDoc_STRVAR (pyMalef_getWidth_doc,
              "This function returns the width of the screen.");
def pyMalef_getWidth (PyObject *self, PyObject *args) {

   malef_col_t width;
   malef_error_t err = malef_getWidth (&width);

   if (_pyMalef_raiseException(err)) {
      return NULL;
   } else {
      return PyLong_FromUnsignedLong (width);
   }
}


PyDoc_STRVAR (pyMalef_newPage_doc,
              "This function moves the terminal down.");
def pyMalef_newPage (PyObject *self, PyObject *args) {

   malef_error_t err = malef_newPage ();

   if (_pyMalef_raiseException(err)) {
      return NULL;
   } else {
      Py_RETURN_NONE;
   }
}


PyDoc_STRVAR (pyMalef_setTitle_doc,
              "This function sets the terminal title.");
def pyMalef_setTitle (PyObject *self, PyObject *args) {

   const char *title_name;

   // We parse the arguments to get a string.
   if (!PyArg_ParseTuple(args, "s", &title_name)) {
         // We raise an exception if it couldn't parse the string from the
         // tuple.
         return NULL;
   }

   malef_error_t err = malef_setTitle (title_name);

   if (_pyMalef_raiseException(err)) {
      return NULL;
   } else {
      Py_RETURN_NONE;
   }
}


PyDoc_STRVAR (pyMalef_updateTerminalSize_doc,
              "This function updates the terminal size if it hasn't been " \
              "initialized.");
def pyMalef_updateTerminalSize (PyObject *self, PyObject *args) {

   bool is_updated;
   malef_error_t err = malef_updateTerminalSize (&is_updated);

   if (_pyMalef_raiseException(err)) {
      return NULL;
   } else {
      if (is_updated) {
         Py_RETURN_TRUE;
      } else {
         Py_RETURN_FALSE;
      }
   }
}



PyDoc_STRVAR (pyMalef_wrapper_doc,
              "This function wraps another function and executes it, in " \
              "any exception or unhandled errors and restores the terminal");
def pyMalef_wrapper (PyObject *self, PyObject *args) {
   // Due to the types of the python objects and many other problems, this
   // function will NOT call the malef_wrapper function. It will be completely
   // written in C with Python types.
   // However we will try to avoid Python calls as much as possible is faster
   // to call C functions from the Malef API.

   PyObject *function_to_wrap;
   PyObject *arguments_to_wrap;
   PyObject *_temp;
   PyObject *return_value;

   if (!PyArg_ParseTuple(args, "O|O", &function_to_wrap, &_temp)) {
      return NULL;
   }

   arguments_to_wrap = PyTuple_GetSlice(args, 1, PyObject_Length(args));

   if (pyMalef_initialize (NULL, _pyMalef_sharedEmptyTuple) == NULL) {
      // Initialization_Error must have been raised by this function.
      return NULL;
   }

   // We call the function with its arguments.
   if (arguments_to_wrap == NULL) {
      return_value = PyObject_Call (function_to_wrap,
                                    _pyMalef_sharedEmptyTuple,
                                    NULL);
   } else {
      return_value = PyObject_Call (function_to_wrap, arguments_to_wrap, NULL);
   }

   // We try to finalize the library.
   if (pyMalef_finalize(NULL, _pyMalef_sharedEmptyTuple) == NULL) {
      if (return_value == NULL) {
         // This means there has been a critical error in the library, we raise
         // an new exception: TODO.
         return NULL;
      } else {
         // The initialization exxception must have raised in the other
         // function.
         return NULL;
      }
   }

   // Finally we check the return_value.
   // TODO: Increase reference
   return return_value;
}



// "METHOD TABLE"
static PyMethodDef pyMalefMethods[] = {
   {"initialize",
    pyMalef_initialize,
    METH_VARARGS,
    pyMalef_initialize_doc},

   {"finalize",
    pyMalef_finalize,
    METH_VARARGS,
    pyMalef_finalize_doc},

   {"is_initialized",
    pyMalef_isInitialized,
    METH_VARARGS,
    pyMalef_isInitialized_doc},

   {"get_height",
    pyMalef_getHeight,
    METH_VARARGS,
    pyMalef_getHeight_doc},

   {"get_width",
    pyMalef_getWidth,
    METH_VARARGS,
    pyMalef_getWidth_doc},

   {"new_page",
    pyMalef_newPage,
    METH_VARARGS,
    pyMalef_newPage_doc},

   {"set_title",
    pyMalef_setTitle,
    METH_VARARGS,
    pyMalef_setTitle_doc},

   {"update_terminal_size",
    pyMalef_updateTerminalSize,
    METH_VARARGS,
    pyMalef_updateTerminalSize_doc},

   {"wrapper",
    pyMalef_wrapper,
    METH_VARARGS,
    pyMalef_wrapper_doc},
   
   {NULL, NULL, 0, NULL}
};


// "MODULE DOCUMENTATION"
PyDoc_STRVAR (pyMalefDocs, "Malef");


// "MODULE DEFINITION"
static struct PyModuleDef pyMalefModule = {
   PyModuleDef_HEAD_INIT,
   "malef",          // Name of the module.
   pyMalefDocs,      // Module documentation.
   -1,               // Size of per-interpreter state of the module.
                     // -1 because it keeps state in global variables.
                     // That way Malef is shared by all the modules that
                     // import it.
   pyMalefMethods,   // The "METHOD TABLE".
};



PyMODINIT_FUNC PyInit_malef (void) {

   PyObject *module = PyModule_Create (&pyMalefModule);
   if (module == NULL) {
      return NULL;
   }

   _pyMalef_initializeUtils ();
   if (!_pyMalef_initializeExceptions (module)) {
      return NULL;
   }

   return module;

}


///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
