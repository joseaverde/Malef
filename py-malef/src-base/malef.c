/*****************************************************************************\
 *                                                                           * 
 *                               M A L E F . C                               * 
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

#ifndef def
#define def static PyObject *
#endif


PyDoc_STRVAR (pyMalef_initialize_doc,
              "This function intializes Malef.");
def pyMalef_initialize (PyObject *self, PyObject *args) {

   malef_initialize ();
   // TODO: Check for errors
   Py_RETURN_NONE;
}


PyDoc_STRVAR (pyMalef_finalize_doc,
              "This function finalizes Malef.");
def pyMalef_finalize (PyObject *self, PyObject *args) {

   malef_finalize ();
   // TODO: Check for errors
   Py_RETURN_NONE;

}


PyDoc_STRVAR (pyMalef_isInitialized_doc,
              "This functions tells whether the Malef library has been " \
              "initialized.");
def pyMalef_isInitialized (PyObject *self, PyObject *args) {
   
   if (malef_isInitialized ()) {
      Py_RETURN_TRUE;
   } else {
      Py_RETURN_FALSE;
   }
}


PyDoc_STRVAR (pyMalef_getHeight_doc,
              "This function returns the height of the screen.");
def pyMalef_getHeight (PyObject *self, PyObject *args) {

   malef_row_t height = malef_getHeight ();
   // TODO: Check for errors

   return PyLong_FromUnsignedLong (height);

}


PyDoc_STRVAR (pyMalef_getWidth_doc,
              "This function returns the width of the screen.");
def pyMalef_getWidth (PyObject *self, PyObject *args) {

   malef_col_t width = malef_getWidth ();
   // TODO: Check for errors

   return PyLong_FromUnsignedLong (width);

}


PyDoc_STRVAR (pyMalef_newPage_doc,
              "This function moves the terminal down.");
def pyMalef_newPage (PyObject *self, PyObject *args) {

   malef_newPage ();
   // TODO: Check for errors;
   Py_RETURN_NONE;

}


PyDoc_STRVAR (pyMalef_setTitle_doc,
              "This function sets the terminal title.");
def pyMalef_setTitle (PyObject *self, PyObject *args) {

   // TODO: Check for errors and actually do it.
   Py_RETURN_NONE;

}


PyDoc_STRVAR (pyMalef_updateTerminalSize_doc,
              "This function updates the terminal size if it hasn't been " \
              "initialized.");
def pyMalef_updateTerminalSize (PyObject *self, PyObject *args) {

   if (malef_updateTerminalSize ()) {
      Py_RETURN_TRUE;
   } else {
      Py_RETURN_FALSE;
   }

}


PyDoc_STRVAR (pyMalef_wrapper_doc,
              "This function wraps another function and executes it, in " \
              "any exception or unhandled errors and restores the terminal");
def pyMalef_wrapper (PyObject *self, PyObject *args) {

   // TODO: Check for errors and actually do it.
   Py_RETURN_NONE;

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

   return PyModule_Create (&pyMalefModule);

}


///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
