/*****************************************************************************\
 *                                                                           * 
 *                     P Y _ M A L E F - C O L O R S . H                     * 
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

#ifndef MALEF_COLORS_H
#define MALEF_COLORS_H

#include <Python.h>
#include <stdlib.h>
#include "Malef.h"

#include "py_malef-exceptions.h"

/*###########################################################################*\
 *####### C O L O R - W R A P P E R   T Y P E   D E C L A R A T I O N #######*
\*###########################################################################*/

#define PYMALEF_COLOR_LENGTH  4

/*
 * This is the wrapper that wraps the C colour type so it can be accessed as
 * a class for the Python3 binding. It's a an array like object, more like a
 * modify-able tuple that contains that contains three 8-bits-long integers
 * ranging from 0 to 255 to specify the Red, Green, Blue and Alpha components
 * from the colours.
 *
 * @field color
 * This is the C colour.
 */
typedef struct {
   PyObject_HEAD
   malef_color_t color ;
} _pyMalef_colorStruct ;



/*###########################################################################*\
 *###################### P R I V A T E   M E T H O D S ######################*
\*###########################################################################*/

/*
 * __len__
 *
 * This function returns the length of the colours which is always 4, but we
 * provide it for any function that uses the `len' method.
 *
 * @param self
 * The colour whose length we want to get.
 *
 * @return
 * It returns the length of the colours.
 */
static Py_ssize_t
pyMalef_Color___len__ ( _pyMalef_colorStruct *self ) {
   return PYMALEF_COLOR_LENGTH ;
}


/*
 * __getitem__
 *
 * This function is called when subscripting the Colours, however it has a
 * fixed size so:
 *  - [0] -> Red
 *  - [1] -> Green
 *  - [2] -> Blue
 *  - [3] -> Alpha
 *
 * @param self
 * The Colour
 *
 * @param index
 * The index.
 *
 * @exception BoundsError
 * This exception is raised when trying to access an item out of bounds.
 */
static PyObject*
pyMalef_Color___getitem__ ( _pyMalef_colorStruct *self,
                            Py_ssize_t            index ) {
   char message[512] ;
   if ( index < 0 || index >= PYMALEF_COLOR_LENGTH ) {
      sprintf ( message,
               "%d: Index out of range\nColours range from 0 to 3 (included)",
               (int)index ) ;
      PyErr_SetString ( pyMalef_BoundsError, message ) ;
      return NULL ;
   } else {
      return PyLong_FromLong ( self->color[index] ) ;
   }
}


/*
 * __setitem__
 *
 * This function is called when trying to change an item from the Colour.
 *
 * @exception BoundsError
 * This exception is raised when trying to access an item out of bounds.
 */
static int
pyMalef_Color___setitem__ ( _pyMalef_colorStruct *self,
                            Py_ssize_t            index,
                            PyObject             *value ) {
   char message[512] ;
   long new_value ;
   if ( index < 0 || index >= PYMALEF_COLOR_LENGTH ) {
      sprintf ( message,
                "%d: Index out of range\nColours range from 0 to 3 (included)",
                (int)index ) ;
      PyErr_SetString ( pyMalef_BoundsError, message ) ;
      return -1 ;
   } else {
      new_value = PyLong_AsLong ( value ) ;
      if ( new_value >= 0 && new_value < 256 ) {
         // When there is an OverflowError -1 is returned, thus we can just
         // check if it's in the `range 0 .. 255' then we can assign it.
         self->color[index] = new_value ;

         return 0 ;
      }
      if ( PyErr_Occurred () != NULL ) {
         // An error may have occurred.
         return -1 ;
      }
      // A bounds error have defintely occurred.
      PyErr_SetString ( pyMalef_BoundsError,
                        "The colour's components range from 0 to 255 "\
                        "(both included)!" ) ;
      return -1 ;
   }
}


/*
 * __contains__
 *
 * This function is called when the `in' operator is used. It checks whether an
 * item is contained by the Colour. There is a predefined `__contains__'
 * method. However, it raises a Bounds_Error. That's why a new one is declared.
 *
 * @return
 * It returns 1 (true), if it was found; otherwise, 0 is returned.
 *
 */
static int
pyMalef_Color___contains__ ( _pyMalef_colorStruct *self,
                             PyObject             *element ) {
   long value = PyLong_AsLong ( element ) ;
   if ( PyErr_Occurred () != NULL ) {
      // An error has occurred.
      return -1 ;
   }

   for ( int i = 0 ; i < 4 ; i++ ) {
      if ( (long)self->color[i] == value ) {
         return 1 ;
      }
   }
   return 0 ;
}



/*###########################################################################*\
 *######################### P Y T H O N   C O L O R #########################*
\*###########################################################################*/

static PySequenceMethods
_pyMalef_Color_as_sequence = {
   .sq_length   = (lenfunc)         pyMalef_Color___len__,
   .sq_item     = (ssizeargfunc)    pyMalef_Color___getitem__,
   .sq_ass_item = (ssizeobjargproc) pyMalef_Color___setitem__,
   .sq_contains = (objobjproc)      pyMalef_Color___contains__
} ;

static PyTypeObject
pyMalef_Color = {
   PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name      = "malef.Color",
   .tp_doc       = "TODO: Add documentation",
   .tp_basicsize = sizeof(_pyMalef_colorStruct),
   .tp_itemsize  = 8,
   .tp_flags     = Py_TPFLAGS_DEFAULT,
   .tp_new       = PyType_GenericNew,

   .tp_as_sequence = &_pyMalef_Color_as_sequence,
} ;


/*###########################################################################*\
 *######################### C O L O R   F / I N I T #########################*
\*###########################################################################*/

/*
 * This function finalizes and clears the Colour declared in this header.
 *
 * @param module
 * The module from which we want to free the Colour type.
 */
static inline void
_pyMalef_finalizeColors ( PyObject *module ) {

   Py_DECREF ( &pyMalef_Color ) ;
}


/*
 * This function adds the Colour type to the module.
 *
 * @param module
 * The module.
 *
 * @return
 * Whether it has succeeded.
 */
static inline bool
_pyMalef_initializeColors ( PyObject *module ) {

   if ( PyType_Ready ( &pyMalef_Color ) < 0 ) {
      return false ;
   }

   Py_INCREF ( &pyMalef_Color ) ;
   if ( PyModule_AddObject ( module, "Color",
                             (PyObject*)&(pyMalef_Color) ) < 0 ) {
      Py_DECREF ( &pyMalef_Color ) ;
      return false ;
   }

   return true ;
}

#endif//MALEF_COLORS_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
