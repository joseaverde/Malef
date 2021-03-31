/*****************************************************************************\
 *                                                                           * 
 *                   P Y _ M A L E F - P A L E T T E S . H                   * 
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

#ifndef PY_MALEF_PALETTES_H
#define PY_MALEF_PALETTES_H

#include <Python.h>
#include "Malef.h"

#include "py_malef-colors.h"
#include "py_malef-exceptions.h"


/*###########################################################################*\
 *##### P A L E T T E - W R A P P E R   T Y P E   D E C L A R A T I O N #####*
\*###########################################################################*/

#define PYMALEF_PALETTE_LENGTH 16

/*
 * This is the wrapper for the palette type. The palette type is two
 * dimensional array of colours of two rows (bright and dim) and eight colours
 * as columns. It's used to declare the default colours in a terminal if the
 * user wants to use the best ones for the terminal. However in python is just
 * a 16-items array in order to reduce complexity.
 *
 * @field palette
 * The C palette.
 */
typedef struct {
   PyObject_HEAD
   PyObject* palette[16] ;
} _pyMalef_paletteStruct ;



/*###########################################################################*\
 *###################### P R I V A T E   M E T H O D S ######################*
\*###########################################################################*/


/*
 * This is the Palette type destructor, this function is called when the
 * reference count in Python reaches 0. It has to unreference the colours it
 * contained.
 *
 * @param self
 * The Palette we want to deallocate.
 */
static void
pyMalef_Palette___dealloc__ ( _pyMalef_paletteStruct *self ) {

   for ( int i = 0 ; i < 16 ; i++ ) {
      Py_DECREF ( self->palette[i] ) ;
   }
}


/*
 * *** __new__ ***
 *
 * This function is called when a new Palette is created it will allocate the
 * Colours to a value other than NULL so it doesn't return any error.
 */
static PyObject*
pyMalef_Palette___new__ ( PyTypeObject *type,
                          PyObject     *args,
                          PyObject     *kwargs ) {

   // We declare a new paletteStruct, allocate it and allocate it's items to
   // be empty colours.
   _pyMalef_paletteStruct *self ;
   self = (_pyMalef_paletteStruct*)type->tp_alloc ( type, 0 ) ;
   if ( self != NULL ) {
      for ( int i = 0 ; i < 16 ; i++ ) {
         self->palette[i] = pyMalef_Color.tp_new ( &pyMalef_Color,
                                                   NULL, NULL ) ;
      }
   }

   return (PyObject*)self ;
}


/*
 * *** __len__ ***
 *
 * This function returns the length of the palette which is fixed to always be
 * 16 in order for it to correctly interact with the internals.
 *
 * @param self
 * The palette whose length we want to retrieve.
 *
 * @return
 * It returns the length of the colours.
*/
static Py_ssize_t
pyMalef_Palette___len__ ( _pyMalef_paletteStruct *self ) {

   return PYMALEF_PALETTE_LENGTH ;
}


/*
 * *** __getitem__ ***
 *
 * This function is called when subscripting a palette.
 *
 * @param self
 * The palette that we want to subscript.
 *
 * @param index
 * The index of the palette we want to get. Here we count from 0.
 *
 * @exception BoundsError
 * This exception is raised when trying to access an item out of bounds.
 */
static PyObject*
pyMalef_Palette___getitem__ ( _pyMalef_paletteStruct *self,
      Py_ssize_t            index ) {
   char message[512] ;
   if ( index < 0 || index >= PYMALEF_PALETTE_LENGTH ) {
      sprintf ( message,
                "%d: Index out of range\nPalettes range from 0 to 15 (both "
                  "included)",
                (int)index ) ;
      PyErr_SetString ( pyMalef_BoundsError, message ) ;
      return NULL ;
   } else {
      Py_INCREF (self->palette[index]) ;
      return self->palette[index] ;
   }
}



/*###########################################################################*\
 *####################### P Y T H O N   P A L E T T E #######################*
\*###########################################################################*/

static PySequenceMethods
_pyMalef_Palette_as_sequence = {
   .sq_length   = (lenfunc)      pyMalef_Palette___len__,
   .sq_item     = (ssizeargfunc) pyMalef_Palette___getitem__
} ;


static PyTypeObject
pyMalef_Palette = {
   PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name      = "malef.Palette",
   .tp_doc       = "TODO: Add documentation",
   .tp_basicsize = sizeof(_pyMalef_paletteStruct),
   .tp_itemsize  = PYMALEF_COLOR_LENGTH * PYMALEF_PALETTE_LENGTH,
   .tp_dealloc   = (destructor)pyMalef_Palette___dealloc__,
   .tp_flags     = Py_TPFLAGS_DEFAULT,
   .tp_new       = PyType_GenericNew,

   .tp_as_sequence = &_pyMalef_Palette_as_sequence,

   .tp_new       = (newfunc)pyMalef_Palette___new__,
} ;

// TODO: Create new function
/*###########################################################################*\
 *####################### P A L E T T E   F / I N I T #######################*
\*###########################################################################*/


/*
 * This function finalises and clears the Palettes in this header.
 *
 * @param module
 * The module from where we want to free the Palette type.
 */
static void
_pyMalef_finalizePalettes ( PyObject *module ) {

   Py_DECREF ( &pyMalef_Palette ) ;
}


/*
 * This function adds the Palette type to the Malef module.
 *
 * @param module
 * This is the module.
 *
 * @return
 * Whether it has succeeded.
 */
static bool
_pyMalef_initializePalettes ( PyObject *module ) {

   if ( PyType_Ready ( &pyMalef_Palette ) < 0 ) {
      return false ;
   }

   Py_INCREF ( &pyMalef_Palette ) ;
   if ( PyModule_AddObject (module, "Palette",
                            (PyObject*)&(pyMalef_Palette) ) < 0 ) {
      return false ;
   }

   return true ;
}



#endif//PY_MALEF_PALETTES_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
