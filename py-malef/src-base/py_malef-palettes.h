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
 * *** malef.Palette.__new__ ***
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
 * *** malef.Palette.__len__ ***
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
 * *** malef.Palette.__getitem__ ***
 *
 * This function is called when subscripting a palette.
 *
 * @param self
 * The palette that we want to subscript.
 *
 * @param index
 * The index of the palette we want to get. Here we count from 0.
 *
 * @exception IndexError
 * This exception is raised when trying to access an item out of bounds.
 */
static PyObject*
pyMalef_Palette___getitem__ ( _pyMalef_paletteStruct *self,
                              Py_ssize_t             index ) {
   char message[512] ;
   if ( index < 0 || index >= PYMALEF_PALETTE_LENGTH ) {
      sprintf ( message,
                "%d: Index out of range\nPalettes range from 0 to 15 (both "
                  "included)",
                (int)index ) ;
      PyErr_SetString ( PyExc_IndexError, message ) ;
      return NULL ;
   } else {
      Py_INCREF (self->palette[index]) ;
      return self->palette[index] ;
   }
}


// TODO: static PyObject* pyMalef_Palette___setitem


/*
 * *** malef.Palette.__repr__ ***
 *
 * This function returns a string that shos the representation of the Palette
 * data type showing all the colours inside it.
 */
static PyObject*
pyMalef_Palette___repr__ ( _pyMalef_paletteStruct *self ) {

   return PyUnicode_FromFormat ( "{ Dim: %R, %R, %R, %R, %R, %R, %R, %R; "
                                 "  Bright: %R, %R, %R, %R, %R, %R, %R, %R }",
                                 self->palette[0],
                                 self->palette[1],
                                 self->palette[2],
                                 self->palette[3],
                                 self->palette[4],
                                 self->palette[5],
                                 self->palette[6],
                                 self->palette[7],

                                 self->palette[8],
                                 self->palette[9],
                                 self->palette[10],
                                 self->palette[11],
                                 self->palette[12],
                                 self->palette[13],
                                 self->palette[14],
                                 self->palette[15] ) ;
}

// TODO: __eq__

/*###########################################################################*\
 *####################### P Y T H O N   P A L E T T E #######################*
\*###########################################################################*/

static PySequenceMethods
_pyMalef_Palette_as_sequence = {
   .sq_length   = (lenfunc)         pyMalef_Palette___len__,
   .sq_item     = (ssizeargfunc)    pyMalef_Palette___getitem__,
// .sq_ass_item = (ssizeobjargproc) pyMalef_Palette___setitem__
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
   .tp_repr      = (reprfunc)pyMalef_Palette___repr__,

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



/*###########################################################################*\
 *#################### P A L E T T E   F U N C T I O N S ####################*
\*###########################################################################*/

/*
 * This function converts a C palette into a Python object.
 *
 * @param palette
 * The C palette we want to convert to Python.
 *
 * @return
 * The converted palette.
 */
static inline PyObject*
_pyMalef_paletteC2Py ( malef_palette_t palette ) {

   PyObject* pyPalette = PyObject_CallObject ( (PyObject*)&pyMalef_Palette,
                                               NULL ) ;

   for ( int i = 0 ; i < 16 ; i++ ) {
      for ( int c = 0 ; c < 4 ; c++ ) {
         ((_pyMalef_colorStruct*)(((_pyMalef_paletteStruct*)pyPalette)->
            palette[i]))->color[c] = palette[i/8][i%8][c] ;
      }
   }

   return pyPalette ;
}


/*
 * This function converts a Python object into a C palette.
 *
 * @param palette
 * The Python object we want to convert to C.
 *
 * @return
 * The converted palette.
 */
static inline void
_pyMalef_palettePy2C ( PyObject         *pyPalette,
                        malef_palette_t *palette) {

   for ( int i = 0 ; i < 16 ; i++ ) {
      for ( int c = 0 ; c < 4 ; c++ ) {
         *palette[i/8][i%8][c] = ((_pyMalef_colorStruct*)
               (((_pyMalef_paletteStruct*)
                  pyPalette)->palette[i]))->color[c] ;
      }
   }
}



/* *** malef.getPalette *** */
PyDoc_STRVAR ( pyMalef_getPalette_doc,
"This function returns the Palette in use if no arguments are given; or the " \
"palette represented by the same value. The available values are declared in "\
"the `PaletteEnum' class, any other value will raise a IndexError exception."
);
static PyObject*
pyMalef_getPalette ( PyObject *self,
                     PyObject *args ) {
   malef_paletteKind_t paletteKind = 0 ;
   int arg_count = PyTuple_GET_SIZE ( args ) ;

   if ( ! PyArg_ParseTuple ( args, "|H", &paletteKind ) ) {
      return NULL ;
   }

   PyObject *pyPalette ;
   malef_palette_t palette ;
   malef_error_t err ;

   if ( arg_count == 0 ) {
      // In order to retrieve it, malef should be initialised, we get the
      // error code just in case.
      err = malef_getPalette ( &palette ) ;
      if ( _pyMalef_raiseException (err) ) {
         return NULL ;
      }
   } else {
      // We check that the enumeration value is in the range.
      // TODO: Change this if new palettes are added.
      if( paletteKind >= malef_MALEF_PALETTE && paletteKind <= malef_UBUNTU ) {
         err = malef_getPaletteKind ( paletteKind, &palette );
         if ( _pyMalef_raiseException (err) ) {
            return NULL ;
         }
      } else {
         PyErr_SetString ( PyExc_IndexError,
                           "The value given is not in the palettes range, take"
                           " a look at the malef.PaletteEnum class." ) ;
         return NULL ;
      }
   }

   pyPalette = _pyMalef_paletteC2Py ( palette ) ;
   
   return pyPalette ;
}
#define pyMalef_getPalette_method {                                           \
   "getPalette",                                                              \
   (PyCFunction)pyMalef_getPalette,                                           \
   METH_VARARGS | METH_KEYWORDS,                                              \
   pyMalef_getPalette_doc                                                     \
}


/* *** malef.setPalette *** */
PyDoc_STRVAR ( pyMalef_setPalette_doc,
"This function changes the current Palette in use either by using a value "   \
"(declared in PaletteEnum) or by using a user-defined Palette." ) ;
static PyObject*
pyMalef_setPalette ( PyObject *self,
                     PyObject *args,
                     PyObject *kwargs ) {

   static char *keyword_list[] = { "palette", NULL } ;
   PyObject* pyPalette ;
   malef_palette_t palette ;
   malef_paletteKind_t paletteKind ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "O", keyword_list,
                                        &pyPalette ) ) {
      return NULL ;
   }

   malef_error_t err ;
   // We check whether it's an integer or a Palette.
   if ( PyObject_IsInstance ( pyPalette, (PyObject*)&pyMalef_Palette ) ) {
      _pyMalef_palettePy2C ( pyPalette, &palette ) ;
      err = malef_setPalette ( palette ) ;
   } else if ( PyLong_Check ( pyPalette ) ) {
      paletteKind = PyLong_AsLong ( pyPalette ) ;
      err = malef_setPaletteKind ( paletteKind ) ;
   } else {
      PyErr_SetString ( PyExc_TypeError,
                        "A palette or integer was expected!" ) ;
      return NULL ;
   }

   if ( _pyMalef_raiseException (err) ) {
      return NULL ;
   } else {
      Py_RETURN_NONE ;
   }
}
#define pyMalef_setPalette_method {                                           \
   "setPalette",                                                              \
   (PyCFunction)pyMalef_setPalette,                                           \
   METH_VARARGS | METH_KEYWORDS,                                              \
   pyMalef_setPalette_doc                                                     \
}


#endif//PY_MALEF_PALETTES_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
