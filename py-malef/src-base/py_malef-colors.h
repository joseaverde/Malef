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
#include <stddef.h>
#include <Malef.h>

#include "structmember.h"
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

static PyTypeObject pyMalef_Color ;


/*
 * This function converts a character from hexadecimal to decimal.
 *
 * @param item
 * The character to convert.
 *
 * @return
 * It returns it's value, but it returns -1 if there is an error. The exception
 * is also raised, what's left is returning NULL.
 */
static inline int
_pyMalef_char2int ( char item ) {

   if ( item >= '0' && item <= '9' ) {
      return item - '0' ;
   } else if ( item >= 'A' && item <= 'Z' ) {
      return item - 'A' ;
   } else if ( item >= 'a' && item <= 'z' ) {
      return item - 'a' ;
   } else {
      PyErr_SetString ( PyExc_ValueError,
                        "Invalid hex value!" ) ;
      return -1 ;
   }
}

/*
 * This complex function converts an object into a Color. It can convert:
 *
 * @param value
 *  - Integer (A 32-bits unsigned hex integer)
 *  - Color (It does nothing)
 *  - String (It must start with a Hash '#' and it can be 3, 4, 7 or 8 chars
 *    long)
 *  - Sequence (It must have a lenght of 4 and the __getitem__ method in order
 *    for it to work).
 *
 * @return
 * The converted colour.
 */
PyObject*
_pyMalef_cast2Color ( PyObject *value ) {

   malef_color_t colorArray ;
   int64_t       colorValue ;
   const char    *colorHex  ;
   // We first check the type of the value.
   if ( PyObject_IsInstance ( value, (PyObject*)&pyMalef_Color ) ) {
      // We just return it, nothing to be done here.
      return value ;
   } else if ( PyLong_Check ( value ) ) {
      // If it's an integer so we can expect a 32 bit integer we can then
      // convert to an array.
      colorValue = PyLong_AsLongLong (value) ;
      // We check for overflow.
      if ( PyErr_Occurred () ) {
         return NULL ;
      }
      if ( colorValue > 0xFFFFFFFF || colorValue < 0x00000000 ) {
         PyErr_SetString ( pyMalef_BoundsError,
                          "Number to big to represent a colour, a 32 bits "
                          "unsigned integer was expected!") ;
         return NULL ;
      }
      // Otherwise we just assign it.
      for ( int i = 0 ; i < 4 ; i++ ) {
         colorArray[i] = ( colorValue >> 8*( 3 - i ) ) & 0xFF ;
      }
   } else if ( PyUnicode_Check (value) ) { 
      colorHex = PyUnicode_AsUTF8AndSize ( value, (Py_ssize_t*)&colorValue ) ;
      colorArray[3] = 0xFF ;
      switch ( colorValue ) {
         case 4:
         case 5:
            for ( int i = 1, tmp ; i < colorValue ; i++ ) {
               tmp = _pyMalef_char2int ( colorHex[i] ) ;
               if ( tmp == -1 ) {
                  return NULL ;
               } else {
                  colorArray[i-1] = tmp*16 + tmp ;
               }
            }
            break ;
         case 7:
         case 9:
            for ( int i = 1, tmp, tmp2 ; i < colorValue ; i += 2 ) {
               tmp = _pyMalef_char2int ( colorHex[i] ) ;
               if ( tmp == -1 ) {
                  return NULL ;
               }
               tmp2 = _pyMalef_char2int ( colorHex[i+1] ) ;
               colorArray [i/2] = tmp * 16 + tmp2 ;
            }
            break ;
         default:
            PyErr_SetString ( PyExc_ValueError,
                              "The string should start with a hash '#' and be "
                              "3, 4, 7 or 9 characters long! For example: "
                              "'#123', '#2212', '#123456', '#12345678'" ) ;
            return NULL ;
         }
   } else {
      // Finally we check if we can iterate it. First we check it has a length.
      if ( PyObject_Length (value) != 4 ) {
         PyErr_SetString ( PyExc_TypeError,
                           "Value couldn't be converted to Color!" ) ;
         return NULL ;
      }
      // If so we can iterate it.
      for ( int i = 0 ; i < 4 ; i++ ) {
         colorValue = PyLong_AsLong ( PyObject_GetItem (
                                             value, PyLong_FromLong(i)) ) ;
         if ( colorValue == -1 ) {
            if ( PyErr_Occurred () ) {
               return NULL ;
            }
         }
         if ( colorValue < 0 || colorValue > 255 ) {
            PyErr_SetString ( pyMalef_BoundsError,
                              "The colour components must be in the range "
                              "0 .. 255 (both included)." ) ;
            return NULL ;
         }
         colorArray[i] = colorValue ;
      }
   }

   PyObject *pyColor = PyObject_CallObject ( (PyObject*)&pyMalef_Color, NULL );
    _pyMalef_colorStruct *color = (_pyMalef_colorStruct*)pyColor ;

    for ( int i = 0 ; i < 4 ; i++ ) {
       color->color[i] = colorArray[i] ;
    }

   return pyColor ;
}


/*###########################################################################*\
 *###################### P R I V A T E   M E T H O D S ######################*
\*###########################################################################*/

/*
 * *** malef.Color.__init__ ***
 *
 * This function initialises a colour object. It takes as argument an object
 * that will be casted into the Color type. If no argument is given, then
 * nothing is done.
 */

static int
pyMalef_Color___init__ ( _pyMalef_colorStruct *self,
                         PyObject             *args,
                         PyObject             *kwargs ) {

   static char *keyword_list[] = { "object", NULL } ;
   PyObject *newColor = NULL ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "|O", keyword_list,
                                        &newColor ) ) {
      return -1 ;
   }

   if ( newColor == NULL ) {
      // It means we have no initialiser, thus we can safely initialise all the
      // components from the colour to zero.
      for ( int i = 0 ; i < 4 ; i++ ) {
         self->color[i] = 0 ;
      }
      // Success.
      return 0 ;
   } else {
      // Otherwise an initialiser has been given as a parameter, therefore we
      // can cast it to a Colour type:
      PyObject *castColor = _pyMalef_cast2Color ( newColor ) ;
      if ( castColor == NULL ) {
         // An error has occurred, we can safely return -1, because the string
         // has already been set.
         return -1 ;
      } else {
         // Otherwise we just assign it.
         for ( int i = 0 ; i < 4 ; i++ ) {
            self->color[i] = ((_pyMalef_colorStruct*)castColor)->color[i] ;
         }
         // We decrease the reference of the new colour.
         Py_DECREF ( castColor ) ;
         // Success.
         return 0 ;
      }
   }
}


/*
 * *** malef.Color.__len__ ***
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
 * *** malef.Color.__getitem__ ***
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
 * @exception IndexError
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
      PyErr_SetString (PyExc_IndexError, message ) ;
      return NULL ;
   } else {
      return PyLong_FromLong ( self->color[index] ) ;
   }
}


/*
 * *** malef.Color.__setitem__ ***
 *
 * This function is called when trying to change an item from the Colour.
 *
 * @exception IndexError
 * This exception is raised when trying to access an item out of bounds.
 *
 * @exception BoundsError
 * This exception is raised when trying to assign a colour component with a
 * value greater than 255 or lower than 0 (1 unsigned byte).
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
      PyErr_SetString ( PyExc_IndexError, message ) ;
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
 * *** malef.Color.__contains__ ***
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


/*
 * *** malef.Color.__repr__ ***
 *
 * This function returns a string that shows the representation of the Colour,
 * i.e: (R, G, B, A), so the user can see what's inside.
 */
static PyObject*
pyMalef_Color___repr__ ( _pyMalef_colorStruct *self ) {

   return PyUnicode_FromFormat ( "(%d, %d, %d, %d)",
                                 self->color[0],
                                 self->color[1],
                                 self->color[2],
                                 self->color[3]) ;
}

/*
 * *** malef.Color.(comparisons) ***
 *
 * This function is used to compare two colours, the only thing that can be
 * compared is whether they are equal, other kind of comparisons are stupid,
 * how do you tell a colour is greater than another, for example:
 *  (255, 0, 0, 0) > (0, 255, 0, 0)
 * That means Red is greater than Green, we all know that is false because
 * Green is the best colour. That's why I don't bother to define those kinds
 * of comparisons.
 */
static PyObject*
pyMalef_Color___richCompare__ ( PyObject *pySelf,
                                PyObject *pyOther,
                                int      op ) {

   // We check that both are colours, because it's stupid to compare a colour
   // with another thing. The first is SUPPOSED to be a colour because it's
   // itself. Therefore we only have to test the other one.
   if ( ! PyObject_IsInstance ( pyOther, (PyObject*)&pyMalef_Color ) ) {
      // They are not implemented.
      Py_INCREF ( Py_NotImplemented ) ;
      return Py_NotImplemented ;
   }

   // We check that the operation the user asks to do, is commonsense or not.
   if ( op != Py_EQ && op != Py_NE ) {
      Py_INCREF ( Py_NotImplemented ) ;
      return Py_NotImplemented ;
   }

   // Otherwise we start the comparison.
   //
   if ( memcmp ( ((_pyMalef_colorStruct*)pySelf)->color,
                 ((_pyMalef_colorStruct*)pyOther)->color,
                 4 ) == 0 ) {
      Py_RETURN_TRUE ;
   } else {
      Py_RETURN_FALSE ;
   }
}



/*###########################################################################*\
 *######################### P Y T H O N   C O L O R #########################*
\*###########################################################################*/

static PyMemberDef
pyMalef_Color_members[] = {
   { "red"  , T_BYTE, offsetof (_pyMalef_colorStruct, color) + 0, 0, "Red"   },
   { "green", T_BYTE, offsetof (_pyMalef_colorStruct, color) + 1, 0, "Green" },
   { "blue" , T_BYTE, offsetof (_pyMalef_colorStruct, color) + 2, 0, "Blue"  },
   { "alpha", T_BYTE, offsetof (_pyMalef_colorStruct, color) + 3, 0, "Alpha" },
   {NULL}
} ;

static PySequenceMethods
_pyMalef_Color_as_sequence = {
   .sq_length   = (lenfunc)         pyMalef_Color___len__,
   .sq_item     = (ssizeargfunc)    pyMalef_Color___getitem__,
   .sq_ass_item = (ssizeobjargproc) pyMalef_Color___setitem__,
   .sq_contains = (objobjproc)      pyMalef_Color___contains__
   // NOTE: This function now is not needed, because the iteration bug has been
   //       fixed. However I'll leave it there because it's faster to iterate
   //       though its items in pure C than in Python. Because in Python it
   //       requires an iterator and call a method to compare them which can
   //       slow it down. And in C we have direct access to the array so there
   //       is no problem.
} ;

static PyTypeObject
pyMalef_Color = {
   PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name        = "malef.Color",
   .tp_doc         = "This is the Color type, it's a 32-bit array that you can"
                     " use to store colours in the format RGBA (Red, Green, "
                     "Blue and Alpha [Opacity]).",
   .tp_basicsize   = sizeof(_pyMalef_colorStruct),
   .tp_itemsize    = 8,
   .tp_flags       = Py_TPFLAGS_DEFAULT,
   .tp_new         = PyType_GenericNew,
   .tp_as_sequence = &_pyMalef_Color_as_sequence,
   .tp_members     = pyMalef_Color_members,
   .tp_repr        = (reprfunc)pyMalef_Color___repr__,
   .tp_init        = (initproc)pyMalef_Color___init__,
   .tp_richcompare = pyMalef_Color___richCompare__,
} ;


#endif//MALEF_COLORS_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
