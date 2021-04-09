/*****************************************************************************\
 *                                                                           * 
 *             P Y _ M A L E F - E N U M _ I T E R A T O R S . H             * 
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

#ifndef MALEF_ENUM_ITERATORS_H
#define MALEF_ENUM_ITERATORS_H

#include <Python.h>


/*
 * This is the EnumIterator type, as it names says, it can be used to iterate
 * over enumeration types. It's very simple because the enumeration types
 * declared in this binding only contain an arithmetic sequence of step one of
 * constant integers as values of the enumeration.
 *
 * This is the type that will be returned after been initialised when called.
 */
typedef struct {
   PyObject_HEAD
   int from ;
   int to ;
   int index ;
} _pyMalef_enumIteratorStruct ;


/*
 * *** malef._EnumIterator.__iter__ ***
 *
 * This function just returns itself, is the iterator that is returned when
 * trying to iterate.
 */
static PyObject*
pyMalef_EnumIterator___iter__ ( PyObject *pySelf ) {

   Py_INCREF ( pySelf ) ;
   return pySelf ;
}


/*
 * *** malef._EnumIterator.__next__ ***
 *
 * This is the function that is called when passing to the next item in the
 * iteration. It just increases the index and returns the current value.
 */
static PyObject*
pyMalef_EnumIterator___next__ ( PyObject *pySelf ) {

   _pyMalef_enumIteratorStruct *self = (_pyMalef_enumIteratorStruct*)pySelf ;
   if ( self->index <= self->to ) {
      return PyLong_FromLong ( self->index++ ) ;
   } else {
      PyErr_SetNone ( PyExc_StopIteration ) ;
      return NULL ;
   }
}



static PyTypeObject
pyMalef_EnumIterator = {
   PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name      = "malef._EnumIterator",
   .tp_doc       = "Iterator for enumeration types.",
   .tp_basicsize = sizeof(_pyMalef_enumIteratorStruct),
   .tp_iter      = pyMalef_EnumIterator___iter__,
   .tp_iternext  = pyMalef_EnumIterator___next__,
   .tp_new       = PyType_GenericNew,
} ;


#endif//MALEF_ENUM_ITERATORS_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
