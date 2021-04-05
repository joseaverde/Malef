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

typedef struct {
   PyObject_HEAD
   int from ;
   int to ;
   int index ;
} _pyMalef_enumIteratorStruct ;


static PyObject*
pyMalef_EnumIterator___iter__ ( PyObject *pySelf ) {

   Py_INCREF ( pySelf ) ;
   return pySelf ;
}


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



static void
_pyMalef_finalizeEnumIterators ( PyObject *module ) {

   Py_DECREF ( &pyMalef_EnumIterator ) ;
}


static bool
_pyMalef_initializeEnumIterators ( PyObject *module ) {

   if ( PyType_Ready ( &pyMalef_EnumIterator ) < 0 ) {
      return false ;
   }

   Py_INCREF ( &pyMalef_EnumIterator ) ;
   if ( PyModule_AddObject ( module, "_EnumIterator",
                             (PyObject*)&(pyMalef_EnumIterator) ) < 0 ) {
      _pyMalef_finalizeEnumIterators ( module ) ;
      return false ;
   }

   return true ;
}



#endif//MALEF_ENUM_ITERATORS_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
