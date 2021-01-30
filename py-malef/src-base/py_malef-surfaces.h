/*****************************************************************************\
 *                                                                           * 
 *                   P Y _ M A L E F - S U R F A C E S . H                   * 
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

#ifndef PY_MALEF_SURFACES_H
#define PY_MALEF_SURFACES_H

#include <Python.h>
#include <stdlib.h>
#include "Malef.h"

#include "py_malef-exceptions.h"


typedef struct {
   PyObject_HEAD
   malef_surface_t surface ;
} _pyMalef_surfaceStruct ;



static void
_pyMalef_surfaceStruct_dealloc ( _pyMalef_surfaceStruct* self ) {

   malef_destroySurface ( self->surface );

}


static PyObject*
_pyMalef_surfaceStruct_new ( PyTypeObject *type,
                             PyObject     *args,
                             PyObject     *kwargs ) {

   _pyMalef_surfaceStruct *self ;
   self = (_pyMalef_surfaceStruct*)type->tp_alloc( type, 0 ) ;
   if ( self != NULL ) {
      self->surface = malef_getNullSurface () ;
   }

   return (PyObject*)self ;
}


static int
_pyMalef_surfaceStruct_init ( _pyMalef_surfaceStruct *self,
                              PyObject               *args,
                              PyObject               *kwargs ) {

   static char *keyword_list[] = { "height", "width", NULL } ;
   malef_col_t height ;
   malef_row_t width ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "HH", keyword_list,
                                        &height, &width ) ) {
      return -1 ;
   }

   malef_error_t err = malef_createSurface ( height, width, self->surface ) ;

   if ( _pyMalef_raiseException (err) ) {
      return -1 ;
   } else {
      return 0 ;
   }
}


static void
_pyMalef_surfaceStruct_finalize ( PyObject *self ) {
   PyObject *error_type, *error_value, *error_traceback;
   /* Save the current exception, if any. */
   PyErr_Fetch ( &error_type, &error_value, &error_traceback ) ;

   /* Restore the saved exception. */
   PyErr_Restore ( error_type, error_value, error_traceback ) ;
}


static PyTypeObject
pyMalef_Surface = {
    PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name      = "malef.Surface",
   .tp_basicsize = sizeof(_pyMalef_surfaceStruct),
// .tp_itemsize
   .tp_dealloc   = (destructor)_pyMalef_surfaceStruct_dealloc,
// .tp_repr
   .tp_hash      = PyObject_HashNotImplemented,
   .tp_call      = NULL,
// .tp_str
/*
 * .tp_getattro
 * .tp_setattro
 */
   .tp_flags     = Py_TPFLAGS_DEFAULT,    // TODO: Change flags
   .tp_doc       = "TODO: Malef.surface",
// .tp_richcompare
// .tp_iter
// .tp_iternext
   .tp_methods   = NULL,
// .tp_members
// .tp_getset
   .tp_dict      = NULL,
// .tp_descr_get
// .tp_descr_set
   .tp_init      = (initproc)_pyMalef_surfaceStruct_init,
/* .tp_alloc
 */
   .tp_new       = (newfunc)_pyMalef_surfaceStruct_new,
   .tp_finalize  = _pyMalef_surfaceStruct_finalize,
} ;


static inline void
_pyMalef_finalizeSurfaces ( PyObject *module ) {

   Py_DECREF ( &pyMalef_Surface ) ;

}


static inline bool
_pyMalef_initializeSurfaces ( PyObject *module ) {

   if ( PyType_Ready ( &pyMalef_Surface ) < 0 ) {
      return false ;
   }

   Py_INCREF ( &pyMalef_Surface ) ;
   if ( PyModule_AddObject ( module, "Surface",
                             (PyObject*)&(pyMalef_Surface) ) < 0 ) {
      Py_DECREF ( &pyMalef_Surface ) ;
      return false ;
   }

   return true ;
}



#endif//PY_MALEF_SURFACES_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
