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


/*###########################################################################*\
 *##### S U R F A C E - W R A P P E R   T Y P E   D E C L A R A T I O N #####*
\*###########################################################################*/

/*
 * This is a wrapper that wraps the C Surface type which wraps the Ada Surface
 * type. This will be hidden from the user, the user must use class methods to
 * interact with the Surface. That way, it's completely hidden and cannot be
 * modified directly.
 *
 * @field surface
 * This is the C surface the object is pointing to, there are no problems with
 * them because, Python will call the finalization, adjust and initialization
 * functions from the Ada Controlled type to take care of memory management.
 */
typedef struct {
   PyObject_HEAD
   malef_surface_t surface ;
} _pyMalef_surfaceStruct ;



/*###########################################################################*\
 *###################### P R I V A T E   M E T H O D S ######################*
\*###########################################################################*/

/*
 * This is the Surface destructor, this function is called when the reference
 * count in Python reaches 0. However, as it's a Controlled type it may not be
 * completely deallocated. For example:
 *
 *  | Imagine we create a Surface object in Python with:
 *  |    my_surf = Surface(10, 12)
 *  | Ada will allocate it and keep its own reference count, if other variable
 *  | references to `my_surf' as in:
 *  |    your_surf = my_surf
 *  | The Ada reference count won't be touched, but Python's one will. If both
 *  | variables exited the scope, then Ada's counter will be reduced and thus
 *  | be completely finallized and deallocated.
 *
 * @param self
 * The Surface we want to deallocate.
 */
static void
pyMalef_Surface___dealloc__ ( _pyMalef_surfaceStruct* self ) {

   // We reduce the reference count for Ada's controlled type using the C
   // binding. If there are more references, it won't be completely freed.
   malef_destroySurface ( self->surface );
}


/*
 * __new__
 *
 * This is the function that is called when a new Surface is created.
 */
static PyObject*
pyMalef_Surface___new__ ( PyTypeObject *type,
                          PyObject     *args,
                          PyObject     *kwargs ) {

   // We deckare a new surfaceStruct and allocate it.
   _pyMalef_surfaceStruct *self ;
   self = (_pyMalef_surfaceStruct*)type->tp_alloc( type, 0 ) ;
   if ( self != NULL ) {
      self->surface = malef_getNullSurface () ;
   }

   // We return it.
   return (PyObject*)self ;
}


/*
 * __init__
 *
 * This function is called when the Surface is initialized, in Python it takes
 * the following parameters:
 *
 * @param height
 * The height of the Surface, it must be number greater than 0.
 *
 * @param width
 * The width of the Surface, it must be number greter than 0.
 *
 * @exception BoundsError
 * This exception is raised whenever the numbeer is 0 or negative.
 */
static int
pyMalef_Surface___init__ ( _pyMalef_surfaceStruct *self,
                           PyObject               *args,
                           PyObject               *kwargs ) {

   // These are the parameters names.
   static char *keyword_list[] = { "height", "width", NULL } ;
   malef_col_t height ;
   malef_row_t width ;

   // We parse the giver arguments.
   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "HH", keyword_list,
                                        &height, &width ) ) {
      return -1 ;
   }

   // Finally we create the surface and get the possible error.
   malef_error_t err = malef_createSurface ( height, width, self->surface ) ;

   if ( _pyMalef_raiseException (err) ) {
      return -1 ;
   } else {
      return 0 ;
   }
}


/*
 * This function is called when a Surface exits a scope or is deleted. The
 * object won't be deallocated unless everyone of them is finalized. This
 * function has been taken from the Python C API official page's examples. It's
 * only a template and maybe something will be added here in the future.
 */
static void
pyMalef_Surface___finalize__ ( PyObject *self ) {
   PyObject *error_type, *error_value, *error_traceback;

   // Save the current exception, if any.
   PyErr_Fetch ( &error_type, &error_value, &error_traceback ) ;

   // Restore the saved exception.
   PyErr_Restore ( error_type, error_value, error_traceback ) ;
}



/*###########################################################################*\
 *####################### P U B L I C   M E T H O D S #######################*
\*###########################################################################*/

PyDoc_STRVAR (pyMalef_Surface_debugPut_doc,
"This function is currently used for development purposes, it only prints the"\
" surface onto the screen. A better function will be added in the future.") ;
static PyObject *
pyMalef_Surface_debugPut ( _pyMalef_surfaceStruct *self,
                           PyObject *Py_UNUSED(ignored) ) {

   // We just print it and return None.
   _malef_debugPutSurface ( self->surface ) ;
   Py_RETURN_NONE ;
}
#define pyMalef_Surface_debugPut_method {                                     \
   "debugPut",                                                                \
   (PyCFunction)pyMalef_Surface_debugPut,                                     \
   METH_NOARGS,                                                               \
   pyMalef_Surface_debugPut_doc                                               \
}



/*###########################################################################*\
 *####################### P Y T H O N   S U R F A C E #######################*
\*###########################################################################*/

static PyMethodDef
pyMalef_SurfaceMethods[] = {
   pyMalef_Surface_debugPut_method,
   { NULL, NULL, 0, NULL }
} ;


// Some of these are commented, but will be added in a future.
static PyTypeObject
pyMalef_Surface = {
    PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name      = "malef.Surface",
   .tp_basicsize = sizeof(_pyMalef_surfaceStruct),
// .tp_itemsize
   .tp_dealloc   = (destructor)pyMalef_Surface___dealloc__,
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
   .tp_methods   = pyMalef_SurfaceMethods,
// .tp_members
// .tp_getset
   .tp_dict      = NULL,
// .tp_descr_get
// .tp_descr_set
   .tp_init      = (initproc)pyMalef_Surface___init__,
/* .tp_alloc
 */
   .tp_new       = (newfunc)pyMalef_Surface___new__,
   .tp_finalize  = pyMalef_Surface___finalize__
} ;



/*###########################################################################*\
 *###################### S U R F A C E S   F / I N I T ######################*
\*###########################################################################*/

/*
 * This function finalizes and clears the Surface type declared in this header.
 * 
 * @param module
 * The module from which we want to free the Surface type.
 */
static inline void
_pyMalef_finalizeSurfaces ( PyObject *module ) {

   Py_DECREF ( &pyMalef_Surface ) ;
}


/*
 * This function adds the Surface type to the module.
 *
 * @param module
 * The module.
 *
 * @return
 * Whether it has succeeded.
 */
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
