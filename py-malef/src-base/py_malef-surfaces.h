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
#include "py_malef-colors.h"


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
pyMalef_Surface___dealloc__ ( _pyMalef_surfaceStruct *self ) {

   // We reduce the reference count for Ada's controlled type using the C
   // binding. If there are more references, it won't be completely freed.
   malef_destroySurface ( self->surface );
}


/*
 * *** pyMalef.Surface.__new__ ***
 *
 * This is the function that is called when a new Surface is created.
 */
static PyObject*
pyMalef_Surface___new__ ( PyTypeObject *type,
                          PyObject     *args,
                          PyObject     *kwargs ) {

   // We declare a new surfaceStruct and allocate it.
   _pyMalef_surfaceStruct *self ;
   self = (_pyMalef_surfaceStruct*)type->tp_alloc( type, 0 ) ;
   if ( self != NULL ) {
      self->surface = malef_getNullSurface () ;
   }

   // We return it.
   return (PyObject*)self ;
}


/*
 * *** pyMalef.Surface.__init__ ***
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

/* *** pyMalef.Surface.debugPut *** */
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

/* *** pyMalef.Surface.getForeground *** */
PyDoc_STRVAR ( pyMalef_Surface_getForeground_doc,
"This function can be used to get the foreground colour in a certain position"\
" of the surface" ) ;
static PyObject*
pyMalef_Surface_getForeground ( _pyMalef_surfaceStruct *self,
                                PyObject               *args,
                                PyObject               *kwargs) {

   static char *keyword_list[] = { "row", "col", NULL } ;
   malef_row_t row ;
   malef_col_t col ;
   malef_color_t color ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "HH", keyword_list,
                                        &row, &col ) ) {
      return NULL ;
   }

   malef_error_t err = malef_getSurfaceForeground ( self->surface,
                                                    row,
                                                    col,
                                                    &color ) ;

   if ( _pyMalef_raiseException ( err ) ) {
      return NULL ;  
   }

   PyObject *pyColor = PyObject_CallObject ( (PyObject*)&pyMalef_Color, NULL );
   for ( int i = 0 ; i < 4 ; i++ ) {
      ((_pyMalef_colorStruct *)pyColor)->color[i] = color[i] ;
   }
   
   return pyColor;
}
#define pyMalef_Surface_getForeground_method {                                \
   "getForeground",                                                           \
   (PyCFunction)pyMalef_Surface_getForeground,                                \
   METH_VARARGS | METH_KEYWORDS,                                              \
   pyMalef_Surface_getForeground_doc                                          \
}

/* *** pyMalef.Surface.getBackground *** */
PyDoc_STRVAR ( pyMalef_Surface_getBackground_doc,
"This function returns the background colour from a certain position of the " \
"surface." ) ;
static PyObject*
pyMalef_Surface_getBackground ( _pyMalef_surfaceStruct *self,
                                PyObject               *args,
                                PyObject               *kwargs ) {
   static char *keyword_list[] = { "row", "col", NULL } ;
   malef_row_t row ;
   malef_col_t col ;
   malef_color_t color ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "HH", keyword_list,
                                        &row, &col) ) {
      return NULL ;
   }

   malef_error_t err = malef_getSurfaceBackground ( self->surface,
                                                    row,
                                                    col,
                                                    &color ) ;

   if ( _pyMalef_raiseException ( err ) ) {
      return NULL ;
   }

   PyObject *pyColor = PyObject_CallObject ( (PyObject*)&pyMalef_Color, NULL ) ;
   for ( int i = 0 ; i < 4 ; i++ ) {
      ((_pyMalef_colorStruct *)pyColor)->color[i] = color[i] ;
   }

   return pyColor ;
}
#define pyMalef_Surface_getBackground_method {                                \
   "getBackground",                                                           \
   (PyCFunction)pyMalef_Surface_getBackground,                                \
   METH_VARARGS | METH_KEYWORDS,                                              \
   pyMalef_Surface_getBackground_doc                                          \
}



/* *** pyMalef.Surface.setForeground *** */
PyDoc_STRVAR ( pyMalef_Surface_setForeground_doc,
"This function can be used to change the foreground colour in a certain "     \
"position of the surface." ) ;
static PyObject*
pyMalef_Surface_setForeground ( _pyMalef_surfaceStruct *self,
                                PyObject               *args,
                                PyObject               *kwargs ) {

   static char *keyword_list[] = { "color", "from_row", "from_col",
                                            "to_row",   "to_col", NULL } ;
   malef_row_t from_row, to_row = 0;
   malef_col_t from_col, to_col = 0;
   PyObject *pyColor ;
   malef_color_t color ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "OHH|HH", keyword_list,
                                        &pyColor, &from_row, &from_col,
                                        &to_row, &to_col ) ) {
      return NULL ;
   }
   malef_row_t temp_row ;
   malef_col_t temp_col ;
   // We process the input.
   if ( to_row == 0 ) {
      to_row = from_row ;
   } else if ( to_row < from_row ) {
      // swap //
      temp_row = to_row ;
      to_row   = from_row ;
      from_row = temp_row ;
   }

   if ( to_col == 0 ) {
      to_col = from_col ;
   } else if ( to_col < from_col ) {
      // swap //
      temp_col = to_col ;
      to_col   = from_col ;
      from_col = temp_col ;
   }

   // We cast the colour to colour.
   pyColor = _pyMalef_cast2Color ( pyColor ) ;
   if ( pyColor == NULL ) {
      // An error has occurred.
      return NULL ;
   }
   for ( int i = 0 ; i < 4 ; i++ ) {
      color[i] = ((_pyMalef_colorStruct*)pyColor)->color[i] ;
   }
   // We decrease the reference to the new colour, because it won't be
   // returned.
   Py_DECREF ( pyColor ) ;

   // We call the function and return.
   malef_error_t err = malef_setSurfaceForeground ( self->surface,
                                                    from_row,
                                                    to_row,
                                                    from_col,
                                                    to_col,
                                                    color ) ;

   if ( _pyMalef_raiseException (err) ) {
      return NULL ;
   } else {
      Py_RETURN_NONE ;
   }
}
#define pyMalef_Surface_setForeground_method {                                \
   "setForeground",                                                           \
   (PyCFunction)pyMalef_Surface_setForeground,                                \
   METH_VARARGS | METH_KEYWORDS,                                              \
   pyMalef_Surface_setForeground_doc                                          \
} 



/* *** pyMalef.Surface.setBackground *** */
PyDoc_STRVAR ( pyMalef_Surface_setBackground_doc,
"This function can be used to change the background colour in a certain "     \
"position of the surface." ) ;
static PyObject*
pyMalef_Surface_setBackground ( _pyMalef_surfaceStruct *self,
                                PyObject               *args,
                                PyObject               *kwargs ) {

   static char *keyword_list[] = { "color", "from_row", "from_col",
                                            "to_row",   "to_col", NULL } ;
   malef_row_t from_row, to_row = 0;
   malef_col_t from_col, to_col = 0;
   PyObject *pyColor ;
   malef_color_t color ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "OHH|HH", keyword_list,
                                        &pyColor, &from_row, &from_col,
                                        &to_row, &to_col ) ) {
      return NULL ;
   }
   malef_row_t temp_row ;
   malef_col_t temp_col ;
   // We process the input.
   if ( to_row == 0 ) {
      to_row = from_row ;
   } else if ( to_row < from_row ) {
      // swap //
      temp_row = to_row ;
      to_row   = from_row ;
      from_row = temp_row ;
   }

   if ( to_col == 0 ) {
      to_col = from_col ;
   } else if ( to_col < from_col ) {
      // swap //
      temp_col = to_col ;
      to_col   = from_col ;
      from_col = temp_col ;
   }

   // We cast the colour to colour.
   pyColor = _pyMalef_cast2Color ( pyColor ) ;
   for ( int i = 0 ; i < 4 ; i++ ) {
      color[i] = ((_pyMalef_colorStruct*)pyColor)->color[i] ;
   }
   // We decrease the reference to the new colour, because it won't be
   // returned.
   Py_DECREF ( pyColor ) ;

   // We call the function and return.
   malef_error_t err = malef_setSurfaceBackground ( self->surface,
                                                    from_row,
                                                    to_row,
                                                    from_col,
                                                    to_col,
                                                    color ) ;

   if ( _pyMalef_raiseException (err) ) {
      return NULL ;
   } else {
      Py_RETURN_NONE ;
   }
}
#define pyMalef_Surface_setBackground_method {                                \
   "setBackground",                                                           \
   (PyCFunction)pyMalef_Surface_setBackground,                                \
   METH_VARARGS | METH_KEYWORDS,                                              \
   pyMalef_Surface_setBackground_doc                                          \
} 


/* *** pyMalef.Surface.getCursorForeground *** */
PyDoc_STRVAR ( pyMalef_Surface_getCursorForeground_doc,
"This function returns the colour of the cursor, i.e. the default writing "
"colour of the surface." ) ;
static PyObject*
pyMalef_Surface_getCursorForeground ( _pyMalef_surfaceStruct *self,
                                      PyObject               *args ) {

   PyObject *pyColor = PyObject_CallObject ( (PyObject*)&pyMalef_Color,
                                             NULL ) ;

   malef_error_t err = malef_getCursorForeground (
      self->surface,
      &((_pyMalef_colorStruct*)pyColor)->color
   ) ;

   if ( _pyMalef_raiseException ( err ) ) {
      Py_DECREF ( pyColor ) ;
      return NULL ;
   } else {
      return pyColor ;
   }

}
#define pyMalef_Surface_getCursorForeground_method {                          \
   "getCursorForeground",                                                     \
   (PyCFunction)pyMalef_Surface_getCursorForeground,                          \
   METH_VARARGS,                                                              \
   pyMalef_Surface_getCursorForeground_doc                                    \
}


/* *** pyMalef.Surface.getCursorBackground *** */
PyDoc_STRVAR ( pyMalef_Surface_getCursorBackground_doc,
"This function returns the background colour of the cursor, i.e. the default "
"writing colour of the surface on the background." ) ;
static PyObject*
pyMalef_Surface_getCursorBackground ( _pyMalef_surfaceStruct *self,
                                      PyObject               *args ) {

   PyObject *pyColor = PyObject_CallObject ( (PyObject*)&pyMalef_Color,
                                             NULL ) ;
   malef_error_t err = malef_getCursorBackground (
      self->surface,
      &((_pyMalef_colorStruct*)pyColor)->color
   ) ;

   if ( _pyMalef_raiseException ( err ) ) {
      Py_DECREF ( pyColor ) ;
      return NULL ;
   } else {
      return pyColor ;
   }
}
#define pyMalef_Surface_getCursorBackground_method {                          \
   "getCursorBackground",                                                     \
   (PyCFunction)pyMalef_Surface_getCursorBackground,                          \
   METH_VARARGS,                                                              \
   pyMalef_Surface_getCursorBackground_doc                                    \
}


/* *** pyMalef.Surface.setCursorForeground *** */
PyDoc_STRVAR ( pyMalef_Surface_setCursorForeground_doc,
"This function can be used to change the default writing foreground colour "
"of a Surface." ) ;
static PyObject*
pyMalef_Surface_setCursorForeground ( _pyMalef_surfaceStruct *self,
                                      PyObject               *args,
                                      PyObject               *kwargs ) {

   static char   *keyword_list[] = { "color", NULL } ;
   PyObject      *_pyColor ;
   malef_color_t color ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "O", keyword_list,
                                        &_pyColor ) ) {
      return NULL ;
   }

   PyObject *pyColor = _pyMalef_cast2Color ( _pyColor ) ;
   if ( pyColor == NULL ) {
      return NULL ;
   }
   for ( int i = 0 ; i < 4 ; i++ ) {
      color[i] = ((_pyMalef_colorStruct*)pyColor)->color[i] ;
   }
   Py_DECREF ( pyColor ) ;
   malef_error_t err = malef_setCursorForeground ( self->surface, color ) ;

   if ( _pyMalef_raiseException ( err ) ) {
      return NULL ;
   } else {
      Py_RETURN_NONE ;
   }
}
#define pyMalef_Surface_setCursorForeground_method {                          \
   "setCursorForeground",                                                     \
   (PyCFunction)pyMalef_Surface_setCursorForeground,                          \
   METH_VARARGS | METH_KEYWORDS,                                              \
   pyMalef_Surface_setCursorForeground_doc                                    \
}


/* *** Surface.setCursorBackground *** */
PyDoc_STRVAR ( pyMalef_Surface_setCursorBackground_doc,
"This function can be used to change the default writing background colour "
"of a Surface." ) ;
static PyObject*
pyMalef_Surface_setCursorBackground ( _pyMalef_surfaceStruct *self,
                                      PyObject               *args,
                                      PyObject               *kwargs ) {

   static char   *keyword_list[] = { "color", NULL } ;
   PyObject      *_pyColor ;
   malef_color_t color ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "O", keyword_list,
                                        &_pyColor ) ) {
      return NULL ;
   }

   PyObject *pyColor = _pyMalef_cast2Color ( _pyColor ) ;
   if ( pyColor == NULL ) {
      return NULL ;
   }
   for ( int i = 0 ; i < 4 ; i++ ) {
      color[i] = ((_pyMalef_colorStruct*)pyColor)->color[i] ;
   }
   Py_DECREF ( pyColor ) ;
   malef_error_t err = malef_setCursorBackground ( self->surface, color ) ;

   if ( _pyMalef_raiseException ( err ) ) {
      return NULL ;
   } else {
      Py_RETURN_NONE ;
   }
}
#define pyMalef_Surface_setCursorBackground_method {                          \
   "setCursorBackground",                                                     \
   (PyCFunction)pyMalef_Surface_setCursorBackground,                          \
   METH_VARARGS | METH_KEYWORDS,                                              \
   pyMalef_Surface_setCursorBackground_doc                                    \
}



/*###########################################################################*\
 *####################### P Y T H O N   S U R F A C E #######################*
\*###########################################################################*/

static PyMethodDef
pyMalef_SurfaceMethods[] = {
   pyMalef_Surface_debugPut_method,
   pyMalef_Surface_getForeground_method,
   pyMalef_Surface_getBackground_method,
   pyMalef_Surface_setForeground_method,
   pyMalef_Surface_setBackground_method,
   pyMalef_Surface_getCursorForeground_method,
   pyMalef_Surface_getCursorBackground_method,
   pyMalef_Surface_setCursorForeground_method,
   pyMalef_Surface_setCursorBackground_method,
   { NULL, NULL, 0, NULL }
} ;


// Some of these are commented, but will be added in a future.
static PyTypeObject
pyMalef_Surface = {
    PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name      = "pyMalef.Surface",
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
   .tp_flags     = Py_TPFLAGS_DEFAULT, 
   .tp_doc       = "The Surface type are objects that store a kind of grid "
                   "with characters, colours and styles that can be printed "
                   "anywhere onto the screen applying transparencies on "
                   "different layers.",
// .tp_richcompare
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


#endif//PY_MALEF_SURFACES_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
