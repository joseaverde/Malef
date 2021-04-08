/*****************************************************************************\
 *                                                                           * 
 *              P Y _ M A L E F - P A L E T T E _ E N U M S . H              * 
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

#ifndef MALEF_PALETTE_ENUMS_H
#define MALEF_PALETTE_ENUMS_H

#include <stddef.h>
#include "structmember.h"

/*
 * This struct contains all the Palette kinds.
 */
typedef struct {
   PyObject_HEAD
   int MALEF_PALETTE,
       VGA,
       WINDOWS_XP_CONSOLE,
       WINDOWS_POWERSHELL,
       VISUAL_STUDIO_CODE,
       WINDOWS_10_CONSOLE,
       TERMINAL_APP,
       PUTTY,
       MIRC,
       XTERM,
       UBUNTU;
} _pyMalef_paletteEnumStruct ;


/*
 * This is the PaletteEnum type already initialized for use.
 */
static PyObject *pyMalef_palettes ;


/*###########################################################################*\
 *##################### P R I V A T E   M E T H O D S #######################*
\*###########################################################################*/

/*
 * *** malef.PaletteEnum.__new__ ***
 *
 * This function allocates the ColorEnum class and initialises its values.
 */
static PyObject*
pyMalef_PaletteEnum___new__ ( PyTypeObject *type,
                              PyObject     *args,
                              PyObject     *kwargs ) {
   // We create a new object and allocate it.
   _pyMalef_paletteEnumStruct *self ;
   self = (_pyMalef_paletteEnumStruct*)type->tp_alloc( type, 0 ) ;
   if ( self != NULL ) {
      self->MALEF_PALETTE      = malef_MALEF_PALETTE ;
      self->VGA                = malef_VGA ;
      self->WINDOWS_XP_CONSOLE = malef_WINDOWS_XP_CONSOLE ;
      self->WINDOWS_POWERSHELL = malef_WINDOWS_POWERSHELL ;
      self->VISUAL_STUDIO_CODE = malef_VISUAL_STUDIO_CODE ;
      self->WINDOWS_10_CONSOLE = malef_WINDOWS_10_CONSOLE ;
      self->TERMINAL_APP       = malef_TERMINAL_APP       ;
      self->PUTTY              = malef_PUTTY              ;
      self->MIRC               = malef_MIRC               ;
      self->XTERM              = malef_XTERM              ;
      self->UBUNTU             = malef_UBUNTU             ;
   }

   return (PyObject*)self ;
}


/*
 * *** malef.PaletteEnum.__iter__ ***
 *
 * This function returns an iterator that can be used to iterate over all the
 * palette names inside the PaletteEnum type. This could be useful to iterate
 * through all predefined palettes.
 */
static PyObject*
pyMalef_PaletteEnum___iter__ ( _pyMalef_paletteEnumStruct *self ) {

   PyObject *pyIterator = PyObject_CallObject ( (PyObject*)
                                                &pyMalef_EnumIterator, NULL ) ;
   _pyMalef_enumIteratorStruct *iterator = (_pyMalef_enumIteratorStruct*)
                                            pyIterator ;

   iterator->from = self->MALEF_PALETTE ;
   iterator->to   = self->UBUNTU ;

   return pyIterator ;
}



/*###########################################################################*\
 *###################### P U B L I C   M E T H O D S ########################*
\*###########################################################################*/

static const char *_pyMalef_PALETTE_ENUM_NAMES[] = {
   "MALEF_PALETTE",
   "VGA",
   "WINDOWS_XP_CONSOLE",
   "WINDOWS_POWERSHELL",
   "VISUAL_STUDIO_CODE",
   "WINDOWS_10_CONSOLE",
   "TERMINAL_APP",
   "PUTTY",
   "MIRC",
   "XTERM",
   "UBUNTU",
} ;

/* *** malef.PaletteEnum.image *** */
PyDoc_STRVAR ( pyMalef_PaletteEnum_image_doc,
"This function returns the image (the string) of each of the values of the "
"enumeration." ) ;
static PyObject*
pyMalef_PaletteEnum_image ( PyObject *self,
                            PyObject *args,
                            PyObject *kwargs ) {

   static char *keyword_list[] = { "value", NULL } ;
   malef_paletteKind_t palette ;

   if ( ! PyArg_ParseTupleAndKeywords ( args, kwargs, "h", keyword_list,
                                        &palette ) ) {
      return NULL ;
   }

   if ( palette < malef_MALEF_PALETTE || palette > malef_UBUNTU ) {
      PyErr_SetString ( pyMalef_BoundsError,
                       "Invalid value for a Palette!" ) ;
      return NULL ;
   }

   return PyUnicode_FromString ( _pyMalef_PALETTE_ENUM_NAMES[palette] ) ;
}
#define pyMalef_PaletteEnum_image_method {                                    \
   "image",                                                                   \
   (PyCFunction)pyMalef_PaletteEnum_image,                                    \
   METH_VARARGS | METH_KEYWORDS,                                              \
   pyMalef_PaletteEnum_image_doc                                              \
}



/*###########################################################################*\
 *################# P Y T H O N   P A L E T T E _ E N U M  ##################*
\*###########################################################################*/


static PyMethodDef
pyMalef_PaletteEnumMethods[] = {
   pyMalef_PaletteEnum_image_method,
   { NULL, NULL, 0, NULL }
} ;


#define _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(name) \
   {#name, T_INT, offsetof (_pyMalef_paletteEnumStruct, name), READONLY, #name}
static PyMemberDef
pyMalef_PaletteEnumMembers[] = {
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(MALEF_PALETTE),
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(VGA),
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(WINDOWS_XP_CONSOLE),
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(WINDOWS_POWERSHELL),
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(VISUAL_STUDIO_CODE),
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(WINDOWS_10_CONSOLE),
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(TERMINAL_APP),
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(PUTTY),
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(MIRC),
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(XTERM),
   _PYMALEF_PALETTE_ENUM_DEFINE_PALETTE_MEMBER(UBUNTU),
   {NULL}      /* SENTINEL */
} ;

static PyTypeObject
pyMalef_PaletteEnum = {
   PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name      = "malef.PaletteEnum",
   .tp_doc       = "This is an enumeration type that assigns a CONSTANT "
                   "integer value to each of the predefined palettes. These "
                   "values can be used to change the default palette or to "
                   "get one of the predefined palettes.",
   .tp_basicsize = sizeof(_pyMalef_paletteEnumStruct),
   .tp_new       = pyMalef_PaletteEnum___new__,
   .tp_iter      = (getiterfunc)pyMalef_PaletteEnum___iter__,
   .tp_members   = pyMalef_PaletteEnumMembers,
   .tp_methods   = pyMalef_PaletteEnumMethods,
} ;


#endif//MALEF_PALETTE_ENUMS_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
