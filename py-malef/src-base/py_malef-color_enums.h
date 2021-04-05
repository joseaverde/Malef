/*****************************************************************************\
 *                                                                           * 
 *                P Y _ M A L E F - C O L O R _ E N U M S . H                * 
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

#ifndef MALEF_COLOR_ENUMS_H
#define MALEF_COLOR_ENUMS_H

#include <stddef.h>
#include "structmember.h"
#include "py_malef-enum_iterators.h"


/*
 * This struct contains all the colours aliases.
 */
typedef struct {
   PyObject_HEAD
   int BLACK,
       RED,
       GREEN,
       YELLOW,
       BLUE,
       MAGENTA,
       CYAN,
       WHITE ;
   int BRIGHT_BLACK,
       BRIGHT_RED,
       BRIGHT_GREEN,
       BRIGHT_YELLOW,
       BRIGHT_BLUE,
       BRIGHT_MAGENTA,
       BRIGHT_CYAN,
       BRIGHT_WHITE ;
} _pyMalef_colorEnumStruct ;



/*###########################################################################*\
 *##################### P R I V A T E   M E T H O D S #######################*
\*###########################################################################*/

/*
 * *** malef.ColorEnum.__new__ ***
 *
 * This function allocates the ColorEnum class and sets the values of the
 * different colours.
 */
static PyObject*
pyMalef_ColorEnum___new__ ( PyTypeObject *type,
                            PyObject     *args,
                            PyObject     *kwargs ) {

   // We create a new object and allocate it.
   _pyMalef_colorEnumStruct *self ;
   self = (_pyMalef_colorEnumStruct*)type->tp_alloc( type, 0 ) ;
   if ( self != NULL ) {
      self->BLACK   = malef_BLACK   ;
      self->RED     = malef_RED     ;
      self->GREEN   = malef_GREEN   ;
      self->YELLOW  = malef_YELLOW  ;
      self->BLUE    = malef_BLUE    ;
      self->MAGENTA = malef_MAGENTA ;
      self->CYAN    = malef_CYAN    ;
      self->WHITE   = malef_WHITE   ;

      self->BRIGHT_BLACK   = malef_BLACK   + 8 ;
      self->BRIGHT_RED     = malef_RED     + 8 ;
      self->BRIGHT_GREEN   = malef_GREEN   + 8 ;
      self->BRIGHT_YELLOW  = malef_YELLOW  + 8 ;
      self->BRIGHT_BLUE    = malef_BLUE    + 8 ;
      self->BRIGHT_MAGENTA = malef_MAGENTA + 8 ;
      self->BRIGHT_CYAN    = malef_CYAN    + 8 ;
      self->BRIGHT_WHITE   = malef_WHITE   + 8 ;
   }

   // We return it.
   return (PyObject*)self ;
}

// TODO: Image Function
// TODO: Document this.
static PyObject*
pyMalef_ColorEnum___iter__ ( _pyMalef_colorEnumStruct *self ) {

   PyObject *pyIterator = PyObject_CallObject ( (PyObject*)
                                                &pyMalef_EnumIterator, NULL ) ;
   _pyMalef_enumIteratorStruct *iterator = (_pyMalef_enumIteratorStruct*)
                                             pyIterator ;

   iterator->from = self->BLACK ;
   iterator->to   = self->BRIGHT_WHITE ;

   return pyIterator ;
}



/*###########################################################################*\
 *################### P Y T H O N   C O L O R _ E N U M  ####################*
\*###########################################################################*/

#define _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(name) \
   {#name, T_INT, offsetof (_pyMalef_colorEnumStruct, name), READONLY, #name}

static PyMemberDef
pyMalef_ColorEnum_members[] = {
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(BLACK),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(RED),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(GREEN),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(YELLOW),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(BLUE),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(MAGENTA),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(CYAN),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(WHITE),

   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(BRIGHT_BLACK),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(BRIGHT_RED),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(BRIGHT_GREEN),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(BRIGHT_YELLOW),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(BRIGHT_BLUE),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(BRIGHT_MAGENTA),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(BRIGHT_CYAN),
   _PYMALEF_COLOR_ENUM_DEFINE_COLOR_MEMBER(BRIGHT_WHITE),

   { NULL }       /* SENTINEL */
} ;

static PyTypeObject
pyMalef_ColorEnum = {
   PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name      = "malef.ColorEnum",
   .tp_doc       = "TODO: Add documentation",
   .tp_basicsize = sizeof(_pyMalef_colorEnumStruct),
   .tp_new       = pyMalef_ColorEnum___new__,
   .tp_iter      = (getiterfunc)pyMalef_ColorEnum___iter__,
   .tp_members   = pyMalef_ColorEnum_members,
} ;



/*###########################################################################*\
 *################### C O L O R _ E N U M   F / I N I T  ####################*
\*###########################################################################*/


/*
 * This is the ColorEnum type already initialised for you.
 */
static PyObject *pyMalef_colors ;

/*
 * This fnuction finalises and clears the ColorEnum declared in this header.
 *
 * @param module
 * But it requieres the module where it must be removed from.
 */
static void
_pyMalef_finalizeColorEnums ( PyObject *module ) {

   Py_DECREF ( &pyMalef_colors ) ;
   Py_DECREF ( &pyMalef_ColorEnum ) ;
}

/*
 * This function adds the ColorEnum type to the module.
 *
 * @param module
 * The same as above we need to place it somewhere.
 *
 * @return
 * Whether it has succeeded or not.
 */
static bool
_pyMalef_initializeColorEnums ( PyObject *module ) {

   if ( PyType_Ready ( &pyMalef_ColorEnum ) < 0 ) {
      return false ;
   }

   pyMalef_colors = PyObject_CallObject ( (PyObject*)&pyMalef_ColorEnum,
                                          NULL ) ;

   if ( PyModule_AddObject ( module, "colors", pyMalef_colors ) < 0 ) {
      Py_DECREF ( &pyMalef_colors ) ;
      return false ;
   }

   Py_INCREF ( &pyMalef_ColorEnum ) ;
   if ( PyModule_AddObject ( module, "ColorEnum",
                            (PyObject*)&(pyMalef_ColorEnum) ) < 0 ) {
      _pyMalef_finalizeColorEnums ( module ) ;
      return false ;
   }

   return true ;
}


#endif//MALEF_COLOR_ENUMS_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
