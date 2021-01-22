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

#include "malef.h"

typedef struct {
   PyObject_HEAD
   malef_surface_t surface;
} _pyMalef_surfaceStruct;


static void
_pyMalef_tp_dealloc ( PyObject* object ) {

   malef_destroySurface;

}


static PyTypeObject
pyMalef_Surface = {
   PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name      = "malef.Surface",
   .tp_basicsize = sizeof(_pyMalef_surfaceStruct),
   .tp_dealloc   = (destructor)_pyMalef_tp_dealloc,
   .tp_
}

#endif//PY_MALEF_SURFACES_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
