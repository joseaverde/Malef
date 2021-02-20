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
   pyMalef_Color palette[16] ;
} _pyMalef_paletteStruct ;



/*###########################################################################*\
 *###################### P R I V A T E   M E T H O D S ######################*
\*###########################################################################*/

/*
 * __len__
 *
 * This function returns the

*/

/*###########################################################################*\
 *####################### P Y T H O N   P A L E T T E #######################*
\*###########################################################################*/

static PySequenceMethods
_pyMalef_Palette_as_sequence = {
} ;


static PyTypeObject
pyMalef_Palette = {
   PyVarObject_HEAD_INIT ( NULL, 0 )
   .tp_name      = "malef.Palette",
   .tp_doc       = "TODO: Add documentation",
   .tp_basicsize = sizeof(_pyMalef_paletteStruct),
   .tp_itemsize  = pyMalef_Color.tp_itemsize * PYMALEF_COLOR_LENGTH *
                   PYMALEF_PALETTE_LENGTH,
   .tp_flags     = Py_TPFLAGS_DEFAULT,
   .tp_new       = PyType_GenericNew,

   .tp_as_sequence = &_pyMalef_Palette_as_sequence,
} ;


/*###########################################################################*\
 *####################### P A L E T T E   F / I N I T #######################*
\*###########################################################################*/


/*
 * This function finalises and clears the Palettes in this header.
 *
 * @param module
 * The module from where we want to free the Palette type.
 */
static inline void
_pyMalef_initializePalettes ( PyObject *module ) {

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
static inline bool
_pyMalef_initializePalettes ( PyObject *module ) {

   if ( PyType_Ready ( &pyMalef_Palette ) < 0 ) {
      return false ;
   }

   Py_INCREF ( &pyMalef_Palette ) ;
   if ( PyModule_AddObject (module, "Palette".
                            (PyObject*)&(pyMalef_Palette) ) < 0 ) {
      return false ;
   }

   return true ;
}



#endif//PY_MALEF_PALETTES_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
