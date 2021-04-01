/*****************************************************************************\
 *                                                                           * 
 *                      P Y _ M A L E F - U T I L S . H                      * 
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



#ifndef PY_MALEF_UTILS_H
#define PY_MALEF_UTILS_H


/*###########################################################################*\
 *###########################  V A R I A B L E S  ###########################*
\*###########################################################################*/


/*
 * This is an empty tuple, it's used throughout the library when there are no
 * arguments to pass to a function.
 */
static PyObject *_pyMalef_sharedEmptyTuple ;



/*###########################################################################*\
 *###########################  F U N C T I O N S  ###########################*
\*###########################################################################*/

/*
 * This function initializes all the utils from this library.
 */
static inline void
_pyMalef_initializeUtils ( void ) {

   // We initialize an empty tuple.
   _pyMalef_sharedEmptyTuple = PyTuple_New ( 0 ) ; // length = 0
   Py_INCREF ( _pyMalef_sharedEmptyTuple ) ;
}


/*
 * This function finalizes all the utils from this library.
 */
static inline void
_pyMalef_finalizeUtils ( void ) {

   Py_DECREF ( _pyMalef_sharedEmptyTuple ) ;
}


#endif//PY_MALEF_UTILS_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
