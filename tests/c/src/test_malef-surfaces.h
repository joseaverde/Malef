/*****************************************************************************\
 *                                                                           * 
 *                 T E S T _ M A L E F - S U R F A C E S . H                 * 
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

#ifndef MALEF_TEST_MALEF_SURFACES_H
#define MALEF_TEST_MALEF_SURFACES_H

/* *** Surface Creation *** */

static char*
test_malef_surfaces_create ( void ) ;

static char*
test_malef_surfaces_createBig ( void ) ;


/* *** Surface Assignation and Destruction *** */

static char*
test_malef_surfaces_assign ( void ) ;

static char*
test_malef_surfaces_destroy ( void ) ;


/* *** Put Strings/Characters *** */

static char*
test_malef_surfaces_putChar ( void ) ;

static char*
test_malef_surfaces_putStr ( void ) ;

static char*
test_malef_surfaces_putCharError ( void ) ;

static char*
test_malfe_surfaces_putStrError ( void ) ;


/* *** Get Strings/Characters *** */
static char*
test_malef_surfaces_getChar ( void ) ;

static char*
test_malef_surfaces_getStr ( void ) ;

static char*
test_malef_surfaces_getCharError ( void ) ;

static char*
test_malfe_surfaces_getStrError ( void ) ;


/* *** Copy, Resize and Compare *** */
static char*
test_malef_surfaces_copy ( void ) ;

static char*
test_malef_surfaces_compare ( void ) ;

static char*
test_malef_surfaces_resize ( void ) 

static char*
test_malef_surfaces_resizeRangeError ( void ) ;


/* *** Clear Surface *** */
static char*
test_malef_surfaces_clear ( void ) ;


void
test_malef_surfaces_main ( void ) ;

#endif//MALEF_TEST_MALEF_SURFACES_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
