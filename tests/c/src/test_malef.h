/*****************************************************************************\
 *                                                                           * 
 *                          T E S T _ M A L E F . H                          * 
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

#ifndef MALEF_TEST_MALEF_H
#define MALEF_TEST_MALEF_H

static char* 
test_malef_initialize ( void ) ;

static char*
test_malef_finalize ( void ) ;

static char*
test_malef_isInitialized ( void ) ;

static char*
test_malef_getHeight ( void ) ;

static char*
test_malef_getWidth ( void ) ;

static char*
test_malef_newPage ( void ) ;

static char*
test_malef_setTitle ( void ) ;


void
test_malef_main ( void ) ;

#endif//MALEF_TEST_MALEF_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
