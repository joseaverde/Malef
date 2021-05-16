/*****************************************************************************\
 *                                                                           * 
 *                       M A L E F - S Y M B O L S . C                       * 
 *                                                                           * 
 *                                 M A L E F                                 * 
 *                                                                           * 
 *                              C   S O U R C E                              * 
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

/*
 * This file is created to define certain symbols, because Ada doesn't have
 * any kind of preprocessor (apart from `gnatprep', but I would like to keep
 * things simple). Ada will have access to this symbols.
 */

#include <stdint.h>

#ifndef DEBUG
#  define DEBUG            1
#endif

#ifndef MAJOR_VERSION
#  define MAJOR_VERSION    0
#endif

#ifndef MINOR_VERSION
#  define MINOR_VERSION    0
#endif

#ifndef PATCH_VERSION
#  define PATCH_VERSION    0
#endif


const uint16_t _malef_debug = DEBUG;

const uint16_t _malef_version[3] = {
   MAJOR_VERSION,
   MINOR_VERSION,
   PATCH_VERSION
};

const uint16_t _malef_version_number =
   ( MAJOR_VERSION << 10 ) | ( MINOR_VERSION << 5 ) | ( PATCH_VERSION );

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
