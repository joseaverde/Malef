/*****************************************************************************\
 *                                                                           * 
 *       M A L E F - S U B S Y S T E M S - C M D - C _ D R I V E R . C       * 
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
 * under  the termsI  of the  GNU  General License  as published by the  Free * 
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

#include <windows.h>


static HANDLE stdoutHandle ;


void
_malefCMD_initialize ( void ) {

   stdoutHandle = GetStdHandle ( STD_OUTPUT_HANDLE ) ;
   // No need to check if it's valid, it should be after initialization.
}


/*
 * This function changes the Console title
 */
void
_malefCMD_setConsoleTitle ( const char *title ) {

   SetConsoleTitle ( title ) ;
}


void
_malefCMD_setPosition ( short row, short col ) {

   COORD position ;
   position.Y = row ;
   position.X = col ;

   SetConsoleCursorPosition ( stdoutHandle, position ) ;
}


void
_malefCMD_setFormat ( short format ) {

   SetConsoleTextAttribute ( stdoutHandle, format ) ;

}


///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
