/*****************************************************************************\
 *                                                                           * 
 *                M A L E F - W I N D O W S - D R I V E R . C                * 
 *                                                                           * 
 *                                 M A L E F                                 * 
 *                                                                           * 
 *                              C   S O U R C E                              * 
 *                                                                           * 
 *---------------------------------------------------------------------------* 
 *     Copyright (c) 2020 José Antonio Verde Jiménez All Rights Reserved     * 
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

/*                                                                           *\
 * This file is used to work with some functions from the <windows.h> API,   *
 * I'm not going to interface directly with Ada without a layer because the  *
 * Windows API's types are hidden in a mess of library and may change in a   *
 * future, so I'm won't risk and interface.                                  *
\*                                                                           */

// We check if we are compiling it for Windows or for other operating system
// that way we can leave a empty implementation in other operating systems.
#ifdef MALEF_COMPILE_FOR_WINDOWS

#include <windows.h>

#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif//ENABLE_VIRTUAL_TERMINAL_PROCESSING

// We assign the standard output handle to a static variable so we don't have
// to call a function every time we need it.
static HANDLE stdoutHandle;

// This variable contains the console's mode before the initialization.
static DWORD  outModeInit;

// This function sets up the console.
void _malef_setupConsole ( void ) {

   DWORD outMode = 0;
   stdoutHandle = GetStdHandle ( STD_OUTPUT_HANDLE );

   if ( stdoutHandle == INVALID_HANDLE_VALUE ) {
      exit ( GetLastError() );
   }

   if ( ! GetConsoleMode ( stdoutHandle, &outMode ) ) {
      exit ( GetLastError() );
   }

   outModeInit = outMode;
   outMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;

   if ( ! SetConsoleMode ( stdoutHandle, outMode ) ) {
      exit ( GetLastError() );
   }

}


// This function restores the console.
void _malef_restoreConsole ( void ) {

   if ( ! SetConsoleMode ( stdoutHandle, outModeInit ) ) {
      exit ( GetLastError() );
   }

}


// This function returns the number of rows and columns the terminal has got.
void _malef_getConsoleScreenSize ( short *rows, short *cols ) {

   CONSOLE_SCREEN_BUFFER_INFO csbi;

   GetConsoleScreenBufferInfo ( stdoutHandle, &csbi );

   *rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
   *cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;

}


// This function changes the title of the console.
void _malef_setConsoleTitle (const char* title) {

   SetConsoleTitle ( title );

}


#else


void _malef_setupConsole ( void ) {
   return;
}


void _malef_restoreConsole ( void ) {
   return;
}


void _malef_getConsoleScreenSize ( short *rows, short *cols ) {

   *rows = 24;
   *cols = 80;

}


void _malef_setConsoleTitle ( const char* title ){

   return;

}


#endif//MALEF_COMPILE_FOR_WINDOWS

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
