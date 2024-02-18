/*****************************************************************************\
 *                                                                           *
 *                  __MALEF-PLATFORM-INPUT-IMPLEMENTATION.C                  *
 *                                                                           *
 *                                 M A L E F                                 *
 *                                                                           *
 *                                  A N S I                                  *
 *                                 P O S I X                                 *
 *                                                                           *
 *                                C   B O D Y                                *
 *                                                                           *
 *---------------------------------------------------------------------------*
 *  Copyright (c) 2021-2024 José Antonio Verde Jiménez  All Rights Reserved  *
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
\*---------------------------------------------------------------------------*/

#include <termios.h>
#include <unistd.h>

#define CONCAT(x,y)  x##y
#define MALEF(name) CONCAT(__malef__platform__terminal__input___c_,name)

static struct termios old_term;
unsigned char MALEF(eof_ch) = 0x04;

void MALEF(prepare) (void) {
  struct termios new_term;
  tcgetattr (STDIN_FILENO, &old_term);
  tcgetattr (STDIN_FILENO, &new_term);

  new_term.c_lflag &= ~ICANON & ~ECHO;

  #ifdef VEOF
    MALEF(eof_ch) = new_term.c_cc[VEOF];
  #else
    MALEF(eof_cH) = 0x04;
  #endif

  new_term.c_cc[VMIN] = 1;
  new_term.c_cc[VTIME] = 0;
  tcsetattr (STDIN_FILENO, TCSANOW, &new_term);
}

void MALEF(restore) (void) {
  tcsetattr (STDIN_FILENO, TCSANOW, &old_term);
}
