/*****************************************************************************\
 *                                                                           *
 *            _ M A L E F - C O N S O L E _ I O - C O M M O N . C            *
 *                                                                           *
 *                                 M A L E F                                 *
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
#include <stdbool.h>
#include <iso646.h>

#define CONCAT(x,y) x##y
#define MALEF(name) CONCAT(___c__malef__console_io__common__,name)

static struct termios old_termios;
static bool initialized = false;
unsigned char MALEF(eof_ch) = 0x04;

void MALEF(initialize) (void) {

  if (initialized) { return; }

  struct termios new_termios;
  tcgetattr (STDIN_FILENO, &old_termios);
  tcgetattr (STDIN_FILENO, &new_termios);

  new_termios.c_lflag &= ~ICANON & ~ECHO;
  MALEF(eof_ch) = new_termios.c_cc[VEOF];
  new_termios.c_cc[VMIN] = 1;
  new_termios.c_cc[VTIME] = 0;
  tcsetattr (STDIN_FILENO, TCSANOW, &new_termios);

  initialized = true;
}

void MALEF(finalize) (void) {

  if (not initialized) { return; }

  tcsetattr (STDIN_FILENO, TCSANOW, &old_termios);

  initialized = false;
}
