/*****************************************************************************\
 *                                                                           * 
 *                               M A L E F . H                               * 
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

#ifndef C_MALEF_H
#define C_MALEF_H

#include <stdbool.h>
#include <stdint.h>

typedef enum _malef_error_t {
   malef_ADA_ERROR            = -1,
   malef_NO_ERROR             =  0,
   malef_INITIALIZATION_ERROR =  1,
   malef_BOUNDS_ERROR         =  2,
   malef_NULL_SURFACE_ERROR   =  3
} malef_error_t;


typedef uint8_t malef_color_t[4];

typedef enum _malef_colorKind_t {
   malef_BLACK   = 0,
   malef_RED     = 1,
   malef_GREEN   = 2,
   malef_YELLOW  = 3,
   malef_BLUE    = 4,
   malef_MAGENTA = 5,
   malef_CYAN    = 6,
   malef_WHITE   = 7
} malef_colorKind_t;

typedef malef_color_t malef_palette_t[2][8];

typedef enum _malef_paletteKind_t {
   malef_MALEF_PALETTE      = 0,
   malef_VGA                = 1,
   malef_WINDOWS_XP_CONSOLE = 2,
   malef_WINDOWS_POWERSHELL = 3,
   malef_VISUAL_STUDIO_CODE = 4,
   malef_WINDOWS_10_CONSOLE = 5,
   malef_TERMINAL_APP       = 6,
   malef_PUTTY              = 7,
   malef_MIRC               = 8,
   malef_XTERM              = 9,
   malef_UBUNTU             = 10
} malef_paletteKind_t;


typedef enum _malef_style_t {
   malef_BOLD             = 0,
   malef_FAINT            = 1,
   malef_ITALIC           = 2,
   malef_UNDERLINE        = 3,
   malef_SLOW_BLINK       = 4,
   malef_RAPID_BLINK      = 5,
   malef_REVERSE_VIDEO    = 6,
   malef_CONCEAL          = 7,
   malef_CROSSED_OUT      = 8,
   malef_DOUBLY_UNDERLINE = 9
} malef_style_t;

typedef bool malef_style_arr[10];


typedef uint16_t malef_row_t, malef_col_t;

typedef struct _malef_cursor_t {
   malef_row_t row;
   malef_col_t col;
} malef_cursor_t;


typedef uint8_t *malef_char_t[4];

typedef malef_char_t *malef_str_t;

typedef struct _malef_surface_t {
   void* object;
} malef_surface_t;


extern void  malef_catchError      (void);
extern bool  malef_isFlyingError   (void);
extern char* malef_getErrorName    (void);
extern char* malef_getErrorMessage (void);

extern malef_error_t malef_initialize         (void);
extern malef_error_t malef_finalize           (void);
extern bool          malef_isInitialized      (void);
extern malef_error_t malef_getHeight          (malef_row_t* height); /*OUT*/
extern malef_error_t malef_getWidth           (malef_col_t* width); /*OUT*/
extern malef_error_t malef_newPage            (void);
extern malef_error_t malef_setTitle           (const char* titleName); /*IN*/
extern malef_error_t malef_updateTerminalSize (bool* is_updated); /*OUT*/
extern malef_error_t malef_wrapper            (void (*function)(void*),
                                               void*  params,   /*IN*/
                                               void*  ret_val); /*OUT*/

extern malef_surface_t malef_createSurface    (malef_col_t,
                                               malef_row_t);
extern void malef_destroySurface   (malef_surface_t);
extern void _malef_debugPutSurface (malef_surface_t);

extern void malef_getSurfaceForeground (malef_surface_t,
                                        malef_row_t,
                                        malef_col_t,
                                        malef_color_t);
extern void malef_getSurfaceBackground (malef_surface_t,
                                        malef_row_t,
                                        malef_col_t,
                                        malef_color_t);
extern void malef_setSurfaceForeground (malef_surface_t,
                                        malef_row_t,
                                        malef_row_t,
                                        malef_col_t,
                                        malef_col_t,
                                        malef_color_t);
extern void malef_setSurfaceBackground (malef_surface_t,
                                        malef_row_t,
                                        malef_row_t,
                                        malef_col_t,
                                        malef_col_t,
                                        malef_color_t);
extern void malef_getCursorForeground  (malef_surface_t,
                                        malef_color_t);
extern void malef_getCursorBackground  (malef_surface_t,
                                        malef_color_t);
extern void malef_setCursorForeground  (malef_surface_t,
                                        malef_color_t);
extern void malef_setCursorBackground  (malef_surface_t,
                                        malef_color_t);
extern void malef_getPalette           (malef_palette_t);
extern void malef_getPaletteKind       (malef_paletteKind_t,
                                        malef_palette_t);
extern void malef_setPalette           (malef_palette_t);
extern void malef_setPaletteKind       (malef_paletteKind_t);

#endif//C_MALEF_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
