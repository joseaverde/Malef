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


/*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*\
 *|||||||||||||||||||||||||||||||| T Y P E S ||||||||||||||||||||||||||||||||*|
\*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*/

/*
 * Due to the lack of Error handling systems in C, this type is returned by
 * (almost) every function in the Malef library to report the exceptions raised
 * in the libray internals. You should get the return value of every function
 * that possibly raises an error to handle it.
 *
 * @value malef_ADA_ERROR
 * This problem is not raised by the library itself, it is an unhandled error.
 * If you come up with any error like this, please report it so we can fix it.
 *
 * @value malef_NO_ERROR
 * No problem, you can continue with the development of you programme because
 * there has been no errors what-so-ever.
 *
 * @value malef_INITIALIZATION_ERROR
 * This error is raised every time you try to use an unitialized type or any
 * library functionality that must be initialized before use.
 *
 * @value malef_BOUNDS_ERROR
 * This error is raised when you try to access a index out of the bounds of
 * anything. In C nothing is raised (maybe a SegFault) but in Ada it's a very
 * serious exception.
 *
 * @value malef_NULL_SURFACE_ERROR
 * This error is raised when you try to modify a null surface, null surfaces
 * can be read, though. It's mandatory to initialize every surface type with a
 * null surface every time you declare it, that way you can be sure that no
 * nasty errors will be raised.
 */
typedef enum _malef_error_t {
   malef_ADA_ERROR            = -1,
   malef_NO_ERROR             =  0,
   malef_INITIALIZATION_ERROR =  1,
   malef_BOUNDS_ERROR         =  2,
   malef_NULL_SURFACE_ERROR   =  3
} malef_error_t;


/*
 * This is the colour type used in Ada. Colour types consist of an array of
 * 4 unsigned 8-bit integers which represent RGBA which stands for Red Green
 * Blue and Alpha. Every colour can be made with a convination of Red, Green
 * and Blue, if you didn't know that, make some research on wikipedia about
 * RGB. The Alpha is the opacity, 255 is fully opaque and 0 is completely
 * transparent. The Alpha component is used to add transparency between
 * layers and NOT to add transparecy for the Terminal itslef.
 * There are special colours:
 * 
 *  * If Alpha is completely 0 once every layer has been merged, then if the
 *    terminal originaly had transparency, this is recovered.
 *
 *  * You can specify the default colours of the terminal (they are usualy
 *    determined at initialization, thus it's imposible to change them before
 *    initialization). In case the terminal has no RGB colour support, then the
 *    most similar colours are chosen. So just use the palettes if you want
 *    your programme to be portable.
 */
typedef uint8_t malef_color_t[4];


/*
 * These are the colours of the palette, in practice you can put the red where
 * the blue goes, these are only names to call each of the colours of the
 * palette.
 */
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


/*
 * This is the palette, the palette is formed by two arrays of the elements
 * of the palette. The first index represents the brightness: `false' or `0'
 * for normal brightness and `true' or `1' for a brighter colour.
 * Note: As said above, you can use the colours you want for each of the
 * palette components. There are some predefined palettes too, see functions.
 */
typedef malef_color_t malef_palette_t[2][8];


/*
 * These are the predefined palettes, so you don't have to search any more for
 * the palette of the system you are writting the programme for. The first
 * palette is the default one, but on initialization it changes (so it's not
 * the default one). I think the names are very self-explanatory, otherwise
 * search it on the internet.
 */
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


/*
 * Styles are used to change how the text is displayed. Not all of them are
 * supported in every system, so this library aims to replicate them as best as
 * possible.
 */
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

/*
 * This is an array of styles, if true means the style is set, otherwise it
 * isn't. Easy, isn't it?
 */
typedef bool malef_style_arr[10];


/*
 * These types are used to count rows and columns.
 * XXX: WARNING: VALUE ZERO IS NOT ALLOWED!!!!!
 * We start counting from 1 in surfaces, so get used to it.
 */
typedef uint16_t malef_row_t, malef_col_t;


/*
 * Cursors are just a vector.
 *
 * @field row
 * The row the cursor is at.
 *
 * @field col
 * The column the cursor is at.
 */
typedef struct _malef_cursor_t {
   malef_row_t row;
   malef_col_t col;
} malef_cursor_t;



/*
 * This is character type in this library, it's UNICODE!!! In order to make
 * this library available in all systems as possible the characters will be
 * Unicode characters. That way the internals will transform the unicode
 * characters into the needed encoding.
 */
typedef uint8_t *malef_char_t[4];


/*
 * Just a string type, an array of malef_char_t.
 */
typedef malef_char_t *malef_str_t;


/*
 * This is the most important type in the library, it's the surface. It acts
 * similarly to a sprite in graphic libraries. In Ada it has garbage collection
 * I will try to make it happen for C, or add some kind of deallocator for it.
 * TODO: Add deallocator.
 */
typedef struct _malef_surface_t {
   void* object;
} malef_surface_t;




/*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*\
 *|||||||||||||||||||||||||||| F U N C T I O N S ||||||||||||||||||||||||||||*|
\*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*/

    ///////////////////////////////////////////////////////////////////////
    //---------- ERROR FUNCTIONS ----------------------------------------//
    ///////////////////////////////////////////////////////////////////////

/*
 * This function `catches' an error, i.e. tells the internals of the library
 * that the error has been received and that it can `release' it. It's not
 * necessary to call this function, because when a new error is raised, the
 * last one is removed. But if you don't remove it, the `malef_isFlyingError'
 * function will return true.
 */
extern void
malef_catchError (void);

/*
 * This function checks whether an error has been `thrown' and is flying
 * somewhere waiting to be `catched' by `malef_catchError'.
 *
 * @return
 * It returns whether a an error has been thrown.
 */
extern bool 
malef_isFlyingError (void);

/*
 * This function gets the last error name and returns it. Keep in mind you
 * have to FREE the string after using it.
 *
 * @return
 * A pointer to an allocated string with the name of the error.
 */
extern char*
malef_getErrorName (void);

/*
 * This function gets the last error message describing the `thrown' error and
 * returns it. Keep in mind you have to FREE the string after using it.
 *
 * @return
 * A pointer to an allocated string with the message of the error.
 */
extern char*
malef_getErrorMessage (void);



    ///////////////////////////////////////////////////////////////////////
    //---------- MALEF FUNCTIONS ----------------------------------------//
    ///////////////////////////////////////////////////////////////////////

extern malef_error_t
malef_initialize (void);

extern malef_error_t
malef_finalize (void);

extern bool
malef_isInitialized (void);

extern malef_error_t
malef_getHeight (malef_row_t* height); /*OUT*/

extern malef_error_t
malef_getWidth (malef_col_t* width); /*OUT*/

extern malef_error_t
malef_newPage (void);

extern malef_error_t
malef_setTitle (const char* titleName); /*IN*/

extern malef_error_t
malef_updateTerminalSize (bool* is_updated); /*OUT*/

extern malef_error_t
malef_wrapper (void (*function)(void*),
               void*  params,   /*IN*/
               void*  ret_val); /*OUT*/



    ///////////////////////////////////////////////////////////////////////
    //---------- COLOUR FUNCTIONS ---------------------------------------//
    ///////////////////////////////////////////////////////////////////////


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
