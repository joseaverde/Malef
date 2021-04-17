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
 * No problem, you can continue with the development of you program because
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
} malef_error_t ;


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
 *    your program to be portable.
 */
typedef uint8_t malef_color_t[4] ;


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
} malef_colorKind_t ;


/*
 * This is the palette, the palette is formed by two arrays of the elements
 * of the palette. The first index represents the brightness: `false' or `0'
 * for normal brightness and `true' or `1' for a brighter colour.
 * Note: As said above, you can use the colours you want for each of the
 * palette components. There are some predefined palettes too, see functions.
 */
typedef malef_color_t malef_palette_t[2][8] ;


/*
 * These are the predefined palettes, so you don't have to search any more for
 * the palette of the system you are writting the program for. The first
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
} malef_paletteKind_t ;


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
} malef_style_t ;

/*
 * This is an array of styles, if true means the style is set, otherwise it
 * isn't. Easy, isn't it?
 */
typedef bool malef_style_arr[10] ;


/*
 * These types are used to count rows and columns.
 * XXX: WARNING: VALUE ZERO IS NOT ALLOWED!!!!!
 * We start counting from 1 in surfaces, so get used to it.
 */
typedef uint16_t malef_row_t, malef_col_t ;

/*
 * These types are used opposite to malef_row_t or malef_col_t to tell where
 * an object is placed. We also start counting from 1, so the top left corner
 * is the position (Row=1, Col=1).
 */
typedef int16_t malef_rowCord_t, malef_colCord_t ;


/*
 * Cursors are just a vector that can be used to tell a position in the visible
 * part of the terminal or inside a surface.
 *
 * @field row
 * The row the cursor is at.
 *
 * @field col
 * The column the cursor is at.
 */
typedef struct _malef_cursor_t {
   malef_row_t row ;
   malef_col_t col ;
} malef_cursor_t ;


/*
 * The Coord type is similar to the Cursor type, however this can also be used
 * to tell a position even out of the terminal's bounds. It's only a 16-bit
 * signed integer because no more is needed.
 *
 * @field row
 * The row where it is.
 *
 * @field col
 * The row where it is.
 */
typedef struct _malef_coord_t {
   malef_rowCord_t row ;
   malef_colCord_t col ;
} malef_coord_t ;


/*
 * This is character type in this library, it's UNICODE!!! In order to make
 * this library available in all systems as possible the characters will be
 * Unicode characters. That way the internals will transform the unicode
 * characters into the needed encoding.
 */
typedef uint32_t malef_char_t ;


/*
 * Just a string type, an array of malef_char_t.
 */
typedef malef_char_t *malef_str_t ;


/*
 * This is the most important type in the library, it's the surface. It acts
 * similarly to a sprite in graphic libraries. In Ada it has garbage collection
 * I will try to make it happen for C, or add some kind of deallocator for it.
 */
typedef struct _malef_surface_t {
   void* object ;
} malef_surface_t ;



/*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*\
 *|||||||||||||||||||||||||||| F U N C T I O N S ||||||||||||||||||||||||||||*|
\*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*/


// These are the parameters in the functions, this only tells the user how to
// pass the parameters when calling those functions
// TODO: Add warning.
#  ifdef out
#     define _malef_temp_out out
#     define out * /*out*/
#  else
#     define out * /*out*/
#  endif

#  ifdef in
#     define _malef_temp_in in
#     define in /*in*/
#  else
#     define in /*in*/
#  endif


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
malef_catchError ( void ) ;

/*
 * This function checks whether an error has been `thrown' and is flying
 * somewhere waiting to be `catched' by `malef_catchError'.
 *
 * @return
 * It returns whether a an error has been thrown.
 */
extern bool
malef_isFlyingError ( void ) ;

/*
 * This function gets the last error name and returns it. Keep in mind you
 * have to FREE the string after using it.
 *
 * @return
 * A pointer to an allocated string with the name of the error.
 */
extern char*
malef_getErrorName ( void ) ;

/*
 * This function gets the last error message describing the `thrown' error and
 * returns it. Keep in mind you have to FREE the string after using it.
 *
 * @return
 * A pointer to an allocated string with the message of the error.
 */
extern char*
malef_getErrorMessage ( void ) ;



    ///////////////////////////////////////////////////////////////////////
    //---------- MALEF FUNCTIONS ----------------------------------------//
    ///////////////////////////////////////////////////////////////////////

/*
 * This function initializes the library, it can only be initialized once, if
 * you want to initialize it again you must finalize it first. You can work
 * with surfaces and types before initialization but you can't do certain IO
 * functions that require it to be initialized. Most of the following functions
 * requiere initialization.
 *
 * @return
 *  - malef_INITIALIZATION_ERROR: If there was any error during the
 *                                initialization of the library or if it was
 *                                already initialized.
 */
extern malef_error_t
malef_initialize ( void );

/*
 * This function finalizes the library and everything. It must have been
 * initialized before finalization.
 *
 * @return
 *  - malef_INITIALIZATION_ERROR: If there was any problem during the
 *                                finalization of the library or if it wasn't
 *                                initialized before calling the function.
 */
extern malef_error_t
malef_finalize ( void ) ;

/*
 * This function doesn't raise any error and doesn't requiere the library to be
 * initialized. It returns whether the library has been initialized or not.
 *
 * @return
 * Whether it has been initialized or not.
 */
extern bool
malef_isInitialized ( void ) ;

/*
 * This function returns (to the parameter) the height of the current terminal
 * or console, but it needs the terminal to be initialized. If there was any
 * errors, the parameter isn't modified.
 *
 * @param height
 * (OUT) The height of the terminal.
 *
 * @return
 *  - malef_INITIALIZATION_ERROR: This exception is raised if the library was
 *                                not initialized.
 */
extern malef_error_t
malef_getHeight ( malef_row_t out height ) ;

/*
 * This function returns (to the parameter) the width of the current terminal
 * or console, but it needs it to be initialized first, otherwise the parameter
 * doesn't change.
 *
 * @param width
 * (OUT) The width of the terminal.
 *
 * @return
 *  - malef_INITIALIZATION_ERROR: This exception is raised if the library was
 *                                not initialized.
 */
extern malef_error_t
malef_getWidth ( malef_col_t out width ) ;

/*
 * This function moves everything there was on the terminal before execution
 * up so that the user can work on a free space.
 *
 * @return
 *  - malef_INITIALIZATION_ERROR: This exception is raised if the library was
 *                                not initialized.
 */
extern malef_error_t
malef_newPage ( void ) ;

/*
 * This function changes the title of the terminal, it's imposible to retrieve
 * it and it will be set this title during execution (even if the library is
 * finalized). TODO: Find a way to recover it.
 *
 * @param titleName
 * (IN) The new title.
 *
 * @return
 *  - malef_INITIALIZATION_ERROR: This exception is raised if the library was
 *                                not initialized.
 */
extern malef_error_t
malef_setTitle ( const char* in titleName ) ;

/*
 * This function updates the terminal size and returns whether it has changed,
 * in some systems like linux this will be done automatically by signals, so it
 * will always return false. However in Windows, (for now XXX) you have to call
 * it to check if it has changed.
 *
 * @param is_updated
 * (OUT) Whether the terminal size has been updated.
 *
 * @return
 *  - malef_INITIALIZATION_ERROR: The library must be initialized to call this
 *                                function.
 */
extern malef_error_t
malef_updateTerminalSize ( bool out is_updated ) ;

/*
 * This is a wrapper, you can give your function and parameters and be sure
 * that the finalization and initialization functions will be called
 * automatically. And any unhandled exception (by the library) will be caught
 * and returned. This way you can be sure that the terminal recovers after
 * finalization.
 *
 * @param function
 * The function you want to call, it must have at least one parameter that will
 * be passed automatically to the function.
 *
 * @param params
 * (IN) The parameters you want to pass to the function. This might be a
 *      pointer to a struct with all the parameters.
 *
 * @param ret_val
 * (OUT) This is the return value of your function, if your function returns
 *       nothing just return NULL.
 */
extern malef_error_t
malef_wrapper ( void* (*function)(void*),
                void* in  params,
                void  out ret_val ) ;



    ///////////////////////////////////////////////////////////////////////
    //---------- SURFACES FUNCTIONS -------------------------------------//
    ///////////////////////////////////////////////////////////////////////


/*
 * This function is used to assign one surface to another. You should do it
 * this way so the internal components can keep track of the surfaces and
 * perfom automatic garbage collection when a surface is destroyed without
 * freeing a surface which is referenced by many other surfaces.
 *
 * @param surface
 * (IN)  The surface you want to assign to another surface, this is the one
 *       that will be modified, keep in mind it should be either a null surface
 *       or an initialized surface.
 *
 * @param to_surface
 * (IN)  This is the surface you want your surface to assigned to. This one
 *       also shouldn't be an uninitialized surface.
 *
 * @return
 * It returns the error code, it will return very nasty errors if you try to
 * use any uninitialized surface.
 */
extern malef_error_t
malef_assignSurface ( malef_surface_t in surface,
                      malef_surface_t in to_surface ) ;


/*
 * This function is used to create a surface. Sizes lower than 1 aren't
 * allowed due to Ada's type checking.
 *
 * @param rows
 * (IN)  The number of rows (the height) of the new surface.
 *
 * @param rows
 * (IN)  The number of columns (the width) of the new surface.
 *
 * @param surface
 * (IN)  The null or initialized surface variable where you want to place the
 *       new created surface.
 */
extern malef_error_t
malef_createSurface ( malef_row_t     in rows,
                      malef_col_t     in cols,
                      malef_surface_t in surface ) ;


/*
 * This function is used to finalize a surface, keep in mind that if the
 * surface it's still being used by other structure or object, it won't be
 * freed, i.e.:
 *    | If you create a surface
 *    |     malef_declareSurface(my_surf) ;
 *    |     // Do something with your surface.
 *    |     your_surf = my_surf
 *    | If you destroy `your_surf', `my_surf' will be freed.
 *    | However, if you use any other structure like (TODO: coming soon)
 *    |     malef_appendBox ( my_box, my_surf ) ;
 *    |     malef_destroy ( my_surf ) ;
 *    | The surface won't be freed, because the Box still has access to it.
 *    | In orther to assing to another surface use the `malef_assignSurface'
 *    | function.
 *
 * @param surface
 * (IN)  The surface to destroy.
 */
extern malef_error_t
malef_destroySurface ( malef_surface_t in surface ) ;


/*
 * DEBUGGING!
 */
extern void
_malef_debugPutSurface ( malef_surface_t surface ) ;


/*
 * This function returns a null surface, remember to initialize all your
 * surfaces with this before even using them.
 *
 * @return
 * A null surface.
 */
extern malef_surface_t
malef_getNullSurface ( void ) ;

/*
 * This is basically a short-cut so you don't remember to initialize always a
 * surface with this.
 */
#define malef_declareSurface(name) malef_surface_t name = \
                                    malef_getNullSurface ()



    ///////////////////////////////////////////////////////////////////////
    //---------- COLOUR FUNCTIONS ---------------------------------------//
    ///////////////////////////////////////////////////////////////////////

/*
 * This function gets the foreground colour from a certain position of
 * the Surface.
 *
 * @param surface
 * (IN)  The surface where we want to get the colour.
 *
 * @param row
 * (IN)  The row from where we want to retrieve the colour. Keep in mind, in
 *       this whole library we start counting at one, so if you try to use any
 *       other number a very nasty error will be raised.
 *
 * @param col
 * (IN)  The column from where we want to retrieve the colour. It's a positive
 *       integer bigger than zero, keep that in mind.
 *
 * @param color
 * (OUT) The foreground colour in the given position.
 *
 * @return
 *  - malef_BOUNDS_ERROR: The position is out of bounds.
 */
extern malef_error_t
malef_getSurfaceForeground ( malef_surface_t in  surface,
                             malef_row_t     in  row,
                             malef_col_t     in  col,
                             malef_color_t   out color ) ;

/*
 * This function gets the background colour from a certain position of the
 * Surface.
 *
 * @param surface
 * (IN)  The surface where we want to get the colour.
 *
 * @param row
 * (IN)  The row from where we want to retrieve the colour. Keep in mind, in
 *       this whole library we start counting at one, so if you try to use any
 *       other number a very nasty error will be raised.
 *
 * @param col
 * (IN)  The column from where we want to retrieve the colour. It's a positive
 *       integer bigger than zero, keep that in mind.
 *
 * @param color
 * (OUT) The background colour in the given position.
 *
 * @return
 *  - malef_BOUNDS_ERROR: The position is out of bounds.
 */
extern malef_error_t
malef_getSurfaceBackground ( malef_surface_t in  surface,
                             malef_row_t     in  row,
                             malef_col_t     in  col,
                             malef_color_t   out color ) ;

/*
 * This function changes the foreground colour in a given block.
 *
 * @param surface
 * (IN)  The surface to edit.
 *
 * @param from_row
 * (IN)  The block's starting row position.
 *
 * @param to_row
 * (IN)  The block's ending row position.
 *
 * @param from_col
 * (IN)  The block's starting column position.
 *
 * @param to_col
 * (IN)  The block's ending column position.
 *
 * @param color
 * (IN)  The colour to dye the given position with.
 *
 * @return
 *  - malef_BOUNDS_ERROR: Any of the positions is out of bounds or the range
 *                        is negative.
 *  - malef_NULL_SURFACE_ERROR: You are trying to modify a null surface which
 *                              is not possible.
 */
extern malef_error_t
malef_setSurfaceForeground ( malef_surface_t in surface,
                             malef_row_t     in from_row,
                             malef_row_t     in to_row,
                             malef_col_t     in from_col,
                             malef_col_t     in to_col,
                             malef_color_t   in color ) ;

/*
 * This function changes the background colour in a given block.
 *
 * @param surface
 * (IN)  The surface to edit.
 *
 * @param from_row
 * (IN)  The block's starting row position.
 *
 * @param to_row
 * (IN)  The block's ending row position.
 *
 * @param from_col
 * (IN)  The block's starting column position.
 *
 * @param to_col
 * (IN)  The block's ending column position.
 *
 * @param color
 * (IN)  The colour to dye the given position with.
 *
 * @return
 *  - malef_BOUNDS_ERROR: Any of the positions is out of bounds or the range
 *                        is negative.
 *  - malef_NULL_SURFACE_ERROR: You are trying to modify a null surface which
 *                              is not possible.
 */
extern malef_error_t
malef_setSurfaceBackground ( malef_surface_t in surface,
                             malef_row_t     in from_row,
                             malef_row_t     in to_row,
                             malef_col_t     in from_col,
                             malef_col_t     in to_col,
                             malef_color_t   in color ) ;

/*
 * This function gets the default Surface's writing colour. This is the colour
 * that will be used when writing a string text into the Surface by default.
 *
 * @param surface
 * (IN)  The surface from which you want to get the default writing colour.
 *
 * @param color
 * (OUT) The surface's default writing colour.
 *
 * @return
 * It usually returns no errors, if it returns any, it is fatal one.
 */
extern malef_error_t
malef_getCursorForeground ( malef_surface_t in  surface,
                            malef_color_t   out color ) ;

/*
 * This function gets the default Surface's writing colour. This is the colour
 * that will be used when writing a string text into the Surface by default.
 *
 * @param surface
 * (IN)  The surface from which you want to get the default writing colour.
 *
 * @param color
 * (OUT) The surface's default writing colour.
 *
 * @return
 * It usually returns no errors, if it returns any, it is fatal one.
 */
extern malef_error_t
malef_getCursorBackground ( malef_surface_t in  surface,
                            malef_color_t   out color ) ;

/*
 * This function changes the default Surface's writing foreground colour. You
 * can even change it in a null surface because it won't have any effect on it
 * because it can't be written.
 *
 * @param surface
 * (IN)  The surface whose default cursor colour you want to change.
 *
 * @param color
 * (IN)  The colour.
 *
 * @return
 * It won't return any error, but you can always check it if anything goes
 * wrong unexpectedly.
 */
extern malef_error_t
malef_setCursorForeground ( malef_surface_t in surface,
                            malef_color_t   in color ) ;

/*
 * This function changes the default Surface's writing background colour. The
 * null surfaces can also be changed because it won't have any effect at all.
 *
 * @param surface
 * (IN)  The surface whose default background cursor colour you want to change.
 *
 * @param color
 * (IN)  The new colour.
 *
 * @return
 * It shouldn't return any errors, but check it if you can because if any error
 * is raised it must be a fatal one.
 */
extern malef_error_t
malef_setCursorBackground ( malef_surface_t in surface,
                            malef_color_t   in color ) ;

/*
 * This function gets the current surface palette.
 *
 * @param palette
 * (OUT) This is the current palette that is being used by the internal engine.
 *
 * @return
 *  - malef_INITIALIZATION_ERROR: This error is returned if the library hasn't
 *                                been initialized yet.
 */
extern malef_error_t
malef_getPalette ( malef_palette_t out palette ) ;

/*
 * This function gets a palette of a given palette name.
 *
 * @param palette_kind
 * (IN)  This is the palette kind whose palette you want to get.
 *
 * @param palette
 * (OUT) This is the palette where the new palette will be stored.
 *
 * @return
 * It shouldn't return any error if you give the palette_kind in its range.
 */
extern malef_error_t
malef_getPaletteKind ( malef_paletteKind_t in  palette_kind,
                       malef_palette_t     out palette ) ;

/*
 * This function changes the current palette to another one, you can change it
 * before initialization but keep in mind that it will be overwritten after
 * initialization for the most suitable one for the operating system.
 *
 * @param palette
 * (IN)  The palette you want to set as default.
 *
 * @return
 * It shouldn't return any error if the palettes aren't corrupted or anything.
 */
extern malef_error_t
malef_setPalette ( malef_palette_t in palette ) ;

/*
 * This function changes the current Malef palette to another one, you can
 * change it before initialization but it won't have any effects because during
 * the initialization process the most suitable one is chose depending on the
 * operating system and the terminal/console emulator.
 *
 * @param palette_kind
 * (IN)  The palette you want to set as default.
 *
 * @return
 * It shouldn't return any error, if it does report it immediately.
 */
extern malef_error_t
malef_setPaletteKind ( malef_paletteKind_t in palette_kind ) ;

/*
 * This function returns the colour of the current palette using the
 * enumeration value and the brightness level. This avoids having to store the
 * palette in a variable and ensures that the returned colour is the one from
 * the palette in use.
 *
 * @param color
 * (OUT) The colour from the palette.
 *
 * @param color_kind
 * (IN)  The colour enumeration value.
 *
 * @param bright
 * (IN)  The brightness level, true is bright and false is the normal colour.
 *
 * @return
 * It shouldn't return any error if the colours are given in the specified
 * range and not random colours such as 17.
 */
extern malef_error_t
malef_getColor ( malef_color_t     out color,
                 malef_colorKind_t in  color_kind,
                 bool              in  bright );


#  ifdef _malef_temp_out
#     define out _malef_temp_out
#  else
#     undef out
#  endif

#  ifdef _malef_temp_in
#     define in _malef_temp_in
#  else
#     undef in
#  endif

#endif//C_MALEF_H

///=======================/////////////////////////=========================///
//=======================// E N D   O F   F I L E //=========================//
///=======================/////////////////////////=========================///
