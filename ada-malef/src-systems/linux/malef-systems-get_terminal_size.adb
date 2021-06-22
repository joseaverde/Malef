-------------------------------------------------------------------------------
--                                                                           --
--   M A L E F - S Y S T E M S - G E T _ T E R M I N A L _ S I Z E . A D B   --
--                               ( L I N U X )                               --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2021 José Antonio Verde Jiménez All Rights Reserved     --
-------------------------------------------------------------------------------
-- This file is part of Malef.                                               --
--                                                                           --
-- This program is free software:  you  can redistribute it and/or modify it --
-- under  the terms  of the  GNU  General License  as published by the  Free --
-- Software  Foundation,  either  version 3  of  the  License,  or  (at your --
-- opinion) any later version.                                               --
--                                                                           --
-- This  program  is distributed  in the  hope that  it will be  useful, but --
-- WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.                                          --
--                                                                           --
-- You should have received  a copy of the  GNU General Public License along --
-- with this program. If not, see <https://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Interfaces.C;

separate (Malef.Systems)
   procedure Get_Terminal_Size (
      Rows : out Row_Type;
      Cols : out Col_Type)
   is
      --
      -- This is the `winsize' type found in <sys/ioctl.h> header from the C
      -- programming language. It's used to store the terminal size.
      --
      -- @field ws_row
      -- The number of rows the terminal has.
      --
      -- @field ws_col
      -- The number of columns the terminal has.
      --
      type winsize is
         record
            -- The number of rows the terminal has.
            ws_row    : Interfaces.C.unsigned_short;
            ws_col    : Interfaces.C.unsigned_short;
            ws_xpixel : Interfaces.C.unsigned_short;
            ws_ypixel : Interfaces.C.unsigned_short;
         end record
      with Convention => C;

      --
      -- The Ioctl function is used to get the size of the terminal. It's
      -- imported from C and it isn't available on Windows, that's why we have
      -- a separate package.
      --
      -- @param Fd
      -- It's the file descriptor: 1 is for standard output.
      --
      -- @param Request
      -- The request we are asking to IOCTL.
      --
      -- @param Struct
      -- The struct where the information will be retrieved.
      --
      function Ioctl (Fd      : Interfaces.C.int;
                      Request : Interfaces.C.unsigned_long;
                      Struct  : out Winsize)
                      return Interfaces.C.int;
      pragma Import (C, Ioctl, "ioctl");

      TIOCGWINSZ : constant Interfaces.C.unsigned_long := 16#5413#;

      Ws   : Winsize;
      Temp : Interfaces.C.int;
   begin

      Temp := Ioctl (Fd      => 1,
                     Request => TIOCGWINSZ,
                     Struct  => Ws);

      Rows := Row_Type(Ws.ws_row);
      Cols  := Col_Type(Ws.ws_col);

   end Get_Terminal_Size;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
