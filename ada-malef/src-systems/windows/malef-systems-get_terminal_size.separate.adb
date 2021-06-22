-------------------------------------------------------------------------------
--                                                                           --
-- MALEF - S Y S T E M S - G E T _ T E R M I N A L _ S I Z E . SEPARATE. ADB --
--                             ( W I N D O W S )                             --
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
      procedure C_Driver_Get_Console_Screen_Size(Rows: out Interfaces.C.short;
                                                 Cols: out Interfaces.C.short)
         with Import        => True,
              Convention    => C,
              External_Name => "_malef_getConsoleScreenSize";
      C_Rows, C_Cols : Interfaces.C.short;
   begin

      C_Driver_Get_Console_Screen_Size (Rows => C_Rows,
                                        Cols => C_Cols);

      Rows := Row_Type (C_Rows);
      Cols := Col_Type (C_Cols);

   end Get_Terminal_Size;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
