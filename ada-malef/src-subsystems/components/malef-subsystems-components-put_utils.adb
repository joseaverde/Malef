-------------------------------------------------------------------------------
--                                                                           --
-- MALEF- S U B S Y S T E M S - C O M P O N E N T S - P U T _ U T I L S .ADB --
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

package body Malef.Subsystems.Components.Put_Utils is

   procedure Get_Bounds (Object : Shared_Surface_Access;
      In_Row     : out Row_Type;
      In_Col     : out Col_Type;
      From_Row   : out Row_Type;
      From_Col   : out Col_Type;
      The_Height : out Row_Type;
      The_Width  : out Col_Type) is
   begin

      -- We first define the bounds and calculate the offset to print it.
      if Object.Position.Row + Row_Coord (Object.Height) < 1 or   -- Too up
         Object.Position.Col + Col_Coord (Object.Width)  < 1 or   -- Too left
         abs Object.Position.Row > Row_Coord (Height)        or   -- Too down
         abs Object.Position.Col > Col_Coord (Width)              -- Too right
      then
         -- Nothing to be done.
         raise Pass;
      end if;

      -- Otherwise it's safe to define the bounds.
      -- If it starts out of bounds we let it start at the begining, we've
      -- already checked the bounds.
      In_Row := Row_Type
         (if Object.Position.Row < 1 then 1 else Object.Position.Row);
      In_Col := Col_Type
         (if Object.Position.Col < 1 then 1 else Object.Position.Col);
      From_Row := Row_Type
         (if Object.Position.Row < 0 then abs Object.Position.Row else 1);
      From_Col := Col_Type
         (if Object.Position.Col < 0 then abs Object.Position.Col else 1);
      The_Height := Row_Type'Min
         (Height, Row_Type(Row_Coord(Object.Height) + Object.Position.Row))-1;
      The_Width := Col_Type'Min
         (Width, Col_Type(Col_Coord(Object.Width) + Object.Position.Col))-1;

   -- FOR DEBUGGING PURPOSES
   -- Std_Out.Write ("In_Row =" & In_Row'Image & ASCII.LF);
   -- Std_Out.Write ("In_Col =" & In_Col'Image & ASCII.LF);
   -- Std_Out.Write ("From_Row =" & From_Row'Image & ASCII.LF);
   -- Std_Out.Write ("From_Col =" & From_Col'Image & ASCII.LF);
   -- Std_Out.Write ("The_Height =" & The_Height'Image & ASCII.LF);
   -- Std_Out.Write ("The_Width =" & The_Width'Image & ASCII.LF);
   -- Std_Out.Write ("Height =" & Object.Height'Image & ASCII.LF);
   -- Std_Out.Dump;

   end Get_Bounds;

end Malef.Subsystems.Components.Put_Utils;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
