-------------------------------------------------------------------------------
--                                                                           --
--                  C _ M A L E F - S U R F A C E S . A D B                  --
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


package body C_Malef.Surfaces is

   function Create (Rows : Row_Type;
                    Cols : Col_Type)
                    return Surface_Type is
   begin

      return New_Surface : Surface_Type
      do
         New_Surface.Object := Malef.Surfaces.Create(
            Rows => Malef.Row_Type(Rows),
            Cols => Malef.Col_Type(Cols));
      end return;

   end Create;


   procedure Destroy (Object : Surface_Type) is
   begin

      -- TODO: Unreference it, maybe pragma Export under Malef.Surfaces and
      --       pragma Import here.
      -- Unreference(Object);
      null;

   end Destroy;


   procedure Debug_Put (Object : Surface_Type) is
   begin

      Object.Object.Debug_Put;

   end Debug_Put;


   function Get_Null_Surface return Surface_Type is
   begin

      -- Due to the Object been a controlled type, trying to finalize the last
      -- Shared_Surface_Type it was pointing to will yield to a very nasty
      -- error, thus we must assing it without raising the attention of the
      -- Controlled types.
      return Surface_Type'(Object => Malef.Surfaces.Null_Surface);

   end Get_Null_Surface;

end C_Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
