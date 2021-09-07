-------------------------------------------------------------------------------
--                                                                           --
--           M A L E F - S D K - M E S S A G E _ B O X E S . A D S           --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
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

--
-- @summary
--
--
-- @description
--
generic
   type Return_Type is (<>);
package Malef.SDK.Message_Boxes is

   package Message_Boxes_Widgets is new Widgets (Return_Type);

   type Message_Box_Type is
      new Message_Boxes_Widgets.Widget_Type with
      private;

   function Create (
      Message          : Str_Type;
      Foreground_Color : Color_Type;
      Background_Color : Color_Type;
      Selected_Color   : Color_Type;
      Shadow_Color     : Color_Type := Default_Shadow_Color;
      Borders          : String := "+-+| |+-+")
      return Message_Box_Type
   with Pre => Borders'Length = 9 and Borders'First = 1;

   overriding
   function Run (Message_Box : in out Message_Box_Type)
      return Return_Type;

private

   type Message_Box_Type is
      new Message_Boxes_Widgets.Widget_Type with
      record
         Message  : Str_Access;
         Position : Return_Type := Return_Type'First;
      end record;

   overriding
   procedure Update (Message_Box : in out Message_Box_Type) is null;
   procedure Finalize (Message_Box : in out Message_Box_Type);

end Malef.SDK.Message_Boxes;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
