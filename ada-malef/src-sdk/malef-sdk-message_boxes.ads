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
-- This package inherits from the Widget_Type to create Message_Boxes, the only
-- parameters are the buttons (which must be an enumeration type) and a
-- function to get their image (their string form).
--
-- @description
-- As you can see there are sometimes that the Str_Type isn't used instead of
-- the default String type. That is because it's easier to work with something
-- like Boolean'Image instead of redefining the function (which you will do
-- anyways if you want lower case)
--
-- When a message box is created the lower rows will be reserved to store the
-- buttons and the rest of the box will be used to write the text with
-- alignment. However if the box is too small the text will stop printing at
-- certain point and add an elipsis (...).
--
generic
   type Return_Type is (<>);
   with function Image (Element : Return_Type) return String;
package Malef.SDK.Message_Boxes is

   package Message_Boxes_Widgets is new Widgets (Return_Type);

   type Message_Box_Type is
      new Message_Boxes_Widgets.Widget_Type with
      private;

   function Create (
      Message          : Str_Type;
      Height           : Row_Type;
      Width            : Col_Type;
      Foreground_Color : Color_Type;
      Background_Color : Color_Type;
      Selected_Color   : Color_Type;
      Shadow_Color     : Color_Type := Default_Shadow_Color;
      Borders          : String := "+-+| |+-+";
      H_Alignment      : Horizontal_Alignment_Type := Align_Left;
      V_Alignment      : Vertical_Alignment_Type := Centered)
      return Message_Box_Type
   with Pre => Borders'Length = 9 and Borders'First = 1;

   overriding
   function Run (Message_Box : in out Message_Box_Type)
      return Return_Type;

private

   type Surface_Array is array (Return_Type'Range) of
      Malef.Surfaces.Surface_Type;

   type Message_Box_Type is
      new Message_Boxes_Widgets.Widget_Type with
      record
         Message    : Str_Access;
         Position   : Return_Type := Return_Type'First;
         Vertical   : Natural := 0;
         Horizontal : Natural := 0;
         Buttons    : Surface_Array;
      end record;

   overriding
   function Get_Reference (Message_Box : in Message_Box_Type)
      return Surface_Reference;
   overriding
   procedure Update (Message_Box : in out Message_Box_Type);
   procedure Finalize (Message_Box : in out Message_Box_Type);

   Indexes : array (Return_Type'Range) of Malef.Boxes.Layer_Type;

end Malef.SDK.Message_Boxes;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
