-------------------------------------------------------------------------------
--                                                                           --
--   MALEF - S U B S Y S T E M S - C O M P O N E N T S - C O L O R S . ADS   --
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

with Malef.Colors; use Malef.Colors;

-- TODO: Comment it
-- @summary
--
--
-- @description
--
package Malef.Subsystems.Components.Colors is

   --
   -- This function converts an Color_Component_Type into a String.
   --
   -- @param C
   -- The Color_Component_Type integer to convert.
   --
   -- @return
   -- The string that represents such colour.
   --
   function To_String (
      C : Color_Component_Type)
      return String
      with Pure_Function,
           Inline;

   --
   -- This function converts a Natural number into a String.
   --
   -- @param N
   -- The number.
   --
   -- @return
   -- The string representation of such number.
   --
   function To_String (
      N : Natural)
      return String
      with Pure_Function,
           Inline;


   type H_Type  is delta 0.1 digits 4 range 0.0 .. 255.9;
   type SB_Type is           digits 6 range 0.0 .. 1.0;

   type HSB_Type is
      record
         Hue        : H_Type;
         Saturation : SB_Type;
         Brightness : SB_Type;
      end record;

   procedure Precalculate;

   procedure To_Color_1 (Color : in HSB_Type;
      Is_White : out Boolean)
      with Inline;

   procedure To_Color_3 (Color : in HSB_Type;
      Out_Color : out Color_Kind)
      with Inline;

   procedure To_Color_4 (Color : HSB_Type;
      Out_Color  : out Color_Kind;
      Brightness : out Boolean)
      with Inline;

   procedure To_Color_8 (Color : Color_Type;
      Out_Color : out Color_Component_Type)
      with Inline;

   function To_HSB (Color : Color_Type)
      return HSB_Type
      with Pure_Function;

   function To_RGB (Color : HSB_Type)
      return Color_Type
      with Pure_Function;

private

   Current_Palette : Palette_Type;
   The_Palette : array(Palette_Type'Range(1),Palette_Type'Range(2))of HSB_Type;

   type Percentage is delta 0.0001 range 0.0000 .. 1.0000;

   function Color_Match (Left, Right : HSB_Type)
      return Percentage
      with Inline, Pure_Function;

   --
   -- This constant contains all numbers from 0 to 255 to speed up the function
   -- that converts a Color_Component_Type into a String using only indexes.
   --    for rg in [range(start, start + 8) for start in range(0, 256, 8)]:
   --       for i in rg:
   --          s = str(i);
   --          s = (3 - len(s)) * '0' + s
   --          print('"%s", ' % s, end="")
   --       print()
   --
   Color_Components_Strings : constant array (Color_Component_Type'Range)
                                       of String (1 .. 3) :=
     ("000", "001", "002", "003", "004", "005", "006", "007",
      "008", "009", "010", "011", "012", "013", "014", "015",
      "016", "017", "018", "019", "020", "021", "022", "023",
      "024", "025", "026", "027", "028", "029", "030", "031",
      "032", "033", "034", "035", "036", "037", "038", "039",
      "040", "041", "042", "043", "044", "045", "046", "047",
      "048", "049", "050", "051", "052", "053", "054", "055",
      "056", "057", "058", "059", "060", "061", "062", "063",
      "064", "065", "066", "067", "068", "069", "070", "071",
      "072", "073", "074", "075", "076", "077", "078", "079",
      "080", "081", "082", "083", "084", "085", "086", "087",
      "088", "089", "090", "091", "092", "093", "094", "095",
      "096", "097", "098", "099", "100", "101", "102", "103",
      "104", "105", "106", "107", "108", "109", "110", "111",
      "112", "113", "114", "115", "116", "117", "118", "119",
      "120", "121", "122", "123", "124", "125", "126", "127",
      "128", "129", "130", "131", "132", "133", "134", "135",
      "136", "137", "138", "139", "140", "141", "142", "143",
      "144", "145", "146", "147", "148", "149", "150", "151",
      "152", "153", "154", "155", "156", "157", "158", "159",
      "160", "161", "162", "163", "164", "165", "166", "167",
      "168", "169", "170", "171", "172", "173", "174", "175",
      "176", "177", "178", "179", "180", "181", "182", "183",
      "184", "185", "186", "187", "188", "189", "190", "191",
      "192", "193", "194", "195", "196", "197", "198", "199",
      "200", "201", "202", "203", "204", "205", "206", "207",
      "208", "209", "210", "211", "212", "213", "214", "215",
      "216", "217", "218", "219", "220", "221", "222", "223",
      "224", "225", "226", "227", "228", "229", "230", "231",
      "232", "233", "234", "235", "236", "237", "238", "239",
      "240", "241", "242", "243", "244", "245", "246", "247",
      "248", "249", "250", "251", "252", "253", "254", "255");


end Malef.Subsystems.Components.Colors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================--- 
