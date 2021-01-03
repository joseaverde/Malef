-------------------------------------------------------------------------------
--                                                                           --
-- M A L E F - S Y S T E M S - G E T _ F O R M A T . S E P A R A T E . A D B --
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

-- with Malef.System_Utils.Ansi;
separate (Malef.Systems)

function Get_Format (Format : Format_Type) return String is
   function To_String (C : Color_Component_Type) return String
       renames Malef.System_Utils.To_String;
begin

   -- TODO: This function only returns colours, optimize it.

   return ASCII.ESC & '[' &
            "38;2;" & To_String(Format.Foreground_Color(R)) & ';' &
                      To_String(Format.Foreground_Color(G)) & ';' &
                      To_String(Format.Foreground_Color(B)) & ';' &
            "48;2;" & To_String(Format.Background_Color(R)) & ';' &
                      To_String(Format.Background_Color(G)) & ';' &
                      To_String(Format.Background_Color(B)) &
            'm';

end Get_Format;



---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
