-------------------------------------------------------------------------------
--                                                                           --
--               M A L E F - S T Y L E S - C O M P A T . A D S               --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2021-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

with Malef.Styles.Sheets;
with Malef.Palettes;

package Malef.Styles.Compat is

   use Palettes;

   Style : constant Sheets.Style_Sheet :=
      [ "Application" => [ Background => Default_Palette (Palettes.Green)'Image
                         , Foreground => Default_Palette (Black)'Image
                         ]
      , "Dialog" => [ Background        => Default_Palette (White)'Image
                    , Foreground        => Default_Palette (Black)'Image
                    , Border_Background => Default_Palette (White)'Image
                    , Border_Foreground => Default_Palette (Black)'Image
                    , Border_Thickness  => "Thin"
                    , Border_Style      => "Square"
                    ]
      ];

end Malef.Styles.Compat;
