-------------------------------------------------------------------------------
--                                                                           --
--     M A L E F - P L A T F O R M - T E R M I N A L - I N P U T . A D S     --
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

with Ada.IO_Exceptions;

package Malef.Platform.Terminal.Input is

   procedure Initialize;

   procedure Finalize;

   procedure Get_Dimensions (
      Rows : out Positive_Row_Count;
      Cols : out Positive_Col_Count);

   Termination_Error : exception;
   Device_Error      : exception renames Ada.IO_Exceptions.Device_Error;
   End_Error         : exception renames Ada.IO_Exceptions.End_Error;

end Malef.Platform.Terminal.Input;
