-------------------------------------------------------------------------------
--                                                                           --
--                  M A L E F - E X C E P T I O N S . A D S                  --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  --
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
-- This package contains the exceptions used in this package, their name is
-- Something_Error.
--
-- @description
-- This package contains the exceptions used in this package, nothing else.
--
package Malef.Exceptions is

   -- This exception is raised when any issue happens during initialization or
   -- finalization of a specific part of a package.
   Initialization_Error : exception;

   -- This exception is raised whenever the user is trying to index somthing
   -- out of its bounds.
   Bounds_Error : exception;

   -- This exception is raised whenever the user is trying to use a null or
   -- not-initialised surface, i.e. the Shared_Null_Surface a.k.a
   -- "THE NULL SURFACE".
   Null_Surface_Error : exception;

   -- This exception is raised when there is any problem with a Window.
   Window_Error : exception;

   -- This exception is raised when there is any problem with layers inside
   -- a Box for example.
   Layer_Error : exception;

end Malef.Exceptions;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
