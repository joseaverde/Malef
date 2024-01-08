with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Directories.Hierarchical_File_Names;

package body Terminfo is

   procedure Open (
      File : in     String;
      Term :    out Term_Type) is null;

   package DHFN renames Ada.Directories.Hierarchical_File_Names;
   package Env renames Ada.Environment_Variables;

   Hex : constant array (0 .. 15) of Character := "0123456789ABCDEF";

   function As_Hex (
      Item : in Character)
      return String is
      ( Hex (Character'Pos (Item) / 16)
      & Hex (Character'Pos (Item) mod 16));

   use Ada.Directories;

   function Indexed (
      Name : in String;
      Path : in String)
      return String is (
      (if Name_Case_Equivalence (Path) in Unknown | Case_Sensitive
         then Name (Name'First) & ""
         else As_Hex (Name (Name'First))));
   -- <https://invisible-island.net/ncurses/man/term.5.html>
   -- Mixed case Terminal Names

   function If_Exists (
      Path : in String)
      return String is (
      (if Exists (Path) then Path else ""));

   function Search (
      Name : in String;
      Path : in String)
      return String is (
      (if not Exists (Path) or else Kind (Path) /= Directory
         then ""
         else If_Exists (DHFN.Compose (
                         DHFN.Compose (Path, Indexed (Name, Path)), Name))));

   function Search (
      Name : in String)
      return String is
   begin

      -- First search using the $TERMINFO environment variable, if it is set
      -- it is the only place we can search.

      if Env.Exists (TERMINFO_Env) then
         declare
            Path : constant String := Search (Name, Env.Value (TERMINFO_Env));
         begin
            return Path;
         end;
      end if;

      -- If not not found, try looking at $HOME/.terminfo

      if Env.Exists (HOME_Env) then
         declare
            Path : constant String
                 := Search (Name,
                            DHFN.Compose (Env.Value (HOME_Env), ".terminfo"));
         begin
            if Path /= "" then
               return Path;
            end if;
         end;
      end if;

      -- Then look in the $TERMINFO_DIRS environment variable. It contains a
      -- list of directories separated by colons.

      if Env.Exists (TERMINFO_DIRS_Env) then

         declare
            Paths : constant String := Env.Value (TERMINFO_DIRS_Env);
            First : Positive := Paths'First;
            Last  : Positive := Paths'First;
         begin

            loop
               if Last > Paths'Last or else Paths (Last) = Dir_Separator then

                  declare
                     Path : constant String
                          := Search (Name, (if Last - First = 0
                                             then Default_Dir
                                             else Paths (First .. Last - 1)));
                  begin
                     if Path /= "" then
                        return Path;
                     end if;
                  end;
                  First := Last + 1;

               end if;

               exit when Last not in Paths'Range;
                  
               Last := Last + 1;
            end loop;

         end;

      end if;

      -- Finally search in /usr/share/terminfo

      declare
         Path : constant String := Search (Name, Default_Dir);
      begin

         -- If it couldn't be found return an empty string, don't raise an
         -- exception. We will later treat this edge case.

         return Path;

      end;

   end Search;


end Terminfo;
