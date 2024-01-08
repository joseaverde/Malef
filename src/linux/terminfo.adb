with Ada.Directories;
with Ada.Directories.Hierarchical_File_Names;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Interfaces;

with Ada.Text_IO;

package body Terminfo is

   use Ada.Streams;

   Magic_Number : constant := 8#0432#;

   type Header_Type is
      record
         Magic                  : Interfaces.Integer_16;
         Terminal_Names_Size    : Interfaces.Integer_16;
         Boolean_Count          : Interfaces.Integer_16;
         Integer_Count          : Interfaces.Integer_16;
         Offset_Count           : Interfaces.Integer_16;
         String_Table_Size      : Interfaces.Integer_16;
      end record;

   procedure Open (
      Term :    out Term_Type;
      Path : in     String)
   is
      File   : Stream_IO.File_Type;
      Header : Header_Type;
      Stream : Stream_IO.Stream_Access;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, Path);
      Stream := Stream_IO.Stream (File);
      Header_Type'Read (Stream, Header);
      Ada.Text_IO.Put_Line (Header'Image);
      Stream_IO.Close (File);
   end Open;

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Term_Type) is
   begin
      Buffer.Put ("Terminal");
   end Put_Image;

   --<<-------------------->>--
   -->> Terminfo Searching <<--
   --<<-------------------->>--

   -- The are two storage models:
   --  * Directory tree
   --  * Hashed Database
   --
   -- ## Directory Tree
   -- The first one is simple enough. Imagine we want to searck for `kitty'.
   -- We would have to search in `k/kitty'. That is, we avoid doing a very long
   -- linear search because we know the first letter (similar to a real life
   -- dictionary). There is only one problem:
   --
   -- <https://invisible-island.net/ncurses/man/term.5.html>
   -- In section `Mixed case Terminal Names', on Case Insensitive systems, i.e.
   -- like in Ada, file "foo" is the same as "FOO" or "fOo". And there are some
   -- terminals whose names start with a capital letter. In those cases instead
   -- of using the first letter as the first directory, we would use the
   -- hexadecimal pair of the first character's position, i.e. `kitty' would be
   -- found in `6B/KITTY' (case-insensitive) instead of `k/kitty'.
   --
   -- Fortunately Ada has a function that returns that, if on a given file
   -- system, its files are Case-Sensitive or Case-Insensitive.
   --
   -- ## Hashed Database
   -- TODO: These are not yet implemented. I think they would require the
   -- library to be linked to `libtinfo.so' (I haven't find enough information
   -- about that library yet). And therefore Malef would have dependencies.

   use Ada.Directories;
   package DHFN renames Ada.Directories.Hierarchical_File_Names;
   package Env renames Ada.Environment_Variables;

   Hex : constant array (0 .. 15) of Character := "0123456789ABCDEF";

   function As_Hex (
      Item : in Character)
      return String is
      ( Hex (Character'Pos (Item) / 16)
      & Hex (Character'Pos (Item) mod 16));

   function Indexed (
      Name : in String;
      Path : in String)
      return String is (
      (if Name_Case_Equivalence (Path) in Unknown | Case_Sensitive
         then Name (Name'First) & ""
         else As_Hex (Name (Name'First))));

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

   function Exists_Not_Null (
      Variable : in String)
      return Boolean is (
      (Env.Exists (Variable) and then Env.Value (Variable) /= ""));

   function Search (
      Name : in String)
      return String is
   begin

      -- First search using the $TERMINFO environment variable, if it is set
      -- it is the only place we can search.

      if Exists_Not_Null (TERMINFO_Env) then
         declare
            Path : constant String := Search (Name, Env.Value (TERMINFO_Env));
         begin
            return Path;
         end;
      end if;

      -- If not not found, try looking at $HOME/.terminfo

      if Exists_Not_Null (HOME_Env) then
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

      if Exists_Not_Null (TERMINFO_DIRS_Env) then
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

   function Search_Current return String is (
      (if Exists_Not_Null (TERM_Env)
         then Search (Env.Value (TERM_Env))
         else Search (Default_Terminal)));

end Terminfo;
