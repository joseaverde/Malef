with Ada.Directories;
with Ada.Directories.Hierarchical_File_Names;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Interfaces;
with System;

with Ada.Text_IO;

package body Malef.Terminfo is

   --<<----------->>--
   -->> Term_Type <<--
   --<<----------->>--

   use Ada.Streams;

   -- The following page describes the common database binary format.
   -- <https://invisible-island.net/ncurses/man/term.5.html>
   --
   -- It seems there isn't such a thing as a common binrary format for compiled
   -- terminfo databases... So we will follow the three formats this page
   -- describes:
   --
   --  * Legacy Storage Format
   --  * Extended Storage Format
   --  * Extended Number Format
   --
   -- And in case we can't load it, we just emit an error and load the common
   -- dummy format.

   generic
      Bit_Order : in System.Bit_Order;
   package Loaders is

      -- It seems the database is stored in little endian format.
      -- We need some black magic in order to achive more speed.
      --
      -- This package implements the deserialisation of the terminfo format.
      -- In the generic parameters we ask about the endianness of the system.
      -- It is the only parameter, and it is known at compile-time. Therefore
      -- all the branches are removed. And code is generated for each specific
      -- case.
      --
      -- In case of little-endian systems, we just read. In big-endian systems
      -- we have to read byte by byte, invert the order and generate a new
      -- number.

      Magic_Number          : constant := 8#0432#;
      Extended_Magic_Number : constant := 8#1036#;

      procedure Read (
         Stream : not null access Root_Stream_Type'Class;
         Term   : in out Term_Type);

   end Loaders;

   package body Loaders is

      -->> Helper Types <<--

      use Interfaces;

      type Header_Type is
         record
            Magic               : Integer_16;
            Terminal_Names_Size : Integer_16;
            Boolean_Count       : Integer_16;
            Integer_Count       : Integer_16;
            Offset_Count        : Integer_16;
            String_Table_Size   : Integer_16;
         end record with
         Object_Size => 16 * 6;

      procedure Read (
         Stream : not null access Root_Stream_Type'Class;
         Item   : out Header_Type) with Unreferenced;

      for Header_Type'Read use Read;

      subtype Count_Type is Integer_32;
      subtype Natural_Count is Count_Type range 0 .. Count_Type'Last;
      subtype Positive_Count is Count_Type range 1 .. Count_Type'Last;

      type Boolean_Array is array (Positive_Count range <>) of Boolean;
      type Integer_Array is array (Positive_Count range <>) of Integer_32;
      type Char_Array is array (Natural_Count range <>) of Character;

      -->> Helper Functions <<--

      function Convert is
         new Ada.Unchecked_Conversion (
         Source => Unsigned_16,
         Target => Integer_16);

      function Convert is
         new Ada.Unchecked_Conversion (
         Source => Unsigned_32,
         Target => Integer_32);

      type Byte_Array is array (Natural range <>) of Unsigned_8;

      procedure Read (
         Stream : not null access Root_Stream_Type'Class;
         Item   : out Integer_16) is
      begin
         case Bit_Order is
            when System.Low_Order_First =>
               Integer_16'Read (Stream, Item);
            when System.High_Order_First =>
               declare
                  Bytes : Byte_Array (0 .. 1);
               begin
                  Byte_Array'Read (Stream, Bytes);
                  Item := Convert (Shift_Left (Unsigned_16 (Bytes (0)), 8)
                                   or          Unsigned_16 (Bytes (1)));
               end;
         end case;
      end Read;

      procedure Read (
         Stream : not null access Root_Stream_Type'Class;
         Item   : out Integer_32) is
      begin
         case Bit_Order is
            when System.Low_Order_First =>
               Integer_32'Read (Stream, Item);
            when System.High_Order_First =>
               declare
                  Bytes : Byte_Array (0 .. 3);
               begin
                  Byte_Array'Read (Stream, Bytes);
                  Item := Convert (Shift_Left (Unsigned_32 (Bytes (0)), 24)
                                or Shift_Left (Unsigned_32 (Bytes (1)), 16)
                                or Shift_Left (Unsigned_32 (Bytes (2)),  8)
                                or             Unsigned_32 (Bytes (3)));
               end;
         end case;
      end Read;

      procedure Read (
         Stream : not null access Root_Stream_Type'Class;
         Item   : out Header_Type) is
      begin
         Read (Stream, Item.Magic);
         Read (Stream, Item.Terminal_Names_Size);
         Read (Stream, Item.Boolean_Count);
         Read (Stream, Item.Integer_Count);
         Read (Stream, Item.Offset_Count);
         Read (Stream, Item.String_Table_Size);
      end Read;

      -->> Implementation <<--

      procedure Read_Terminal_Names (
         Stream : not null access Root_Stream_Type'Class;
         Term   : in out Term_Type;
         Count  : in     Count_Type)
      is
         Str   : String (1 .. Natural (Count));
         First : Natural := Str'First;
      begin
         -- The string is zero-terminated we skip the last character.
         String'Read (Stream, Str);
         for I in Str'First .. Str'Last - 1 loop
            if Str (I) in '|' | ' ' then
               Term.Names.Append (To_Unbounded_String (Str (First .. I - 1)));
               First := I + 1;
            end if;
            if Str (I) = ' ' then
               Term.Full_Name
                  := To_Unbounded_String (Str (First .. Str'Last - 1));
               exit;
            end if;
         end loop;
      end Read_Terminal_Names;

      function Read_Booleans (
         Stream : not null access Root_Stream_Type'Class;
         Term   : in out Term_Type;
         Chars  : in     Count_Type;
         Count  : in Count_Type)
         return Boolean_Array
      is
         Byte : Unsigned_8;
      begin
         return Result : Boolean_Array (1 .. Count) do
            for I in Result'Range loop
               Unsigned_8'Read (Stream, Byte);
               Result (I) := (if Byte = 0 then False else True);
            end loop;

            -- 16-bit alignment
            if (Chars + Count) mod 2 = 1 then
               Unsigned_8'Read (Stream, Byte);
            end if;

         end return;
      end Read_Booleans;

      function Read_Integers_16 (
         Stream : not null access Root_Stream_Type'Class;
         Term   : in out Term_Type;
         Count  : in Count_Type)
         return Integer_Array is
      begin
         return Result : Integer_Array (1 .. Count) do
            for I in Result'Range loop
               Read (Stream, Integer_16 (Result (I)));
            end loop;
         end return;
      end Read_Integers_16;

      function Read_Integers_32 (
         Stream : not null access Root_Stream_Type'Class;
         Term   : in out Term_Type;
         Count  : in Count_Type)
         return Integer_Array is
      begin
         return Result : Integer_Array (1 .. Count) do
            for I in Result'Range loop
               Read (Stream, Result (I));
            end loop;
         end return;
      end Read_Integers_32;

      function Substring (
         Item : in Char_Array;
         From : in Natural_Count)
         return Unbounded_String
      is
         Value : Unbounded_String;
      begin
         for I in From .. Item'Last loop
            exit when Item (I) = Character'First;
            Append (Value, Item (I));
         end loop;
         return Value;
      end Substring;

      procedure Read (
         Stream : not null access Root_Stream_Type'Class;
         Term   : in out Term_Type)
      is
         Header : Header_Type;
      begin
         Header_Type'Read (Stream, Header);
         Read_Terminal_Names (Stream, Term,
                              Count_Type (Header.Terminal_Names_Size));
         declare
            Booleans : constant Boolean_Array
                     := Read_Booleans (Stream, Term,
                                       Count_Type (Header.Terminal_Names_Size),
                                       Count_Type (Header.Boolean_Count));
            Integers : constant Integer_Array :=
               (if Header.Magic = Magic_Number
                  then Read_Integers_16 (Stream, Term,
                                         Count_Type (Header.Integer_Count))
                  else Read_Integers_32 (Stream, Term,
                                         Count_Type (Header.Integer_Count)));
            Strings : constant Integer_Array :=
               (if Header.Magic = Magic_Number
                  then Read_Integers_16 (Stream, Term,
                                         Count_Type (Header.Offset_Count))
                  else Read_Integers_32 (Stream, Term,
                                         Count_Type (Header.Offset_Count)));
            Table : Char_Array (0 .. Count_Type (Header.String_Table_Size) - 1);
         begin
            Char_Array'Read (Stream, Table);
            Ada.Text_IO.Put_Line (Booleans'Image);
            Ada.Text_IO.Put_Line (Integers'Image);
            Ada.Text_IO.Put_Line (Strings'Image);
            for Index of Strings when Index > 0 loop
               -- Ada.Text_IO.Put_Line (To_String (Substring (Table, Index)));
               -- Ada.Text_IO.New_Line (2);
               Ada.Text_IO.Put_Line (Index'Image);
            end loop;
         end;

         Ada.Text_IO.Put_Line (Header'Image);
      end Read;

   end Loaders;

   package Loader is new Loaders (System.Default_Bit_Order);

   procedure Open (
      Term :    out Term_Type;
      Path : in     String)
   is
      File   : Stream_IO.File_Type;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, Path);
      Loader.Read (Stream_IO.Stream (File), Term);
      Stream_IO.Close (File);
   end Open;

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Term_Type) is
   begin

      if Arg.Names.Is_Empty then
         Buffer.Put ("<INVALID TERMINAL>");
         return;
      end if;

      Buffer.Put (To_String (Arg.Names.First_Element));
      for I in 2 .. Arg.Names.Last_Index loop
         Buffer.Put ("|"); Buffer.Put (To_String (Arg.Names (I)));
      end loop;
      Buffer.Put (" "); Buffer.Put (To_String (Arg.Full_Name));

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
      return String is (
        Hex (Character'Pos (Item) / 16)
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

end Malef.Terminfo;
