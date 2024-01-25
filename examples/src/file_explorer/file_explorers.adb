with Malef;
with Ada.Text_IO;
with Ada.Containers.Generic_Array_Sort;
with Ada.Calendar;

package body File_Explorers is

   use Ada.Strings.Unbounded;
   use Ada.Directories;

   File_Colour       : constant Malef.RGBA_Type := (240, 240, 240, 255);
   Directory_Colour  : constant Malef.RGBA_Type := (80, 230, 100, 255);

   Folder_Image : constant Malef.Glyph_Block :=
      ["🭅█████🭎🬭🬭🬭🬽",
       "███████████",
       "███████████",
       "███████████"];

   File_Image : constant Malef.Glyph_Block :=
      ["  ██████🭏  ",
       "  █▓▓█▓██  ",
       "  █▓▓▓█▓█  ",
       "  ███████  "];

   Special_File_Image : constant Malef.Glyph_Block :=
      ["  ██ █ █🭏  ",
       "  ███ ███  ",
       "  ██ █ ██  ",
       "  █ ███ █  "];

   Left_Offset    : constant := 2;
   Right_Offset   : constant := 2;
   Top_Offset     : constant := 2;
   Bottom_Offset  : constant := 2;
   Row_Separation : constant := 2;
   Col_Separation : constant := 2;

   type File_Entry_Type is
      record
         Name : Unbounded_String;
         Size : File_Size;
         Kind : File_Kind;
         Time : Ada.Calendar.Time;
      end record;

   function "<" (Left, Right : in File_Entry_Type)
      return Boolean is (
      Left.Name < Right.Name);

   function Create (
      The_Entry : in Directory_Entry_Type)
      return File_Entry_Type is (
      Name => To_Unbounded_String (Simple_Name (The_Entry)),
      Size => Size (The_Entry),
      Kind => Kind (The_Entry),
      Time => Modification_Time (The_Entry));

   type File_Entry_Array is array (Positive range <>) of File_Entry_Type;

   procedure Sort is
      new Ada.Containers.Generic_Array_Sort (
      Index_Type   => Positive,
      Element_Type => File_Entry_Type,
      Array_Type   => File_Entry_Array,
      "<"          => "<");

   function Is_Hidden (
      File : in Directory_Entry_Type)
      return Boolean is (
      (declare
         Name : constant String := Simple_Name (File);
      begin
         Name (Name'First) = '.'));

   function Always_Hidden (
      File : in Directory_Entry_Type)
      return Boolean is (
      (declare
         Name : constant String := Simple_Name (File);
      begin
         Name = "." or else Name = ".."));

   function File_Count (
      Path        : in String;
      Show_Hidden : in Boolean := False)
      return Natural
   is
      Searcher  : Search_Type;
      Dir_Entry : Directory_Entry_Type;
      Count     : Natural := 0;
   begin
      Start_Search (Searcher, Path, "*", Filter => (others => True));
      while More_Entries (Searcher) loop
         Get_Next_Entry (Searcher, Dir_Entry);
         if Show_Hidden or else not Is_Hidden (Dir_Entry) then
            Count := Count + 1;
         end if;
      end loop;
      return Count - (if not Show_Hidden then 2 else 0);
   end File_Count;

   function List_Files (
      Path        : in String;
      Show_Hidden : in Boolean := False)
      return File_Entry_Array
   is
      Max_Files : constant Natural := File_Count (Path, Show_Hidden);
      Searcher  : Search_Type;
      Dir_Entry : Directory_Entry_Type;
      Index     : Natural := 1;
   begin
      return Result : File_Entry_Array (1 .. Max_Files) do
         Start_Search (Searcher, Path, "*", Filter => (others => True));
         while More_Entries (Searcher) and then Index in Result'Range loop
            Get_Next_Entry (Searcher, Dir_Entry);
            if not Always_Hidden (Dir_Entry) and then
               (Show_Hidden or else not Is_Hidden (Dir_Entry))
            then
               Result (Index) := Create (Dir_Entry);
               Index := Index + 1;
            end if;
         end loop;
      end return;
   end List_Files;

   overriding
   procedure On_Draw (
      Object  : in     File_Explorer_Widget;
      Surface : in out Malef.Surfaces.Surface;
      Area    : in     Malef.Widgets.Draw_Area)
   is

      use Malef;

      -- We precalculate the size of the screen.

      Screen_Width   : constant Col_Type := Area.To.Col - Area.From.Col;
      Screen_Height  : constant Row_Type := Area.To.Row - Area.From.Row;

      -- We calculate the real element size.

      Element_Height : constant Natural
                     := Folder_Image'Length (1) + Row_Separation;
      Element_Width  : constant Natural
                     := Folder_Image'Length (2) + Col_Separation;

      -- We calculate the number of elements that can fit.

      Elements_Per_Col : constant Natural
                       := Natural (Screen_Height - Top_Offset -
                                   Bottom_Offset + Col_Separation)
                                  / Element_Height;
      Elements_Per_Row : constant Natural
                       := Natural (Screen_Width - Left_Offset -
                                   Right_Offset + Row_Separation)
                                  / Element_Width;
      Max_Elements : constant Natural := Elements_Per_Col * Elements_Per_Row;

      -- We recalculate the offsets

      Row_Remainder : constant Row_Type
                    := Screen_Height + Row_Separation
                     - Row_Type (Elements_Per_Col * Element_Height);
      Col_Remainder : constant Col_Type
                    := Screen_Width + Col_Separation
                     - Col_Type (Elements_Per_Row * Element_Width);
      Top_Offset    : constant Row_Type := Row_Remainder / 2;
      Bottom_Offset : constant Row_Type := (Row_Remainder + 1) / 2;
      Left_Offset   : constant Col_Type := Col_Remainder / 2;
      Right_Offset  : constant Col_Type := (Col_Remainder + 1) / 2;

      -- Get the elements in the current directory

      File_List : File_Entry_Array := List_Files (To_String (Object.Path));
      Element_Count : constant Natural := Natural'Min (File_List'Length,
                                                       Max_Elements);
      Background_Colour : constant RGBA_Type := (80, 80, 80, 255);

      Foreground_Colour : RGBA_Type;
      Index : Natural := 0;
      The_Row : Row_Type;
      The_Col : Col_Type;
   begin
      Surface.Fill_Background (Area.From, Area.To, Background_Colour);
      Sort (File_List);
      Outer_Loop : for Row in 1 .. Row_Type (Elements_Per_Col) loop
         The_Row := (Row - 1) * Row_Type (Element_Height) + Top_Offset;
         for Col in 1 .. Col_Type (Elements_Per_Row) loop
            Index := Index + 1;
            exit Outer_Loop when Index > Element_Count;
            The_Col := (Col - 1) * Col_Type (Element_Width) + Left_Offset;
            case File_List (Index).Kind is
               when Directory =>
                  Foreground_Colour := Directory_Colour;
                  Surface.Put (The_Row, The_Col, Folder_Image);
               when Ordinary_File =>
                  Foreground_Colour := File_Colour;
                  Surface.Put (The_Row, The_Col, File_Image);
               when Special_File =>
                  Foreground_Colour := File_Colour;
                  Surface.Put (The_Row, The_Col, Special_File_Image);
            end case;
            Surface.Fill_Foreground ((The_Row, The_Col),
                                     (The_Row + File_Image'Length (1),
                                      The_Col + File_Image'Length (2)),
                                     Foreground_Colour);
         end loop;
      end loop Outer_Loop;
   end On_Draw;

end File_Explorers;
