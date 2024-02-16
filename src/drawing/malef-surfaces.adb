package body Malef.Surfaces is

   --<<------------------>>--
   -->> Helper Functions <<--
   --<<------------------>>--

   procedure Restore (
      Object : in out Surface'Class;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Bg     : in     Boolean := False;
      Fg     : in     Boolean := False) is
   begin
      if Object.Matrix (Row, Col).Has_Name then
         Object.Matrix (Row, Col).Has_Name := False;
         if Bg then
            Object.Matrix (Row, Col).Background := (
               Object.Palette (Object.Matrix (Row, Col).Bg_Name));
         end if;
         if Fg then
            Object.Matrix (Row, Col).Foreground := (
               Object.Palette (Object.Matrix (Row, Col).Fg_Name));
         end if;
      end if;
   end Restore;

   procedure Restore_Palette (
      Object : in out Surface'Class;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Bg     : in     Boolean := False;
      Fg     : in     Boolean := False) is
   begin
      if not Object.Matrix (Row, Col).Has_Name then
         Object.Matrix (Row, Col).Has_Name := True;
         if Bg then
            Object.Matrix (Row, Col).Bg_Name := Palettes.Nearest (
               Object.Palette, Object.Matrix (Row, Col).Background);
         end if;
         if Fg then
            Object.Matrix (Row, Col).Fg_Name := Palettes.Nearest (
               Object.Palette, Object.Matrix (Row, Col).Foreground);
         end if;
      end if;
   end Restore_Palette;

   procedure Set_Updated (
      Object : in out Surface'Class;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count) is
   begin
      if not Object.Updated then
         Object.Updated := True;
         Object.From := (Row, Col);
         Object.To := (Row, Col);
      else
         Object.From.Row := Row_Type'Min (Row, @);
         Object.From.Col := Col_Type'Min (Col, @);
         Object.To.Row := Row_Type'Max (Row, @);
         Object.To.Col := Col_Type'Max (Col, @);
      end if;
   end Set_Updated;

   procedure Set_Updated (
      Object : in out Surface'Class;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type) is
   begin
      Set_Updated (Object, From.Row, From.Col);
      Set_Updated (Object, To.Row, To.Col);
   end Set_Updated;

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Surface) is
   begin
      for Row in 1 .. Arg.Rows loop
         Buffer.Put ("[ """);
         for Col in 1 .. Arg.Cols loop
            if Arg.Matrix (Row, Col).Character = Nul then
               Buffer.Wide_Wide_Put (" ");
            else
               Buffer.Wide_Wide_Put (Arg.Matrix (Row, Col).Character & "");
            end if;
         end loop;
         Buffer.Put (""" ]");
         Buffer.New_Line;
      end loop;
      Buffer.Increase_Indent;
      if Arg.Updated then
         Buffer.Put ("Has been modified in region (");
         Buffer.Put (Arg.From.Row'Image); Buffer.Put (",");
         Buffer.Put (Arg.From.Col'Image); Buffer.Put (" ) -> (");
         Buffer.Put (Arg.To.Row'Image); Buffer.Put (",");
         Buffer.Put (Arg.To.Col'Image); Buffer.Put (" )");
      else
         Buffer.Put ("   Is up to date");
      end if;
      Buffer.New_Line;
      Palettes.Put_Image (Buffer, Arg.Palette);
   end Put_Image;

   --<<-------------->>--
   -->> Surface Info <<--
   --<<-------------->>--

   procedure Set_Up_to_Date (
      Object : in out Surface) is
   begin
      Object.Updated := False;
   end Set_Up_to_Date;

   --<<-------------------->>--
   -->> On Cell Operations <<--
   --<<-------------------->>--

   -->> Glyphs <<--

   function Reference (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return Glyph_Reference_Type is
   begin
      Set_Updated (Object, Row, Col);
      return (Element => Object.Matrix (Row, Col).Character'Access);
   end Reference;

   procedure Set (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     Glyph) is
   begin
      Set_Updated (Object, Row, Col);
      Object.Matrix (Row, Col).Character := Item;
   end Set;

   -->> Styles <<--

   function Reference (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return Style_Reference_Type is
   begin
      Set_Updated (Object, Row, Col);
      return (Element => Object.Matrix (Row, Col).Style'Access);
   end Reference;

   procedure Set (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     Style_Type) is
   begin
      Set_Updated (Object, Row, Col);
      Object.Matrix (Row, Col).Style := Item;
   end Set;

   -->> Colours <<--

   function Background (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return RGBA_Reference_Type is
   begin
      Set_Updated (Object, Row, Col);
      Restore (Object, Row, Col, Fg => True);
      return (Element => Object.Matrix (Row, Col).Background'Access);
   end Background;

   function Foreground (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return RGBA_Reference_Type is
   begin
      Set_Updated (Object, Row, Col);
      Restore (Object, Row, Col, Bg => True);
      return (Element => Object.Matrix (Row, Col).Foreground'Access);
   end Foreground;

   procedure Set (
      Object     : in out Surface;
      Row        : in     Positive_Row_Count;
      Col        : in     Positive_Col_Count;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type) is
   begin
      Set_Updated (Object, Row, Col);
      Restore (Object, Row, Col);
      Object.Matrix (Row, Col).Background := Background;
      Object.Matrix (Row, Col).Foreground := Foreground;
   end Set;

   procedure Set_Background (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     RGBA_Type) is
   begin
      Set_Updated (Object, Row, Col);
      Restore (Object, Row, Col, Fg => True);
      Object.Matrix (Row, Col).Background := Item;
   end Set_Background;

   procedure Set_Foreground (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     RGBA_Type) is
   begin
      Set_Updated (Object, Row, Col);
      Restore (Object, Row, Col, Bg => True);
      Object.Matrix (Row, Col).Foreground := Item;
   end Set_Foreground;

   -->> Palettes <<--

   function Background_Id (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return Palette_Index_Reference_Type is
   begin
      Set_Updated (Object, Row, Col);
      Restore_Palette (Object, Row, Col, Fg => True);
      return (Element => Object.Matrix (Row, Col).Bg_Name'Access);
   end Background_Id;

   function Foreground_Id (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return Palette_Index_Reference_Type is
   begin
      Set_Updated (Object, Row, Col);
      Restore_Palette (Object, Row, Col, Bg => True);
      return (Element => Object.Matrix (Row, Col).Fg_Name'Access);
   end Foreground_Id;

   procedure Set (
      Object     : in out Surface;
      Row        : in     Positive_Row_Count;
      Col        : in     Positive_Col_Count;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index) is
   begin
      Set_Updated (Object, Row, Col);
      Restore_Palette (Object, Row, Col);
      Object.Matrix (Row, Col).Bg_Name := Background;
      Object.Matrix (Row, Col).Fg_Name := Foreground;
   end Set;

   procedure Set_Background (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     Palette_Index) is
   begin
      Set_Updated (Object, Row, Col);
      Restore (Object, Row, Col, Fg => True);
      Object.Matrix (Row, Col).Bg_Name := Item;
   end Set_Background;

   procedure Set_Foreground (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     Palette_Index) is
   begin
      Set_Updated (Object, Row, Col);
      Restore (Object, Row, Col, Bg => True);
      Object.Matrix (Row, Col).Fg_Name := Item;
   end Set_Foreground;

   -->> Omni <<--

   procedure Set (
      Object     : in out Surface;
      Row        : in     Positive_Row_Count;
      Col        : in     Positive_Col_Count;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type;
      Character  : in     Glyph;
      Style      : in     Style_Type) is
   begin
      Set_Updated (Object, Row, Col);
      Restore (Object, Row, Col);
      Object.Matrix (Row, Col) := (
         @ with delta Background => Background,
                      Foreground => Foreground,
                      Character  => Character,
                      Style      => Style);
   end Set;

   procedure Set (
      Object     : in out Surface;
      Row        : in     Positive_Row_Count;
      Col        : in     Positive_Col_Count;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index;
      Character  : in     Glyph;
      Style      : in     Style_Type) is
   begin
      Set_Updated (Object, Row, Col);
      Restore_Palette (Object, Row, Col);
      Object.Matrix (Row, Col) := (
         @ with delta Bg_Name => Background,
                      Fg_Name => Foreground,
                      Character => Character,
                      Style     => Style);
   end Set;

   --<<---------------------->>--
   -->> On Ranges Operations <<--
   --<<---------------------->>--

   -->> Glyphs <<--

   procedure Fill (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
    Item   : in     Glyph) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Object.Matrix (Row, Col).Character := Item;
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill;

   procedure Fill (
      Object : in out Surface;
      Item   : in     Glyph) is
   begin
      Fill (Object, (1, 1), (Object.Rows, Object.Cols), Item);
   end Fill;

   procedure Fill (
      Object  : in out Surface;
      From    : in     Cursor_Type;
      To      : in     Cursor_Type;
      Process : not null access procedure (Item     : in out Glyph;
                                           Position : in     Cursor_Type)) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Process.all (Object.Matrix (Row, Col).Character, (Row, Col));
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill;

   procedure Fill (
      Object  : in out Surface;
      Process : not null access procedure (Item     : in out Glyph;
                                           Position : in     Cursor_Type)) is
   begin
      Fill (Object, (1, 1), (Object.Rows, Object.Cols), Process);
   end Fill;

   -->> Styles <<--

   procedure Fill (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     Style_Type) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Object.Matrix (Row, Col).Style := Item;
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill;

   procedure Fill (
      Object : in out Surface;
      Item   : in     Style_Type) is
   begin
      Fill (Object, (1, 1), (Object.Rows, Object.Cols), Item);
   end Fill;

   procedure Fill (
      Object  : in out Surface;
      From    : in     Cursor_Type;
      To      : in     Cursor_Type;
      Process : not null access procedure (Item     : in out Style_Type;
                                           Position : in     Cursor_Type)) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Process.all (Object.Matrix (Row, Col).Style, (Row, Col));
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill;

   procedure Fill (
      Object  : in out Surface;
      Process : not null access procedure (Item     : in out Style_Type;
                                           Position : in     Cursor_Type)) is
   begin
      Fill (Object, (1, 1), (Object.Rows, Object.Cols), Process);
   end Fill;

   -->> Colours <<--

   procedure Fill_Background (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     RGBA_Type) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Restore (Object, Row, Col, Fg => True);
            Object.Matrix (Row, Col).Background := Item;
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill_Background;

   procedure Fill_Foreground (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     RGBA_Type) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Restore (Object, Row, Col, Bg => True);
            Object.Matrix (Row, Col).Foreground := Item;
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill_Foreground;

   procedure Fill (
      Object     : in out Surface;
      From       : in     Cursor_Type;
      To         : in     Cursor_Type;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Restore (Object, Row, Col);
            Object.Matrix (Row, Col).Background := Background;
            Object.Matrix (Row, Col).Foreground := Foreground;
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill;

   procedure Fill_Background (
      Object : in out Surface;
      Item   : in     RGBA_Type) is
   begin
      Fill_Background (Object, (1, 1), (Object.Rows, Object.Cols), Item);
   end Fill_Background;

   procedure Fill_Foreground (
      Object : in out Surface;
      Item   : in     RGBA_Type) is
   begin
      Fill_Foreground (Object, (1, 1), (Object.Rows, Object.Cols), Item);
   end Fill_Foreground;

   procedure Fill (
      Object     : in out Surface;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type) is
   begin
      Fill (Object, (1, 1), (Object.Rows, Object.Cols),
            Background, Foreground);
   end Fill;

   procedure Fill (
      Object  : in out Surface;
      From    : in     Cursor_Type;
      To      : in     Cursor_Type;
      Process : not null access procedure (Background : in out RGBA_Type;
                                           Foreground : in out RGBA_Type;
                                           Position   : in     Cursor_Type)) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Restore (Object, Row, Col, Bg => True, Fg => True);
            Process (Background => Object.Matrix (Row, Col).Background,
                     Foreground => Object.Matrix (Row, Col).Foreground,
                     Position   => (Row, Col));
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill;

   procedure Fill (
      Object  : in out Surface;
      Process : not null access procedure (Background : in out RGBA_Type;
                                           Foreground : in out RGBA_Type;
                                           Position   : in     Cursor_Type)) is
   begin
      Fill (Object, (1, 1), (Object.Rows, Object.Cols), Process);
   end Fill;

   -->> Palettes <<--

   procedure Fill_Background (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     Palette_Index) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Restore_Palette (Object, Row, Col, Fg => True);
            Object.Matrix (Row, Col).Bg_Name := Item;
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill_Background;

   procedure Fill_Foreground (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     Palette_Index) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Restore_Palette (Object, Row, Col, Bg => True);
            Object.Matrix (Row, Col).Fg_Name := Item;
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill_Foreground;

   procedure Fill (
      Object     : in out Surface;
      From       : in     Cursor_Type;
      To         : in     Cursor_Type;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Restore_Palette (Object, Row, Col);
            Object.Matrix (Row, Col).Bg_Name := Background;
            Object.Matrix (Row, Col).Fg_Name := Foreground;
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill;

   procedure Fill_Background (
      Object : in out Surface;
      Item   : in     Palette_Index) is
   begin
      Fill_Background (Object, (1, 1), (Object.Rows, Object.Cols), Item);
   end Fill_Background;

   procedure Fill_Foreground (
      Object : in out Surface;
      Item   : in     Palette_Index) is
   begin
      Fill_Foreground (Object, (1, 1), (Object.Rows, Object.Cols), Item);
   end Fill_Foreground;

   procedure Fill (
      Object     : in out Surface;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index) is
   begin
      Fill (Object, (1, 1), (Object.Rows, Object.Cols),
            Background, Foreground);
   end Fill;

   procedure Fill (
      Object  : in out Surface;
      From    : in     Cursor_Type;
      To      : in     Cursor_Type;
      Process : not null access procedure (Background : in out Palette_Index;
                                           Foreground : in out Palette_Index;
                                           Position   : in     Cursor_Type)) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Restore_Palette (Object, Row, Col, Bg => True, Fg => True);
            Process (Background => Object.Matrix (Row, Col).Bg_Name,
                     Foreground => Object.Matrix (Row, Col).Fg_Name,
                     Position   => (Row, Col));
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill;

   procedure Fill (
      Object  : in out Surface;
      Process : not null access procedure (Background : in out Palette_Index;
                                           Foreground : in out Palette_Index;
                                           Position   : in     Cursor_Type)) is
   begin
      Fill (Object, (1, 1), (Object.Rows, Object.Cols), Process);
   end Fill;

   -->> Omni <<--

   procedure Fill (
      Object     : in out Surface;
      From       : in     Cursor_Type;
      To         : in     Cursor_Type;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type;
      Character  : in     Glyph;
      Style      : in     Style_Type) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Restore (Object, Row, Col);
            Object.Matrix (Row, Col) := (
               @ with delta Background => Background,
                            Foreground => Foreground,
                            Character  => Character,
                            Style      => Style);
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill;

   procedure Fill (
      Object     : in out Surface;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type;
      Character  : in     Glyph;
      Style      : in     Style_Type) is
   begin
      Fill (Object, (1, 1), (Object.Rows, Object.Cols),
            Background, Foreground, Character, Style);
   end Fill;

   procedure Fill (
      Object     : in out Surface;
      From       : in     Cursor_Type;
      To         : in     Cursor_Type;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index;
      Character  : in     Glyph;
      Style      : in     Style_Type) is
   begin
      for Row in From.Row .. To.Row loop
         for Col in From.Col .. To.Col loop
            Restore_Palette (Object, Row, Col);
            Object.Matrix (Row, Col) := (
               @ with delta Bg_Name   => Background,
                            Fg_Name   => Foreground,
                            Character => Character,
                            Style     => Style);
         end loop;
      end loop;
      Set_Updated (Object, From, To);
   end Fill;

   procedure Fill (
      Object     : in out Surface;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index;
      Character  : in     Glyph;
      Style      : in     Style_Type) is
   begin
      Fill (Object, (1, 1), (Object.Rows, Object.Cols),
            Background, Foreground, Character, Style);
   end Fill;

   --<<------>>--
   -->> Text <<--
   --<<------>>--

   procedure Put (
      Object : in out Surface;
      Row    : in     Row_Type;
      Col    : in     Col_Type;
      Item   : in     Glyph_String) is
   begin
      for C in Col .. Col + Item'Length - 1 loop
         Object.Matrix (Row, C).Character
            := Item (Item'First + Natural (C - Col));
      end loop;
      Set_Updated (Object, (Row, Col), (Row, Col + Item'Length - 1));
   end Put;

   procedure Put (
      Object : in out Surface;
      Row    : in     Row_Type;
      Col    : in     Col_Type;
      Block  : in     Glyph_Block) is
   begin
      for R in Row .. Row + Block'Length (1) - 1 loop
         for C in Col .. Col + Block'Length (2) - 1 loop
            Object.Matrix (R, C).Character :=
               Block (Block'First (1) + Natural (R - Row),
                      Block'First (2) + Natural (C - Col));
         end loop;
      end loop;
      Set_Updated (Object, (Row, Col),
                           ((Row + Block'Length (1) - 1),
                            (Col + Block'Length (2) - 1)));
   end Put;

   --<<---------->>--
   -->> Palettes <<--
   --<<---------->>--

   function Set_Palette (
      Object : aliased in out Surface)
      return Palette_Reference_Type is
   begin
      Set_Updated (Object, (1, 1), (Object.Rows, Object.Cols));
      return (Element => Object.Palette'Access);
   end Set_Palette;

end Malef.Surfaces;
