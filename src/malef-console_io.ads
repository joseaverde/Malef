private package Malef.Console_IO is

   task Console is

      entry Start;
      entry Stop;
      entry Put (
         Position   : in Cursor_Type;
         Item       : in Glyph_String;
         Background : in RGBA_Type;
         Foreground : in RGBA_Type;
         Style      : in Style_Type);
      entry Put_Indexed (
         Position   : in Cursor_Type;
         Item       : in Glyph_String;
         Background : in Palette_Index;
         Foreground : in Palette_Index;
         Style      : in Style_Type);
      entry Flush;

   end Console;

end Malef.Console_IO;
