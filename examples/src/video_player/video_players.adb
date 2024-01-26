with Ada.Text_IO;
with Util.Processes;
with Util.Streams.Buffered;
with Util.Streams.Pipes;

package body Video_Players is

   Max_Pipes : constant := 8;

   -- TODO: Protect it

   Pipes : array (1 .. Max_Pipes) of aliased Util.Streams.Pipes.Pipe_Stream;
   Buffs : array (1 .. Max_Pipes) of Util.Streams.Buffered.Input_Buffer_Stream;
   Using : array (1 .. Max_Pipes) of Boolean := (others => False);

   function Escape_Name (
      Name : in String)
      return String
   is
      Result : String (1 .. Name'Length * 2);
      Index  : Positive := 1;
   begin
      Result (1) := ''';
      for Char of Name loop
         Index := Index + 1;
         if Char = ''' then
            Result (Index) := '\';
            Index := Index + 1;
         end if;
         Result (Index) := Char;
      end loop;
      Index := Index + 1;
      Result (Index) := ''';
      return Result (1 .. Index);
   end Escape_Name;

   overriding
   procedure On_Draw (
      Object  : in     Video_Player_Widget;
      Surface : in out Malef.Surfaces.Surface;
      Area    : in     Malef.Widgets.Draw_Area)
   is
      use Malef;
      Rows : constant Row_Type := Area.To.Row - Area.From.Row + 1;
      Cols : constant Col_Type := Area.To.Col - Area.From.Col + 1;
      Image renames Object.Image.Constant_Reference.Element;
      procedure Filler (
         Background : in out RGBA_Type;
         Foreground : in out RGBA_Type;
         Position   : in     Cursor_Type)
      is
         Real_Y : constant Row_Type
                := Row_Type (
                   Integer (Position.Row - Area.From.Row)
                 * Integer (Object.Height) / 2
                 / Integer (Rows));
         Real_X : constant Col_Type
                := Col_Type (1 +
                   Integer (Position.Col - Area.From.Col)
                 * Integer (Object.Width)
                 / Integer (Cols));
      begin
         Foreground := Image (Real_Y * 2 + 1, Real_X);
         Background := Image (Real_Y * 2 + 2, Real_X);
      end Filler;
   begin
      if Object.Frames = 0 or else Object.Frame = 0 then
         return;
      end if;
      Surface.Fill (Area.From, Area.To, '▀');
      Surface.Fill (Filler'Access);
   end On_Draw;

   procedure Start_FFmpeg (
      Object : in out Video_Player_Widget)
   is
      Command : constant String :=
         "ffmpeg -hide_banner -loglevel error -i "                   &
         Escape_Name (Ada.Strings.Unbounded.To_String (Object.Path)) &
         " -c:v ppm -f rawvideo -an -";
   begin
      for I in Pipes'Range when not Using (I) loop
         Object.Id := I;
         Using (I) := True;
         exit;
      end loop;
      Buffs (Object.Id).Initialize (Input => Pipes (Object.Id)'Access,
                                    Size  => 65536);
      Pipes (Object.Id).Open (Command => Command,
                              Mode    => Util.Processes.READ);
   end Start_FFmpeg;

   procedure Load_Size (
      Object : in out Video_Player_Widget)
   is
      use Ada.Strings.Unbounded;
      Buffer renames Buffs (Object.Id);
      Char : Character;
      Str  : Unbounded_String;
   begin
      loop
         Buffer.Read (Char);
         exit when Char = ASCII.LF;
      end loop;

      loop
         Buffer.Read (Char);
         exit when Char = ' ';
         Append (Str, Char);
      end loop;
      Object.Width := Malef.Col_Type'Value (To_String (Str));

      Str := Null_Unbounded_String;
      loop
         Buffer.Read (Char);
         exit when Char = ASCII.LF;
         Append (Str, Char);
      end loop;
      Object.Height := Malef.Row_Type'Value (To_String (Str));

      loop
         Buffer.Read (Char);
         exit when Char = ASCII.LF;
      end loop;
   end Load_Size;

   procedure Load_Frame (
      Object : in out Video_Player_Widget)
   is
      use Malef;
      Buffer renames Buffs (Object.Id);
      Image renames Object.Image.Reference.Element;
      Char : Character;
   begin
      for Row in 1 .. Object.Height loop
         for Col in 1 .. Object.Width loop
            for Index in Red .. Blue loop
               Buffer.Read (Char);
               Image (Row, Col) (Index)
                  := Component_Type (Character'Pos (Char));
            end loop;
            Image (Row, Col) (Alpha) := 255;
         end loop;
      end loop;
   end Load_Frame;

   procedure Next_Frame (
      Object : in out Video_Player_Widget) is
   begin
      if Object.Frames = 0 or else Object.Frame >= Object.Frames then
         return;
      elsif Object.Frame = 0 then
         Start_FFmpeg (Object);
      end if;
      Load_Size (Object);
      Load_Frame (Object);
      Object.Frame := Object.Frame + 1;
   end Next_Frame;

   function Count_Frames (
      Path : in String)
      return Positive
   is
      Command : constant String :=
         "ffprobe -v error -select_streams v:0 -count_packets " &
         "-show_entries stream=nb_read_packets -of csv=p=0 "    &
         Escape_Name (Path);
      Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
      Buffer  : Util.Streams.Buffered.Input_Buffer_Stream;
      Frames  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Buffer.Initialize (Input => Pipe'Unchecked_Access, Size => 16);
      Pipe.Open (Command => Command, Mode => Util.Processes.READ);
      Buffer.Read (Into => Frames);
      declare
         Image : constant String := Ada.Strings.Unbounded.To_String (Frames);
      begin
         return Natural'Value (Image (Image'First .. Image'Last - 1));
      end;
   end Count_Frames;

end Video_Players;
