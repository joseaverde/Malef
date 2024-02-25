with Ada.Directories;
with Malef.Widgets;
with Malef.Surfaces;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Holders;

package Video_Players is

   type Video_Player_Widget is
      new Malef.Widgets.Widget with
      private;

   function New_Video_Player (
      Path : in String)
      return Video_Player_Widget with
      Pre      => Ada.Directories.Exists (Path)
         and then Ada.Directories."=" (Ada.Directories.Kind (Path),
                                       Ada.Directories.Ordinary_File);

   overriding
   procedure On_Draw (
      Object  : in     Video_Player_Widget;
      Surface : in out Malef.Surfaces.Surface;
      Area    : in     Malef.Widgets.Draw_Area);

   overriding
   function Name (
      Object : in Video_Player_Widget)
      return Wide_Wide_String is (
      "Video_Player");

   function Current_Frame (
      Object : in Video_Player_Widget)
      return Natural;

   function Frame_Count (
      Object : in Video_Player_Widget)
      return Natural;

   procedure Next_Frame (
      Object : in out Video_Player_Widget);

private

   Max_Height : constant := 2048;
   Max_Width  : constant := Max_Height * 2;

   type Frame_Type is
      array (Malef.Row_Type range 1 .. Max_Height,
             Malef.Col_Type range 1 .. Max_Width)
      of Malef.RGBA_Type;

   Empty : Frame_Type;

   package Frame_Holders is
      new Ada.Containers.Indefinite_Holders (
      Element_Type => Frame_Type);

   type Video_Player_Widget is
      new Malef.Widgets.Widget with
      record
         Path   : Ada.Strings.Unbounded.Unbounded_String;
         Frames : Natural := 0;
         Frame  : Natural := 0;
         Image  : Frame_Holders.Holder;
         Height : Malef.Row_Type;
         Width  : Malef.Col_Type;
         Id     : Natural := 0;
      end record;

   function Count_Frames (
      Path : in String)
      return Positive;

   function New_Video_Player (
      Path : in String)
      return Video_Player_Widget is (
      Malef.Widgets.Widget with
      Path   => Ada.Strings.Unbounded.To_Unbounded_String (Path),
      Frames => Count_Frames (Path),
      Image  => Frame_Holders.To_Holder (Empty),
      others => <>);

   function Current_Frame (
      Object : in Video_Player_Widget)
      return Natural is (
      Object.Frame);

   function Frame_Count (
      Object : in Video_Player_Widget)
      return Natural is (
      Object.Frames);

end Video_Players;
