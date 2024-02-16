with Ada.Directories;
with Malef.Widgets;
with Malef.Surfaces;
private with Ada.Strings.Unbounded;

package File_Explorers is

   type File_Explorer_Widget is
      new Malef.Widgets.Widget with
      private;

   function New_File_Explorer (
      Path : in String)
      return File_Explorer_Widget with
      Pre      => Ada.Directories.Exists (Path)
         and then Ada.Directories."=" (Ada.Directories.Kind (Path),
                                       Ada.Directories.Directory);

   overriding
   procedure On_Draw (
      Object  : in     File_Explorer_Widget;
      Surface : in out Malef.Surfaces.Surface;
      Area    : in     Malef.Widgets.Draw_Area);

private

   type File_Explorer_Widget is
      new Malef.Widgets.Widget with
      record
         Path : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function New_File_Explorer (
      Path : in String)
      return File_Explorer_Widget is (
      Malef.Widgets.Widget with
      Path => Ada.Strings.Unbounded.To_Unbounded_String (Path));

end File_Explorers;
