private package Malef.Subsystem is

   type Key_Type is new Glyph;

   -- protected Terminal is

   --    procedure Begin_Frame;

   --    procedure End_Frame;

   --    procedure Set_Title (
   --       Item : in String);

   --    procedure Move_To (
   --       Row : in Positive_Row_Count;
   --       Col : in Positive_Col_Count);

   --    procedure Format (
   --       Background : in RGBA_Type;
   --       Foregroudn : in RGBA_Type;
   --       Style      : in Style_Type);

   --    procedure Put (
   --       Item : in Glyph_String);

   --    procedure Get (
   --       Key       : out Key_Type;
   --       Available : out Boolean);

   -- end Terminal;

   procedure Initialize;

   procedure Finalize;

end Malef.Subsystem;