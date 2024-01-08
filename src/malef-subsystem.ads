private package Malef.Subsystem with Preelaborate is

   procedure Begin_Frame is null;

   procedure End_Frame is null;

   procedure Put_Background (Item : in RGBA_Type) is null;

   procedure Put_Foreground (Item : in RGBA_Type) is null;

   procedure Put_Style (Item : in Style_Type) is null;

end Malef.Subsystem;
