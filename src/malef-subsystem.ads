private package Malef.Subsystem with Preelaborate is

   procedure Begin_Frame;

   procedure End_Frame;

   procedure Put_Background (Item : in RGBA_Type);

   procedure Put_Foreground (Item : in RGBA_Type);

   procedure Put_Style (Item : in Style_Type);

end Malef.Subsystem;
