package body Malef.Groups.Tests is

   Black   : constant RGBA_Type := (0, 0, 0, 255);
   Red     : constant RGBA_Type := (255, 0, 0, 255);
   Green   : constant RGBA_Type := (0, 255, 0, 255);
   Blue    : constant RGBA_Type := (0, 0, 255, 255);
   Yellow  : constant RGBA_Type := (255, 255, 0, 255);
   Magenta : constant RGBA_Type := (255, 0, 255, 255);

   overriding
   procedure Set_Up (T : in out Test) is
   begin
      T.Group_A := Empty (T.Group_A.Capacity);
      T.Group_B := Empty (T.Group_B.Capacity);
      T.The_Group := Empty (T.The_Group.Capacity);

      T.Surface_A.Fill_Background (Red);
      T.Surface_A.Fill_Foreground (Black);
      T.Surface_A.Fill ('A');

      T.Surface_B.Fill_Background (Green);
      T.Surface_B.Fill_Foreground (Black);
      T.Surface_B.Fill ('B');

      T.Surface_C.Fill_Background (Blue);
      T.Surface_C.Fill_Foreground (Black);
      T.Surface_C.Fill ('C');

      T.Surface_D.Fill_Background (Yellow);
      T.Surface_D.Fill_Foreground (Black);
      T.Surface_D.Fill ('D');

      T.Surface_D.Fill_Background (Magenta);
      T.Surface_D.Fill_Foreground (Black);
      T.Surface_D.Fill ('E');
   end Set_Up;

   use Test_Cases.Assertions;
   use all type Surfaces.Surface;
   use all type Atomic_Boolean;

   -->> Group_Layer Tests <<--

   procedure Test_Group_Layer_Add_Unnamed (T : in out Test) is
      Item : Group_Layer;
   begin
      T.The_Group := Empty (7);
      Assert (T.The_Group.Size = 0, "Initial size must be 0");
      Assert (T.The_Group.Add_Unnamed_Index = 0, "Initial Unnamed Index not 0");
      Assert (T.The_Group.Last_Index = 0, "Initial Last_Index must be 0");

      Item := Group_Layer (Layer (T.Surface_A));
      T.The_Group.Add_Unnamed (Item);
      Assert (T.The_Group.Size = 1, "The size must increase in 1 to be 1");
      Assert (T.The_Group.Add_Unnamed_Index = 1, "The Unnamed Index not 1");
      Assert (T.The_Group.Last_Index = 1, "The last Index not 1");
      Assert (T.The_Group.Layers (1) = Item.Layer, "The pointers don't match");
      Assert (T.The_Group.Layers (1).Kind = A_Surface, "It must be a surface");
      Assert (T.The_Group.Layers (1).Surface.all = T.Surface_A,
              "The Surface objects differ");

      Item := Group_Layer (Layer (T.Group_B));
      T.The_Group.Add_Unnamed (Item);
      Assert (T.The_Group.Size = 2, "The size doesn't increase to be 2");
      Assert (T.The_Group.Add_Unnamed_Index = 2, "The Unnamed Index not 2");
      Assert (T.The_Group.Last_Index = 2, "The last Index not 2");
      Assert (T.The_Group.Layers (2) = Item.Layer, "The pointers don't match");
      Assert (T.The_Group.Layers (2).Kind = A_Group, "It must be a group");
      Assert (T.The_Group.Layers (2).Group.Capacity = T.Group_B.Capacity,
              "Groups capacities differ");

      Item := Group_Layer (No_Layer);
      T.The_Group.Add_Unnamed (Item);
      Assert (T.The_Group.Size = 2, "The size must be the same (No_Layer)");
      Assert (T.The_Group.Add_Unnamed_Index = 3, "The index must increase");
      Assert (T.The_Group.Last_Index = 2, "Last_Index must not change");
      Assert (Item.Layer = null, "No_Layer => Item.Layer = null");
      Assert (T.The_Group.Layers (3) = Item.Layer, "Added null");

      for I in Layer_Index range 4 .. 7 loop
         Item := Group_Layer (Layer (T.Surface_D));
         T.The_Group.Add_Unnamed (Item);
         Assert (T.The_Group.Size = I - 1, "The size doesn't match");
         Assert (T.The_Group.Add_Unnamed_Index = I, "Wrong Unnamed Index");
         Assert (T.The_Group.Last_Index = I, "Last Index must increase");
         Assert (T.The_Group.Layers (I) = Item.Layer, "Pointers differ");
         Assert (T.The_Group.Layers (I).Kind = A_Surface, "Must be A_Surface");
         Assert (T.The_Group.Layers (I).Surface.all = T.Surface_D,
                 "Objects differ");
      end loop;

      T.The_Group.Add_Unnamed (No_Layer);
      Assert (False, "Constraint_Error should be raised!");
   exception
      when Constraint_Error =>
      declare
         My_Group : constant Group (7) :=
            [Layer (T.Surface_A), Layer (T.Surface_B), Layer (T.Group_A),
             Layer (T.Surface_C), Layer (T.Surface_D), Layer (T.Group_B),
             Layer (T.Surface_E)];
      begin
         Assert (My_Group.Size = 7, "Group not at max capacity");
         Assert (My_Group.Add_Unnamed_Index = 7, "Index must be last");
         Assert (My_Group.Last_Index = 7, "Wront last index");
         Assert (My_Group.Layers (1).Surface.all = T.Surface_A, "Wrong at 1");
         Assert (My_Group.Layers (2).Surface.all = T.Surface_B, "Wrong at 2");
         Assert (My_Group.Layers (3).Group.Capacity = T.Group_A.Capacity,
                 "Wrong at 3");
         Assert (My_Group.Layers (4).Surface.all = T.Surface_C, "Wrong at 4");
         Assert (My_Group.Layers (5).Surface.all = T.Surface_D, "Wrong at 5");
         Assert (My_Group.Layers (6).Group.Capacity = T.Group_B.Capacity,
                 "Wrong at 6");
         Assert (My_Group.Layers (7).Surface.all = T.Surface_E, "Wrong at 7");
      end;
   end Test_Group_Layer_Add_Unnamed;

   procedure Test_Group_Layer_Assign_Indexed (T : in out Test)
      renames Not_Implemented;

   procedure Test_Group_Layer_Ownership (T : in out Test) is
      Item_A : Group_Layer;
      Item_B : Group_Layer;
      Item_C : Group_Layer;
   begin
      Item_A := Group_Layer (Layer (T.Surface_A));
      Assert (Item_A.Control /= null and then
              Item_A.Control.Owned = 0,
              "When created, it must not have an owner (Item_A)");
      Item_B := Group_Layer (Layer (T.Group_B));
      Assert (Item_B.Control /= null and then
              Item_B.Control.Owned = 0,
              "When created, it must not have an owner (Item_B)");
      Item_C := Group_Layer (No_Layer);
      Assert (Item_C.Control = null, "No_Layer has null control");
      T.Group_B.Add_Unnamed (Item_A);
      Assert (Item_A.Control.Owned /= 0, "Item_A should be owned by the Group");
      T.Group_B.Add_Unnamed (Item_B);
      Assert (Item_B.Control.Owned /= 0, "Item_B should be owned by the Group");
      T.Group_B.Add_Unnamed (Item_C);
      Assert (Item_C.Control = null, "Item_C should be still be null");
   end Test_Group_Layer_Ownership;

   procedure Test_Group_Layer_Not_Multiple_Owners (T : in out Test) is
      Item : Group_Layer;
   begin
      Item := Group_Layer (Layer (T.Surface_A));
      Assert (Item.Control.Owned = 0, "On creation it, no owner");
      T.Group_A.Add_Unnamed (Item);
      Assert (Item.Control.Owned /= 0, "Now the layer should have an owner");
      T.Group_B.Add_Unnamed (Item);
      Assert (False, "It has two owners!!!");
   exception
      when Program_Error => null;
   end Test_Group_Layer_Not_Multiple_Owners;

   procedure Test_Group_Layer_Reference_Counted (T : in out Test) is
   begin
      declare
         Item  : Group_Layer;
      begin
         Assert (Item.Control = null, "Not yet assigned!");
         Item := Group_Layer (Layer (T.Surface_A));
         Assert (Item.Control /= null, "Assigned");
         Assert (Item.Control.Counter = 1, "Only one object referencing it");
         declare
            Other   : Group_Layer;
            Other_2 : Group_Layer;
            Nope    : Group_Layer;
         begin
            Other := Item;
            Assert (Item.Control.Counter = 2, "There should be 2 references");
            Other_2 := Other;
            Assert (Item.Control.Counter = 3, "There should be 3 references");
            Other := Nope;
            Assert (Item.Control.Counter = 2, "There should be 2 references");
            Other := Other_2;
            Assert (Item.Control.Counter = 3, "There should be 3 references");
         end;
         Assert (Item.Control.Counter = 1, "Two references less => 1 ref!");
      end;
   end Test_Group_Layer_Reference_Counted;

   procedure Test_Group_Layer_Group_Moved (T : in out Test)
      renames Not_Implemented;
   procedure Test_Group_Layer_Not_Moved_To_Itself (T : in out Test)
      renames Not_Implemented;

   -->> Group Tests <<--

   procedure Test_Groups_No_Tampering (T : in out Test)
      renames Not_Implemented;
   procedure Test_Groups_References_Keep_References (T : in out Test)
      renames Not_Implemented;
   procedure Test_Groups_Are_Freed (T : in out Test)
      renames Not_Implemented;
   procedure Test_Groups_Reference_Counted (T : in out Test)
      renames Not_Implemented;

end Malef.Groups.Tests;
