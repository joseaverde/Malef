with Malef.Generic_Suites;
with Malef.Test_Cases;
with Malef.Surfaces;

package Malef.Groups.Tests is

   type Test is
      new Malef.Test_Cases.Test_Case with
      record
         Surface_A : Surfaces.Surface (4, 8);
         Surface_B : Surfaces.Surface (7, 3);
         Surface_C : Surfaces.Surface (5, 5);
         Surface_D : Surfaces.Surface (1, 1);
         Surface_E : Surfaces.Surface (2, 1);
         Group_A   : Group (4);
         Group_B   : Group (3);
         The_Group : Group (7);
      end record;

   overriding
   procedure Set_Up (T : in out Test);

   procedure Test_Group_Layer_Add_Unnamed (T : in out Test);
   procedure Test_Group_Layer_Assign_Indexed (T : in out Test);
   procedure Test_Group_Layer_Ownership (T : in out Test);
   procedure Test_Group_Layer_Not_Multiple_Owners (T : in out Test);
   procedure Test_Group_Layer_Reference_Counted (T : in out Test);
   procedure Test_Group_Layer_Group_Moved (T : in out Test);
   procedure Test_Group_Layer_Not_Moved_To_Itself (T : in out Test);

   procedure Test_Groups_No_Tampering (T : in out Test);
   procedure Test_Groups_References_Keep_References (T : in out Test);
   procedure Test_Groups_Reference_Counted (T : in out Test);

   package Suites is new Malef.Generic_Suites (Test, "Groups");
   use Suites;
   function Suite is new Suites.Suite (
      [
      -- Group Elements

         (Test_Group_Layer_Add_Unnamed'Access,
          +"Group.Add_Unnamed (Group_Layer)")
      ,  (Test_Group_Layer_Assign_Indexed'Access,
          +"Group.Assign_Indexed (Group_Layer)")
      ,  (Test_Group_Layer_Ownership'Access,
          +"Adding a Group_Layer to a group makes it take ownership")
      ,  (Test_Group_Layer_Not_Multiple_Owners'Access,
          +"A Group_Layer cannot have multiple Onwers at the same time")
      ,  (Test_Group_Layer_Reference_Counted'Access,
          +"The Group_Layer objects are reference counted")
      ,  (Test_Group_Layer_Group_Moved'Access,
          +"The Group_Layer allows moving a Group without copying")
      ,  (Test_Group_Layer_Not_Moved_To_Itself'Access,
          +"A Group cannot be moved to itself")

      -- Groups

      ,  (Test_Groups_No_Tampering'Access,
          +"A Group cannot be tampered with")
      ,  (Test_Groups_References_Keep_References'Access,
          +"A reference to an element in the group keeps a reference")
      ,  (Test_Groups_Reference_Counted'Access,
          +"Groups are reference counted")
      ]);

end Malef.Groups.Tests;
