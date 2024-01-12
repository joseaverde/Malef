with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

generic
   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;
   Name : in String;
package Malef.Generic_Suites is

   type Test_Method is not null access procedure (T : in out Test_Type);

   type Test_Case is
      record
         Method : Test_Method;
         Name   : Unbounded_String;
      end record;

   type Test_Case_Array is array (Positive range <>) of Test_Case;

   function "+" (Right : in String)
      return Unbounded_String
      renames To_Unbounded_String;

   generic
      Cases : in Test_Case_Array;
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Malef.Generic_Suites;
