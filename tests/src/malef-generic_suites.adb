with AUnit.Test_Caller;
with AUnit.Assertions;

package body Malef.Generic_Suites is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
      for Cas of Cases loop
         Result.Add_Test (Caller.Create (Name & " :: " & To_String (Cas.Name),
                                         Caller.Test_Method (Cas.Method)));
      end loop;
      return Result;
   end Suite;

   Test_Not_Implemented : exception with Unreferenced;

   procedure Not_Implemented (T : in out Test_Type) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (True, "Not implemented");
   end Not_Implemented;

end Malef.Generic_Suites;
