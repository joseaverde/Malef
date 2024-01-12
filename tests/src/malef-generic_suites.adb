with AUnit.Test_Caller;

package body Malef.Generic_Suites is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
      for Cas of Cases loop
         Result.Add_Test (Caller.Create (Name & " :: " & To_String (Cas.Name),
                                         Caller.Test_Method (Cas.Method)));
      end loop;
      return Result;
   end Suite;

end Malef.Generic_Suites;
