with Malef.Terminfo.Tests;
with Malef.Groups.Tests;

package body Malef.Suite is

   Result : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      -- Result.Add_Test (Malef.Terminfo.Tests.Suite);
      Result.Add_Test (Malef.Groups.Tests.Suite);
      return Result'Access;
   end Suite;

end Malef.Suite;
