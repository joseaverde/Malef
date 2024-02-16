with AUnit.Test_Fixtures;
with AUnit.Assertions;

package Malef.Test_Cases is

   package Assertions renames AUnit.Assertions;

   type Test_Case is abstract
      new AUnit.Test_Fixtures.Test_Fixture with
      null record;

end Malef.Test_Cases;
