with Malef.Generic_Suites;
with Malef.Test_Cases;

package Malef.Terminfo.Tests is

   type Test is new Malef.Test_Cases.Test_Case with null record;

   procedure Test_ADM_3_Definition (T : in out Test);
   procedure Test_ANSI_Definition (T : in out Test);

   procedure Test_Searchs_Environment_Variable_First (T : in out Test);
   procedure Test_Searchs_Current_Found (T : in out Test);
   procedure Test_Searchs_Current_Not_Found (T : in out Test);
   procedure Test_Searchs_Directory_List (T : in out Test);
   procedure Test_Searchs_Not_Found (T : in out Test);

   package Suites is new Malef.Generic_Suites (Test, "Terminfo");
   use Suites;
   function Suite is new Suites.Suite (
      [
      -- Loader

         (Test_ADM_3_Definition'Access,
          +"Description for Lear-Siegler ADM-3 terminal")
      ,  (Test_ANSI_Definition'Access,
          +"Description for ANSI-compliant terminals")

      -- Searches

      ,  (Test_Searchs_Environment_Variable_First'Access,
          +"Search Environment Variable First")
      ,  (Test_Searchs_Current_Found'Access,
          +"Search and Find Current Terminal")
      ,  (Test_Searchs_Current_Not_Found'Access,
          +"Search missing Current Terminal")
      ,  (Test_Searchs_Directory_List'Access,
          +"Searchs Environmentan Variable with paths")
      ,  (Test_Searchs_Not_Found'Access,
          +"Search missing Terminal")
      ]);

end Malef.Terminfo.Tests;
