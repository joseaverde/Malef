with Ada.Text_IO;

package body Malef.Terminfo.Tests is

   use Test_Cases.Assertions;

   procedure Test_ADM_3_Definition (T : in out Test) is
      Term : Term_Type;
   begin
      Open (Term, "db/a/adm3a");
      Ada.Text_IO.Put_Line (Term'Image);
      Assert (True, "True");
      Not_Implemented (T);
   end Test_ADM_3_Definition;

   procedure Test_ANSI_Definition (T : in out Test) is
      Term : Term_Type;
   begin
      Open (Term, "db/a/ansi");
      Ada.Text_IO.Put_Line (Term'Image);
      Assert (True, "True");
      Not_Implemented (T);
   end Test_ANSI_Definition;

   procedure Test_Searchs_Environment_Variable_First (T : in out Test)
      renames Not_Implemented;
   procedure Test_Searchs_Current_Found (T : in out Test)
      renames Not_Implemented;
   procedure Test_Searchs_Current_Not_Found (T : in out Test)
      renames Not_Implemented;
   procedure Test_Searchs_Directory_List (T : in out Test)
      renames Not_Implemented;
   procedure Test_Searchs_Not_Found (T : in out Test)
      renames Not_Implemented;

end Malef.Terminfo.Tests;
