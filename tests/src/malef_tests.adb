with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Run;

with Malef.Suite;

procedure Malef_Tests is

   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Options  : constant AUnit.Options.AUnit_Options := (
      Global_Timer     => True,
      Test_Case_Timer  => True,
      Report_Successes => True,
      Filter           => null);

   procedure Run is new AUnit.Run.Test_Runner (Malef.Suite.Suite);

begin

   Reporter.Set_Use_ANSI_Colors (True);
   Run (Reporter, Options);

end Malef_Tests;
