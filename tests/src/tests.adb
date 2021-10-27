with AUnit; use AUnit;
with AUnit.Run;
with AUnit.Reporter.Text;

with GNAT.OS_Lib;

with Test_Cases;
with Test_Cases.Strings;
with Test_Cases.Strings.Vector;

procedure Tests is
   function Runner is new AUnit.Run.Test_Runner_With_Status (Test_Cases.Get_Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Reporter.Set_Use_ANSI_Colors (True);

   if Runner (Reporter,
              (Global_Timer     => True,
               Test_Case_Timer  => True,
               Report_Successes => True,
               others           => <>))
     /= AUnit.Success
   then
      GNAT.OS_Lib.OS_Exit (1);
   end if;


end Tests;
