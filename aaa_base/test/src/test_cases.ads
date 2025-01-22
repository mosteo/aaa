with AUnit.Test_Suites;

private with AUnit.Test_Fixtures;
private with AUnit.Test_Caller;

package Test_Cases is

   function Get_Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   type Null_Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

   package Null_Caller is new AUnit.Test_Caller (Null_Fixture);

   Suite : aliased AUnit.Test_Suites.Test_Suite;

end Test_Cases;
