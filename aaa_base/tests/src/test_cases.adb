package body Test_Cases is

   ---------------
   -- Get_Suite --
   ---------------

   function Get_Suite return AUnit.Test_Suites.Access_Test_Suite
   is (Suite'Access);

end Test_Cases;
