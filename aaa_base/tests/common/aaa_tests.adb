package body Aaa_Tests is

   ------------
   -- Assert --
   ------------

   procedure Assert (Cond : Boolean; Text : String) is
   begin
      pragma Assert (Cond, Text);
   end Assert;

end Aaa_Tests;
