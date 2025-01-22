package body AAA.Enum_Tools is

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Str : String) return Boolean is
   begin
      declare
         E : constant Enum := Enum'Value (Str) with Unreferenced;
      begin
         return True;
      end;
   exception
      when others =>
         return False;
   end Is_Valid;

end AAA.Enum_Tools;
