package AAA.Enum_Tools with Preelaborate is

   generic
      type Enum is (<>);
   function Is_Valid (Str : String) return Boolean;
   --  Check if a string can be Enum'Value'd without error.

end AAA.Enum_Tools;
