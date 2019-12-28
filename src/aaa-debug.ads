package AAA.Debug with Preelaborate is

   function Stack_Trace return String;
   --  Return a string containing the call stack. To keep things Preelaborable,
   --  a fake exception is generated and the exception information retrieved.
   --  Hence efficacy of this will be dependent on your compilation switches.

end AAA.Debug;
