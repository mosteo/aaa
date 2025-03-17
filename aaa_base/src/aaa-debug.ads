with Ada.Exceptions;

package AAA.Debug with Preelaborate is

   function Image (E : Ada.Exceptions.Exception_Occurrence) return String;
   --  Just concatenate the exception name, message and information

   procedure Put_Exception (E           : Ada.Exceptions.Exception_Occurrence;
                            Title       : String := "AAA EXCEPTION DUMP:";
                            Stack_Trace : Boolean := True);
   --  Print Image and optionally Stack_Trace

   function Stack_Trace return String;
   --  Return a string containing the call stack. To keep things Preelaborable,
   --  a fake exception is generated and the exception information retrieved.
   --  Hence efficacy of this will be dependent on your compilation switches.

end AAA.Debug;
