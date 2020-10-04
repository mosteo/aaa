with AAA.Strings;

package AAA.Processes is

   Child_Error : exception;

   type Result is record
      Exit_Code : aliased Integer;
      Output    : Strings.Vector;
   end record;

   function Run (Command_Line   : Strings.Vector;
                 Input          : String := "";
                 Err_To_Out     : Boolean := False;
                 Raise_On_Error : Boolean := True)
                 return Result;
   --  Run a command, giving optional Input to it, and capture its output,
   --  optionally including stderr output. If the child's process exit code is
   --  /= 0, Child_Error will be raised when Raise_On_Error. CR & LF sequences
   --  will be interpreted as plain LF sequences.

end AAA.Processes;
