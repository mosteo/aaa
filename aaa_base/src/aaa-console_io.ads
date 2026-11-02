with GNAT.IO;

package AAA.Console_IO is

   --  Text in the standard streams as the bytes it is made of, whatever the
   --  encoding of the main program. Output is expected to be already in UTF-8,
   --  and input is whatever the terminal sends.
   --
   --  This isn't Ada.Text_IO or its wide variants because their encoding at
   --  run time depends on the main program being compiled with -gnatW8 or not,
   --  and using that switch for a library has no effect on this:
   --
   --  * With the switch, Ada.Text_IO takes each byte of a String for a Latin-1
   --    character and encodes it again, garbling text already in UTF-8.
   --  * Without the switch, Ada.Wide_Wide_Text_IO takes each byte it reads for
   --    a whole character, garbling input in UTF-8.
   --  * With the switch, Ada.Wide_Wide_Text_IO.Get_Line raises on invalid
   --    UTF-8, leaving the rest of the line to be read as the next line.
   --  * In GNAT, Get_Immediate of the wide variants waits for input even when
   --    it has an Available parameter, so it can't be used to discard it.

   procedure Put (Text : String) renames GNAT.IO.Put;

   procedure Put_Line (Text : String) renames GNAT.IO.Put_Line;

   procedure New_Line (Spacing : Positive := 1) renames GNAT.IO.New_Line;

   procedure Flush;
   --  Show any output pending in standard output, like a line not terminated

   function Get_Line return String;
   --  Show pending output and read a line from standard input, with no limit
   --  on its length. The line has the same bytes that were read, which aren't
   --  necessarily UTF-8, e.g., in a Windows console; see Is_UTF_8 to vet them.
   --  Raises Ada.IO_Exceptions.End_Error when there is no more input.

   procedure Discard_Input;
   --  Discard whatever is pending in standard input without waiting for more.
   --  This is meant for terminals; on a pipe it waits for the end of input.

   function Is_UTF_8 (Text : String) return Boolean;
   --  Say if Text is valid UTF-8

end AAA.Console_IO;
