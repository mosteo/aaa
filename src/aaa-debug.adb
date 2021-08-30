with GNAT.IO;

package body AAA.Debug is

   -----------
   -- Image --
   -----------

   function Image (E : Ada.Exceptions.Exception_Occurrence) return String
   is
      use Ada.Exceptions;
      use ASCII;
   begin
      return
        "EXCEPTION NAME" & LF & Exception_Name (E) & LF
        & "EXCEPTION MESSAGE" & LF & Exception_Message (E) & LF
        & "EXCEPTION INFORMATION" & LF & Exception_Information (E);
   end Image;

   -------------------
   -- Put_Exception --
   -------------------

   procedure Put_Exception (E           : Ada.Exceptions.Exception_Occurrence;
                            Title       : String := "AAA EXCEPTION DUMP:";
                            Stack_Trace : Boolean := True)
   is
      use GNAT.IO;
   begin
      Put_Line (Title & ASCII.LF
                & Image (E));
      if Stack_Trace then
         Put_Line (Debug.Stack_Trace);
      end if;
   end Put_Exception;

   -----------------
   -- Stack_Trace --
   -----------------

   function Stack_Trace return String is
      Debug_Exception : exception;
   begin
      raise Debug_Exception;
   exception
      when E : Debug_Exception =>
         return Ada.Exceptions.Exception_Information (E);
   end Stack_Trace;

end AAA.Debug;
