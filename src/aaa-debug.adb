with Ada.Exceptions;

package body AAA.Debug is

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
