with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.IO_Aux;

with Interfaces.C_Streams;

package body AAA.Console_IO is

   -----------
   -- Flush --
   -----------

   procedure Flush is
      use Interfaces.C_Streams;
      Unused : constant int := fflush (stdout);
   begin
      null;
   end Flush;

   --------------
   -- Get_Line --
   --------------

   function Get_Line return String is
   begin
      Flush;

      --  This relies on Ada.Text_IO.Get_Line, which keeps bytes as they are
      return GNAT.IO_Aux.Get_Line;
   end Get_Line;

   -------------------
   -- Discard_Input --
   -------------------

   procedure Discard_Input is
      use Interfaces.C_Streams;

      procedure Getc_Immediate_Nowait (Stream      : FILEs;
                                       Ch          : out int;
                                       End_Of_File : out int;
                                       Avail       : out int);
      pragma Import (C, Getc_Immediate_Nowait, "getc_immediate_nowait");
      --  From the GNAT runtime, as used by Ada.Text_IO.Get_Immediate, which is
      --  the only read that doesn't wait in the standard library. Using it
      --  directly avoids its decoding: under -gnatW8, a stray byte would wait
      --  for the rest of a character, swallowing what is typed next.

      Ch, End_Of_File, Available : int;
   begin
      loop
         Getc_Immediate_Nowait (stdin, Ch, End_Of_File, Available);

         exit when ferror (stdin) /= 0
           or else End_Of_File /= 0
           or else Available = 0;
      end loop;
   end Discard_Input;

   --------------
   -- Is_UTF_8 --
   --------------

   function Is_UTF_8 (Text : String) return Boolean is
   begin
      declare
         Unused : constant Wide_Wide_String :=
                    Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode (Text);
      begin
         return True;
      end;
   exception
      when Ada.Strings.UTF_Encoding.Encoding_Error =>
         return False;
   end Is_UTF_8;

end AAA.Console_IO;
