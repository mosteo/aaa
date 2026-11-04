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
      Pos : Positive := Text'First;
      Len : Natural;
   begin
      while Pos <= Text'Last loop
         Len := UTF_8_Sequence_Length (Text, Pos);
         if Len = 0 then
            return False;
         end if;
         Pos := Pos + Len;
      end loop;

      return True;
   end Is_UTF_8;

   ---------------------------
   -- UTF_8_Sequence_Length --
   ---------------------------

   function UTF_8_Sequence_Length (Text  : String;
                                   First : Positive)
                                   return Natural
   is
      subtype Byte is Natural range 0 .. 255;

      function At_Offset (Offset : Natural) return Byte
      is (Character'Pos (Text (First + Offset)));

      function Fits (Length : Positive) return Boolean
      is (Text'Last - First >= Length - 1);

      function Is_Continuation (Offset : Natural) return Boolean
      is (At_Offset (Offset) in 16#80# .. 16#BF#);

      Lead : constant Byte := At_Offset (0);

      --  The range of the second byte depends on the lead byte, to exclude
      --  overlong forms, surrogates and code points beyond U+10FFFF.

      Low  : Byte := 16#80#;
      High : Byte := 16#BF#;
      Len  : Positive;
   begin
      case Lead is
         when 16#00# .. 16#7F# => return 1;
         when 16#C2# .. 16#DF# => Len := 2;
         when 16#E0#           => Len := 3; Low  := 16#A0#;
         when 16#E1# .. 16#EC# => Len := 3;
         when 16#ED#           => Len := 3; High := 16#9F#;
         when 16#EE# .. 16#EF# => Len := 3;
         when 16#F0#           => Len := 4; Low  := 16#90#;
         when 16#F1# .. 16#F3# => Len := 4;
         when 16#F4#           => Len := 4; High := 16#8F#;
         when others           => return 0;
            --  80 .. BF: continuation without lead
            --  C0 .. C1: always overlong
            --  F5 .. FF: beyond U+10FFFF or never used
      end case;

      if not Fits (Len) or else At_Offset (1) not in Low .. High then
         return 0;
      end if;

      for Offset in 2 .. Len - 1 loop
         if not Is_Continuation (Offset) then
            return 0;
         end if;
      end loop;

      return Len;
   end UTF_8_Sequence_Length;

end AAA.Console_IO;
