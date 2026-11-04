with AAA.Console_IO;

pragma Alire_Test (Should_Fail, False);

--  Reading from a terminal can't be tested here, as it needs one

procedure Aaa_Tests.Console_IO is

   use AAA.Console_IO;

   function C (Code : Natural) return Character is (Character'Val (Code));

begin
   Assert (Is_UTF_8 (""), "Empty");
   Assert (Is_UTF_8 ("plain ASCII"), "ASCII");

   Assert (Is_UTF_8 (C (16#C3#) & C (16#B1#)), "Two bytes");
   Assert (Is_UTF_8 (C (16#E2#) & C (16#82#) & C (16#AC#)), "Three bytes");
   Assert (Is_UTF_8 (C (16#F0#) & C (16#9F#) & C (16#98#) & C (16#80#)),
           "Four bytes");
   Assert (Is_UTF_8 ("a" & C (16#C3#) & C (16#B1#) & "b"), "Mixed");

   Assert (not Is_UTF_8 ("caf" & C (16#E9#)), "Latin-1");
   Assert (not Is_UTF_8 ("caf" & C (16#E9#) & "s"), "Latin-1 mid-text");
   Assert (not Is_UTF_8 ("caf" & C (16#C3#)), "Truncated");
   Assert (not Is_UTF_8 ("" & C (16#B1#)), "Continuation byte alone");

   --  Limits of each form, which must be accepted

   Assert (Is_UTF_8 ("" & C (16#7F#)), "Last one-byte");
   Assert (Is_UTF_8 (C (16#C2#) & C (16#80#)), "First two-byte");
   Assert (Is_UTF_8 (C (16#E0#) & C (16#A0#) & C (16#80#)),
           "First three-byte");
   Assert (Is_UTF_8 (C (16#ED#) & C (16#9F#) & C (16#BF#)),
           "Before surrogates");
   Assert (Is_UTF_8 (C (16#EE#) & C (16#80#) & C (16#80#)),
           "After surrogates");
   Assert (Is_UTF_8 (C (16#F0#) & C (16#90#) & C (16#80#) & C (16#80#)),
           "First four-byte");
   Assert (Is_UTF_8 (C (16#F4#) & C (16#8F#) & C (16#BF#) & C (16#BF#)),
           "U+10FFFF");
   Assert (Is_UTF_8 (C (16#EF#) & C (16#BB#) & C (16#BF#) & "BOM"), "BOM");

   --  Ill-formed sequences that a lax decoder lets through

   Assert (not Is_UTF_8 (C (16#C0#) & C (16#80#)), "Overlong NUL");
   Assert (not Is_UTF_8 (C (16#C1#) & C (16#BF#)), "Overlong two-byte");
   Assert (not Is_UTF_8 (C (16#E0#) & C (16#9F#) & C (16#BF#)),
           "Overlong three-byte");
   Assert (not Is_UTF_8 (C (16#F0#) & C (16#8F#) & C (16#BF#) & C (16#BF#)),
           "Overlong four-byte");
   Assert (not Is_UTF_8 (C (16#ED#) & C (16#A0#) & C (16#80#)),
           "High surrogate");
   Assert (not Is_UTF_8 (C (16#ED#) & C (16#BF#) & C (16#BF#)),
           "Low surrogate");
   Assert (not Is_UTF_8 (C (16#F4#) & C (16#90#) & C (16#80#) & C (16#80#)),
           "Beyond U+10FFFF");
   Assert (not Is_UTF_8 (C (16#F5#) & C (16#80#) & C (16#80#) & C (16#80#)),
           "Lead F5");
   Assert (not Is_UTF_8 ("" & C (16#FF#)), "Byte FF");
   Assert (not Is_UTF_8 (C (16#E2#) & C (16#82#) & "x"),
           "Bad third byte");
   Assert (not Is_UTF_8 (C (16#F0#) & C (16#9F#) & C (16#98#)),
           "Truncated four-byte");

   --  Sequence lengths, also in a slice not starting at 1

   declare
      Text : constant String (11 .. 18) :=
               "a" & C (16#C3#) & C (16#B1#) & C (16#E2#) & C (16#82#)
               & C (16#AC#) & C (16#FF#) & "z";
   begin
      Assert (UTF_8_Sequence_Length (Text, 11) = 1, "Length ASCII");
      Assert (UTF_8_Sequence_Length (Text, 12) = 2, "Length two-byte");
      Assert (UTF_8_Sequence_Length (Text, 13) = 0, "Length continuation");
      Assert (UTF_8_Sequence_Length (Text, 14) = 3, "Length three-byte");
      Assert (UTF_8_Sequence_Length (Text, 17) = 0, "Length FF");
      Assert (UTF_8_Sequence_Length (Text, 18) = 1, "Length last");
      Assert (UTF_8_Sequence_Length (Text, 15) = 0, "Length mid-sequence");
   end;
end Aaa_Tests.Console_IO;
