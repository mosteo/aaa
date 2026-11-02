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
end Aaa_Tests.Console_IO;
