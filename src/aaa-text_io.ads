with Ada.Text_IO;

package AAA.Text_IO is

   subtype Line_Widths is Positive range 2 .. Positive'Last;
   --  We need to at least be able to
   --  w-
   --  r-
   --  i-
   --  t-
   --  e like this.

   Default_Line_Width : constant := 79;

   type Filling_Modes is (Greedy);
   --  More fancy modes exist, not implemented for now.

   procedure Put_Paragraph (Text        : String;
                            Line_Width  : Line_Widths := Default_Line_Width;
                            Line_Prefix : String := "";
                            Filling     : Filling_Modes := Greedy;
                            File        : Ada.Text_IO.File_Access :=
                              Ada.Text_Io.Standard_Output);
   --  Reformat Text and write it to the given File.
   --  Lines will be broken at either spaces or '-'.
   --  If Line_Width is too short for a word or syllable, the word will be
   --  mercilessly broken wherever the line is completed.
   --  Line_Prefix is prepended to all lines, only if
   --  Line_Prefix'Length < Line_Width - 2.
   --  Caveat: at least one full line will be allocated.

end AAA.Text_IO;
