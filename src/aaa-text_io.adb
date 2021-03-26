with AAA.ANSI;

with Ada.Strings.Unbounded;

package body AAA.Text_IO is

   -------------------
   -- Put_Paragraph --
   -------------------

   procedure Put_Paragraph (Text        : String;
                            Line_Width  : Line_Widths := Default_Line_Width;
                            Line_Prefix : String := "";
                            Filling     : Filling_Modes := Greedy;
                            File        : Ada.Text_IO.File_Access :=
                              Ada.Text_IO.Standard_Output)
   is
      pragma Unreferenced (Filling);
      use Ada.Text_IO;

      Pos : Integer := Text'First;

      ---------------
      -- Next_Word --
      ---------------

      function Next_Word return String is
      begin
         for I in Pos .. Text'Last loop
            if Text (I) = ' ' then
               return Text (Pos .. I - 1);
            elsif Text (I) = '-' then
               return Text (Pos .. I);
            end if;
         end loop;

         --  No breaker found...
         return Text (Pos .. Text'Last);
      end Next_Word;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line is
         use Ada.Strings.Unbounded;

         Line : Unbounded_String;
         --  Doing this with fixed strings and ANSI unknown extra lengths is
         --  unnecessary trouble.

         ----------
         -- Used --
         ----------

         function Used return Natural
         is (ANSI.Length (To_String (Line)));

         PPos : constant Integer := Pos;
         --  Initial Pos, to check we had some progress

         ---------
         -- Put --
         ---------

         procedure Put (Word : String) is
         begin
            Append (Line, Word);
         end Put;

      begin
         --  Set prefix, if it fits
         if Line_Prefix'Length < Line_Width - 2 then
            Put (Line_Prefix);
         end if;

         --  Eat words until line is complete
         while Used + ANSI.Length (Next_Word) - 1 <= Line_Width loop
            Put (Next_Word);
            Pos := Pos + Next_Word'Length;

            exit when Used > Line_Width or else Pos > Text'Last;

            --  Advance on spaces
            if Text (Pos) = ' ' then
               Put (" ");
               Pos := Pos + 1;
            end if;
         end loop;

         --  Forcefully break a word if line is still empty. This won't work
         --  with ANSI codes... So don't have too short lines, I guess.
         if Pos = PPos then
            declare
               Remain : constant Positive := Line_Width - Used;
               --  Space for text (without counting the '-')
            begin
               Put (Text (Pos .. Pos + Remain - 1));
               Pos := Pos + Remain;
               Put ("-");
            end;
         end if;

         --  Final dump to file
         Put_Line (File.all, To_String (Line));

         --  Eat spaces that would start the next line:
         while Pos <= Text'Last and then Text (Pos) = ' ' loop
            Pos := Pos + 1;
         end loop;
      end Put_Line;

   begin
      --  Trivial case of empty line:
      if Text = "" then
         New_Line (File.all);
      else
         --  Regular case:
         while Pos <= Text'Last loop
            Put_Line;
         end loop;
      end if;
   end Put_Paragraph;

end AAA.Text_IO;
