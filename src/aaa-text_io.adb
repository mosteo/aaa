package body AAA.Text_IO is

   -------------------
   -- Put_Paragraph --
   -------------------

   procedure Put_Paragraph (Text        : String;
                            Line_Width  : Line_Widths := Default_Line_Width;
                            Line_Prefix : String := "";
                            Filling     : Filling_Modes := Greedy;
                            File        : Ada.Text_IO.File_Access :=
                              Ada.Text_Io.Standard_Output)
   is
      pragma Unreferenced (Filling);
      use Ada.Text_IO;

      Pos : Integer := Text'First;

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

      procedure Put_Line is
         Line : String (1 .. Line_Width);
         LPos : Positive := Line'First;

         PPos : constant Integer := Pos; -- Initial Pos, to check we had some progress

         procedure Put (Word : String) is
         begin
            Line (LPos .. LPos + Word'Length - 1) := Word;
            LPos := LPos + Word'Length;
         end Put;

      begin
         --  Set prefix, if it fits
         if Line_Prefix'Length < Line_Width - 2 then
            Put (Line_Prefix);
         end if;

         --  Eat words until line is complete
         while LPos + Next_Word'Length - 1 <= Line'Last loop
            Put (Next_Word);
            Pos := Pos + Next_Word'Length;

            exit when LPos > Line'Last or else Pos > Text'Last;

            --  Advance on spaces
            if Text (Pos) = ' ' then
               Put (" ");
               Pos := Pos + 1;
            end if;
         end loop;

         --  Forcefully break a word if line is still empty
         if Pos = PPos then
            declare
               Remain : constant Positive := Line'Last - LPos;
               --  Space for text (without counting the '-')
            begin
               Put (Text (Pos .. Pos + Remain - 1));
               Pos := Pos + Remain;
               Put ("-");
            end;
         end if;

         --  Final dump to file
         Put_Line (File.all, Line (Line'First .. LPos - 1));

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
