with AAA.ANSI;
with AAA.Filesystem;

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

   ------------------
   -- Append_Lines --
   ------------------

   procedure Append_Lines (File       : String;
                           Lines      : Strings.Vector;
                           Backup     : Boolean := True;
                           Backup_Dir : String  := "")
   is
      F : AAA.Text_IO.File := Load (File, Backup, Backup_Dir);
   begin
      F.Lines.Append (Lines);
   end Append_Lines;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (This : in out File) is
      use Ada.Text_IO;
      use type Strings.Vector;

      File : File_Type;
   begin
      if This.Lines = This.Orig then
         return;
      end if;

      declare
         Replacer : Filesystem.Replacer :=
                      Filesystem.New_Replacement (This.Name,
                                                  This.Backup,
                                                  This.Backup_Dir);
      begin
         Open (File, Out_File, Replacer.Editable_Name);
         for Line of This.Lines loop
            Put_Line (File, Line);
         end loop;
         Close (File);
         Replacer.Replace;
      end;
   end Finalize;

   -----------
   -- Lines --
   -----------

   function Lines (This : aliased in out File) return access Strings.Vector
   is (This.Lines'Access);

   ----------
   -- Load --
   ----------

   function Load (From       : String;
                  Backup     : Boolean := True;
                  Backup_Dir : String := "")
                  return File
   is
      use Ada.Text_IO;
      F : File_Type;
   begin
      return This : File := (Ada.Finalization.Limited_Controlled with
                             Length     => From'Length,
                             Backup_Len => Backup_Dir'Length,
                             Name       => From,
                             Backup     => Backup,
                             Backup_Dir => Backup_Dir,
                             Lines      => <>,
                             Orig       => <>)
      do
         Open (F, In_File, From);
         while not End_Of_File (F) loop
            This.Orig.Append (Get_Line (F));
         end loop;
         Close (F);

         This.Lines := This.Orig;
      end return;
   end Load;

end AAA.Text_IO;
