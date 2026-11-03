with AAA.ANSI.Tools;
with AAA.Console_IO;
with AAA.Debug;
with AAA.Filesystem;

with Ada.Directories;
with Ada.Strings.Unbounded;

package body AAA.Text_IO is

   -------------------
   -- Put_Paragraph --
   -------------------

   procedure Put_Paragraph (Text        : String;
                            Line_Width  : Line_Widths := Default_Line_Width;
                            Line_Prefix : String := "";
                            Filling     : Filling_Modes := Greedy)
   is
      pragma Unreferenced (Filling);

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
         is (ANSI.Tools.Length (To_String (Line)));

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
         while Used + ANSI.Tools.Length (Next_Word) - 1 <= Line_Width loop
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

         --  Final dump to output
         Console_IO.Put_Line (To_String (Line));

         --  Eat spaces that would start the next line:
         while Pos <= Text'Last and then Text (Pos) = ' ' loop
            Pos := Pos + 1;
         end loop;
      end Put_Line;

   begin
      --  Trivial case of empty line:
      if Text = "" then
         Console_IO.New_Line;
      else
         --  Regular case:
         while Pos <= Text'Last loop
            Put_Line;
         end loop;
      end if;
   end Put_Paragraph;

   --------------------
   -- Put_Paragraphs --
   --------------------

   procedure Put_Paragraphs (Text        : Strings.Vector;
                             Line_Width  : Line_Widths := Default_Line_Width;
                             Line_Prefix : String := "";
                             Filling     : Filling_Modes := Greedy)
   is
   begin
      for Line of Text loop
         Put_Paragraph (Line, Line_Width, Line_Prefix, Filling);
      end loop;
   end Put_Paragraphs;

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
      F.Save;
   end Append_Lines;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (This : in out File) is
   begin
      if not This.Failed then
         This.Save;
      end if;
   exception
      when E : others =>
         Debug.Put_Exception (E);
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
      Backup_To : constant String :=
                    (if Backup_Dir /= ""
                     then Backup_Dir
                     else Ada.Directories.Containing_Directory (From));
   begin
      return This : File := (Ada.Finalization.Limited_Controlled with
                             Length     => From'Length,
                             Backup_Len => Backup_To'Length,
                             Name       => From,
                             Backup     => Backup,
                             Backup_Dir => Backup_To,
                             Lines      => <>,
                             Orig       => <>,
                             Failed     => False)
      do
         This.Orig  := Filesystem.Read_Lines (From);
         This.Lines := This.Orig;
      end return;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (This : in out File) is
      use type Strings.Vector;
   begin
      This.Failed := False;

      if This.Lines = This.Orig then
         return;
      end if;

      declare
         Replacer : Filesystem.Replacer :=
                      Filesystem.New_Replacement (This.Name,
                                                  This.Backup,
                                                  This.Backup_Dir);
      begin
         Filesystem.Write_Lines (Replacer.Editable_Name, This.Lines);
         Replacer.Replace;
      end;

      This.Orig := This.Lines;
   exception
      when others =>
         This.Failed := True;
         raise;
   end Save;

end AAA.Text_IO;
