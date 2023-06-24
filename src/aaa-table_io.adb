with AAA.ANSI;

with Ada.Containers;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Wide_Wide_Unbounded;

with GNAT.IO;

with Umwi;

package body AAA.Table_IO is

   Debug : constant Boolean := False;

   package UTF   renames Ada.Strings.UTF_Encoding;

   use all type Ada.Containers.Count_Type;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Table; Cell : String) is
   begin
      declare
         Cell : constant Wide_Wide_String :=
                  UTF.Wide_Wide_Strings.Decode (Append.Cell);
      begin
         if T.Rows.Is_Empty then
            T.New_Row;
         end if;

         if Natural (T.Max_Widths.Length) < T.Next_Column then
            T.Max_Widths.Append (ANSI.Length (Cell));
         else
            T.Max_Widths (T.Next_Column) :=
              Natural'Max (ANSI.Length (Cell), T.Max_Widths (T.Next_Column));
         end if;

         T.Rows (Natural (T.Rows.Length)).Append (Cell);
         T.Next_Column := T.Next_Column + 1;
      end;
   end Append;

   ------------
   -- Append --
   ------------

   function Append (T : aliased in out Table; Cell : String) return Reference
   is
   begin
      T.Append (Cell);
      return Reference'(Table => T'Access);
   end Append;

   -------------
   -- New_Row --
   -------------

   procedure New_Row (T : in out Table) is
   begin
      T.Next_Column := 1;
      T.Rows.Append (String_Vectors.Empty_Vector);
   end New_Row;

   ----------------
   -- Put_Padded --
   ----------------

   function Prepare_Padded (T     : Table;
                            Col   : Positive;
                            Text  : Wide_Wide_String;
                            Align : Ada.Strings.Alignment)
                            return Wide_Wide_String
   is
      use all type Ada.Strings.Alignment;

      Counts : constant Umwi.Counts := Umwi.Count (Text);

      --  We need Field to be as wide as T.Max_Widths (Col), so any width
      --  missing is due to Text being shorter (Max - Text.Width). But also
      --  we need to take into account the invisible ANSI codes for the total
      --  Field length, as it's not included in Text'Length.

      Pad    : constant Wide_Wide_String :=
                 (1 .. ANSI.Count_Extra (Text)
                       + T.Max_Widths (Col)
                       - Counts.Width => ' ');

      Mid    : constant Integer := Pad'Length / 2;

      Field  : constant Wide_Wide_String :=
                 ""
                 & (case Align is
                       when Left   => "",
                       when Right  => Pad,
                       when Center => Pad (1 .. Mid))
                 & Text
                 & (case Align is
                       when Left   => Pad,
                       when Right  => "",
                       when Center => Pad (Mid + 1 .. Pad'Last))
      ;

   begin
      if Debug then
         GNAT.IO.Put_Line
           (""
            & "  g:" & Counts.Clusters'Image
            & "; w:" & Counts.Width'Image
            & "; l:" & Counts.Points'Image
            & "; f:" & Field'Length'Image
            & "; m:" & Natural'(T.Max_Widths (Col))'Image
            & "; a:" & ANSI.Count_Extra (Text)'Image
            & "; t:'" & Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
              (Text) & "'");
      end if;

      --  Alignment is broken, considering that ANSI is counted as regular text
      --  by Ada.Strings.Move, so let's just use left-align for the time being.

      return Field;
   end Prepare_Padded;

   -----------
   -- Print --
   -----------

   procedure Print (T         : Table;
                    Separator : String := " ";
                    Align     : Alignments := (1 .. 0 => <>);
                    Put_Line  : access procedure (Line : String) := null)
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      Wide_Separator : constant Wide_Wide_String :=
                         UTF.Wide_Wide_Strings.Decode (Separator);
   begin
      for Row of T.Rows loop
         declare
            Line : Unbounded_Wide_Wide_String;
         begin
            for I in 1 .. Natural (Row.Length) loop
               Append (Line,
                       Prepare_Padded
                         (T,
                          I,
                          Row (I),
                          (if Align'Length >= I
                           then Align (I)
                           else Ada.Strings.Left)));

               if I < Natural (Row.Length) then
                  Append (Line, Wide_Separator);
               else
                  declare
                     UTF8_Line : constant String :=
                                   UTF.Wide_Wide_Strings.Encode
                                     (To_Wide_Wide_String (Line));
                  begin
                     if Put_Line /= null then
                        Put_Line (UTF8_Line);
                     else
                        GNAT.IO.Put_Line (UTF8_Line);
                     end if;
                  end;
               end if;
            end loop;
         end;
      end loop;
   end Print;

end AAA.Table_IO;
