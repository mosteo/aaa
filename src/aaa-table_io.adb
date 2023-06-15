with AAA.ANSI;

with Ada.Containers;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded;

with GNAT.IO;

with Umwi;

package body AAA.Table_IO is

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

      Extra : constant Natural := Umwi.Width (Text) - Text'Length;
      --  We are putting the string in a field that will have excess length
      --  at the time of rendering if it contains any wide character. Move
      --  below is not taking into account Unicode grapheme clusters nor
      --  wide characters, so we need to substract the excess length from the
      --  result. Still, I suspect for this to be 100% correct, Text'Length
      --  should be replaced with the length in grapheme clusters, which Umwi
      --  does not yet provide.

      Field : Wide_Wide_String (1 ..
                                T.Max_Widths (Col) + ANSI.Count_Extra (Text));
   begin
      Ada.Strings.Wide_Wide_Fixed.Move (Text,
                                        Field,
                                        Drop    => Ada.Strings.Error,
                                        Justify => Align);

      if Extra = 0 then
         return Field;
      else
         case Align is
            when Right  => return Field (1 + Extra .. Field'Last);
            when others => return Field (1 .. Field'Last - Extra);
         end case;
      end if;
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
