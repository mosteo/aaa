with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with GNAT.IO;

package body AAA.Table_IO is

   use all type Ada.Containers.Count_Type;

   ----------------
   -- ANSI_Extra --
   ----------------

   function ANSI_Extra (Text : String) return Natural is
      Counting : Boolean := False;
      Extra    : Natural := 0;
   begin
      for Char of Text loop
         if Counting then
            Extra := Extra + 1;
            if Char = 'm' then
               Counting := False;
            end if;
         else
            if Char = ASCII.ESC then
               Counting := True;
               Extra := Extra + 1;
            end if;
         end if;
      end loop;

      return Extra;
   end ANSI_Extra;

   -----------------
   -- ANSI_Length --
   -----------------

   function ANSI_Length (Text : String) return Natural is
   begin
      return Text'Length - ANSI_Extra (Text);
   end ANSI_Length;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Table; Cell : String) is
   begin
      if T.Rows.Is_Empty then
         T.New_Row;
      end if;

      if Natural (T.Max_Widths.Length) < T.Next_Column then
         T.Max_Widths.Append (ANSI_Length (Cell));
      else
         T.Max_Widths (T.Next_Column) :=
           Natural'Max (ANSI_Length (Cell), T.Max_Widths (T.Next_Column));
      end if;

      T.Rows (Natural (T.Rows.Length)).Append (Cell);
      T.Next_Column := T.Next_Column + 1;
   end Append;

   ------------
   -- Append --
   ------------

   function Append (T : in out Table; Cell : String) return Reference is
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
                            Text  : String;
                            Align : Ada.Strings.Alignment) return String
   is
      Field : String (1 .. T.Max_Widths (Col) + ANSI_Extra (Text));
   begin
      Ada.Strings.Fixed.Move (Text,
                              Field,
                              Drop    => Ada.Strings.Error,
                              Justify => Align);
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
      use Ada.Strings.Unbounded;
   begin
      for Row of T.Rows loop
         declare
            Line : Unbounded_String;
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
                  Append (Line, Separator);
               else
                  if Put_Line /= null then
                     Put_Line (To_String (Line));
                  else
                     GNAT.IO.Put_Line (To_String (Line));
                  end if;
               end if;
            end loop;
         end;
      end loop;
   end Print;

end AAA.Table_IO;
