with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with Ada.Strings;

package AAA.Table_IO with Preelaborate is

   type Table is tagged private;

   type Reference (Table : access Table_IO.Table) Is limited null Record
     with Implicit_Dereference => Table;

   procedure Append (T : in out Table; Cell : String);

   function Append (T : in out Table; Cell : String) return Reference;


   procedure New_Row (T : in out Table);


   type Alignments is array (Positive range <>) of Ada.Strings.Alignment;

   procedure Print (T         : Table;
                    Separator : String := " ";
                    Align     : Alignments := (1 .. 0 => <>);
                    Put_Line  : access procedure (Line : String) := null);
   --  Will print the table using GNAT.IO, unless Put_Line is supplied

private

   package Natural_Vectors is new Ada.Containers.Vectors (Positive, Natural);

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive,
                                                                    String);
   subtype Row is String_Vectors.Vector;
   use all type Row;

   package Row_Vectors is new Ada.Containers.Vectors (Positive, Row);

   type Table is tagged record
      Next_Column : Positive := 1;
      Rows        : Row_Vectors.Vector;
      Max_Widths  : Natural_Vectors.Vector;
   end record;

end AAA.Table_IO;
