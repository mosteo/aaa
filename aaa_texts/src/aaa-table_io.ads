with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with Ada.Strings;

with LML;

package AAA.Table_IO is

   --  A type to format tables according to the max length of fields. The table
   --  is ANSI-aware, so it will work properly for text with embedded ANSI
   --  control sequences. However, non-left-aligned text may not align
   --  properly.

   --  Text supplied to these tables is supposed to be ASCII or UTF-8; other
   --  encodings will either cause errors or break alignment. If any input
   --  requires UTF encoding, output will be conversely encoded.

   type Table is tagged private;

   type Reference (Table : access Table_IO.Table'Class) is limited null record
     with Implicit_Dereference => Table;

   procedure Append (T : in out Table; Cell : String);

   function Append (T : aliased in out Table; Cell : String) return Reference;

   procedure Header (T : in out Table; Cell : String);
   --  Headers are printed as given, but are keep internally for the structured
   --  output as field names. There should be as many headers as columns.

   function Header (T    : aliased in out Table;
                    Cell : String)
                    return Reference;

   procedure New_Row (T : in out Table);

   type Alignments is array (Positive range <>) of Ada.Strings.Alignment;

   procedure Print (T         : Table;
                    Separator : String := " ";
                    Align     : Alignments := (1 .. 0 => <>);
                    Put_Line  : access procedure (Line : String) := null);
   --  Will print the table using GNAT.IO, unless Put_Line is supplied
   --  FIXME:  Align is currently ignored

   procedure Print (T        : Table;
                    Format   : LML.Formats;
                    Put_Line : access procedure (Line : String) := null);

private

   package Natural_Vectors is new Ada.Containers.Vectors (Positive, Natural);

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive,
      Wide_Wide_String);
   subtype Row is String_Vectors.Vector;
   use all type Row;

   package Row_Vectors is new Ada.Containers.Vectors (Positive, Row);

   type Sections is (Nothing, Headers, Data);

   type Table is tagged record
      Next_Column : Positive := 1;
      Headers     : Row;
      Rows        : Row_Vectors.Vector;
      Max_Widths  : Natural_Vectors.Vector;
      Section     : Sections := Nothing;
      Is_Header   : Boolean  := False;
      --  Internal state to discriminate in Append when a header is being added
   end record;

end AAA.Table_IO;
