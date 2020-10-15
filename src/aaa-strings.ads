with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;

package AAA.Strings with Preelaborate is

   function Head (S : String; Separator : Character) return String;
   --  if S contains Separator, the lhs is returned. Otherwise Str is returned.

   function Tail (S : String; Separator : Character) return String;
   --  If S contains Separator, the rhs is returned. Otherwise "".

   function Trim (S : String; Target : Character := ' ') return String;
   --  Remove Target at S extremes

   function Replace (Text  : String;
                     Match : String;
                     Subst : String)
                     return String;
   --  Replace every occurrence of Match in Text by Subst

   ----------
   -- Maps --
   ----------

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, String);

   type Map is new Maps.Map with null record;

   Empty_Map : constant Map;

   -------------
   -- Vectors --
   -------------

   --  A standard vector of strings, for reuse across AAA where string arrays
   --  are needed.

   package Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Vector is new Vectors.Vector with null record;

   Empty_Vector : constant Vector;

   function Append (V : Vector;
                    S : String) return Vector;
   --  Returns a copy of V with S appended at the end

   function Append (L, R : Vector) return Vector;
   --  Append R at the end of L.

   procedure Append_Line (V : in out Vector;
                          S : String;
                          C : Ada.Containers.Count_Type := 1)
                          renames Append;

   procedure Append_Vector (V : in out Vector; V2 : Vector)
                            renames Append;

   function Append_To_Last_Line (V : Vector;
                                 S : String)
                                 return Vector;
   --  Appends S to the last line in V. Does *not* add a new line. If V is
   --  empty, then a vector with a single line equal to S is returned.

   function Count (V : Vector) return Natural;
   --  FSM do I hate the Containers.Count_Type...

   function Flatten (V         : Vector;
                     Separator : String := " ")
                     return String;
   --  Concatenate all elements

   function Flatten (V         : Vector;
                     Separator : Character)
                     return String;
   --  Likewise, using a Character

   function Indent (V      : Vector;
                    Spaces : String := "   ")
                    return   Vector;

   function New_Line (V : Vector) return Vector;
   --  Append an empty line to V

   function Split (S : String; Separator : Character) return Vector;
   --  Split a string in substrings at Separator positions. A Separator at
   --  S'First or S'Last will result in an empty string also being included.

   function Tail (V           : Vector;
                  Allow_Empty : Boolean := False)
                  return Vector with Pre =>
     not V.Is_Empty
     or else Allow_Empty
     or else raise Constraint_Error with "Cannot take tail of empty vector";
   --  Return V without its first element. If Allow_Empty, tail of an empty
   --  vector will be another empty vector.

   not overriding
   function To_Vector (S : String) return Vector;

   procedure Write (V         : Vector;
                    Filename  : String;
                    Separator : String := ASCII.LF & "");
   --  Dump contents to a given file

private

   Empty_Map    : constant Map    := (Maps.Empty_Map with null record);
   Empty_Vector : constant Vector := (Vectors.Empty_Vector with null record);

end AAA.Strings;