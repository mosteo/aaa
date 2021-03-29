with Ada.Characters.Handling;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;

package AAA.Strings with Preelaborate is

   function Camel_To_Mixed (S : String) return String;
   --  Converts ThisThing into This_Thing

   function Contains (Full : String; Sub : String) return Boolean;

   function Has_Prefix (Full : String; Prefix : String) return Boolean;

   function Has_Suffix (Full : String; Suffix : String) return Boolean;

   function Head (S : String; Separator : Character) return String;
   --  if S contains Separator, the lhs is returned. Otherwise Str is returned.

   function Head (S : String; Separator : String) return String;
   --  if S contains Separator, the lhs is returned. Otherwise Str is returned.

   function Tail (S : String; Separator : Character) return String;
   --  If S contains Separator, the rhs is returned. Otherwise "".

   function Tail (S : String; Separator : String) return String;
   --  If S contains Separator, the rhs is returned. Otherwise "".

   function To_Lower_Case (S : String) return String
                           renames Ada.Characters.Handling.To_Lower;

   function To_Mixed_Case (S : String) return String;

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

   ----------
   -- Sets --
   ----------

   package Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   type Set is new Sets.Set with null record;

   Empty_Set : constant Set;

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

   procedure Append_To_Last_Line (V : in out Vector; S : String);

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
   --  Append an empty line to V and return it in a new vector

   procedure New_Line (V : in out Vector);
   --  Append new line to V

   procedure Prepend (V : in out Vector; S : Set'Class);

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

   function Contains (Full : String; Sub : String) return Boolean
   is (for some I in Full'Range =>
          I + Sub'Length - 1 in Full'Range and then
          Full (I .. I + Sub'Length - 1) = Sub);

   Empty_Map    : constant Map    := (Maps.Empty_Map with null record);
   Empty_Set    : constant Set    := (Sets.Empty_Set with null record);
   Empty_Vector : constant Vector := (Vectors.Empty_Vector with null record);

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (Full, Prefix : String) return Boolean is
     (Full'Length >= Prefix'Length
      and then Full (Full'First .. Full'First + Prefix'Length - 1) = Prefix);

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix (Full, Suffix : String) return Boolean is
     (Full'Length >= Suffix'Length
      and then Full (Full'Last - Suffix'Length + 1 .. Full'Last) = Suffix);

end AAA.Strings;
