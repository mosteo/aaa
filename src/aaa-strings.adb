with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Containers.Bounded_Vectors;

with GNAT.Case_Util;

package body AAA.Strings is

   ------------
   -- Append --
   ------------

   function Append (V : Vector;
                    S : String) return Vector is
   begin
      return R : Vector := V do
         R.Append (S);
      end return;
   end Append;

   ------------
   -- Append --
   ------------

   function Append (L, R : Vector) return Vector is
   begin
      return Result : Vector := L do
         Result.Append (R);
      end return;
   end Append;

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (L : Vector;
                 R : Vector) return Boolean
   is
   begin
      if L.Count /= R.Count then
         return False;
      end if;

      for Index
      in L.First_Index .. L.Last_Index
      loop
         if L.Element (Index) /= R.Element (Index) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   -------------------------
   -- Append_To_Last_Line --
   -------------------------

   procedure Append_To_Last_Line (V : in out Vector; S : String) is
   begin
      if V.Is_Empty then
         V.Append (S);
      else
         declare
            Last : constant String := V.Last_Element;
         begin
            V.Delete_Last;
            V.Append_Line (Last & S);
         end;
      end if;
   end Append_To_Last_Line;

   -------------------------
   -- Append_To_Last_Line --
   -------------------------

   function Append_To_Last_Line (V : Vector;
                                 S : String)
                                 return Vector
   is
   begin
      return R : Vector := V do
         R.Append_To_Last_Line (S);
      end return;
   end Append_To_Last_Line;

   --------------------
   -- Camel_To_Mixed --
   --------------------

   function Camel_To_Mixed (S : String) return String is
   begin
      for I in S'First + 1 .. S'Last loop
         if Ada.Characters.Handling.Is_Upper (S (I)) then
            return
              S (S'First .. I - 1)
              & "_"
              & Camel_To_Mixed (S (I .. S'Last));
         end if;
      end loop;

      return S;
   end Camel_To_Mixed;

   -----------
   -- Count --
   -----------

   function Count (V : Vector) return Natural
   is (Natural (Vectors.Vector (V).Length));

   -------------
   -- Flatten --
   -------------

   function Flatten (V         : Vector;
                     Separator : String := " ")
                     return String
   is

      function Flatten (Pos : Positive; V : Vector) return String;

      -------------
      -- Flatten --
      -------------

      function Flatten (Pos : Positive; V : Vector) return String is
        (if Pos = V.Count
         then V (Pos)
         else V (Pos) & Separator & Flatten (Pos + 1, V));

   begin
      if V.Is_Empty then
         return "";
      else
         return Flatten (1, V);
      end if;
   end Flatten;

   -------------
   -- Flatten --
   -------------

   function Flatten (V         : Vector;
                     Separator : Character)
                     return String
   is (V.Flatten ((1 => Separator)));

   ----------
   -- Head --
   ----------

   function Head (S : String; Separator : Character) return String is
   begin
      for I in S'Range loop
         if S (I) = Separator then
            return S (S'First .. I - 1);
         end if;
      end loop;

      return S;
   end Head;

   ----------
   -- Head --
   ----------

   function Head (S : String; Separator : String) return String is
   begin
      for I in S'Range loop
         if I + Separator'Length - 1 in S'Range and then
           S (I .. I + Separator'Length - 1) = Separator
         then
            return S (S'First .. I - 1);
         end if;
      end loop;

      return S;
   end Head;

   ------------
   -- Indent --
   ------------

   function Indent (V      : Vector;
                    Spaces : String := "   ")
                    return   Vector is
   begin
      return R : Vector do
         for Line of V loop
            R.Append (String'(Spaces & Line));
         end loop;
      end return;
   end Indent;

   --------------
   -- New_Line --
   --------------

   function New_Line (V : Vector) return Vector
   is (V.Append (""));

   --------------
   -- New_Line --
   --------------

   procedure New_Line (V : in out Vector) is
   begin
      V.Append ("");
   end New_Line;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (V : in out Vector; S : Set'Class) is
   begin
      for Str of reverse S loop
         V.Prepend (Str);
      end loop;
   end Prepend;

   -------------
   -- Replace --
   -------------

   function Replace (Text  : String;
                     Match : String;
                     Subst : String)
                     return String
   is
      use Ada.Strings.Fixed;
      First : Natural;
   begin
      First := Index (Text, Match);
      if First = 0 then
         return Text;
      else
         return
           Text (Text'First .. First - 1)
           & Subst
           & Replace (Text (First + Match'Length .. Text'Last), Match, Subst);
      end if;
   end Replace;

   -----------
   -- Split --
   -----------

   function Split (Text      : String;
                   Separator : Character;
                   Side      : Halves := Head;
                   From      : Halves := Head;
                   Count     : Positive := 1;
                   Raises    : Boolean  := True) return String
   is
      Seen : Natural := 0;
      Pos  : Integer := (if From = Head then Text'First else Text'Last);
      Inc  : constant Integer := (if From = Head then 1 else -1);
   begin
      loop
         if Text (Pos) = Separator then
            Seen := Seen + 1;

            if Seen = Count then
               if Side = Head then
                  return Text (Text'First .. Pos - 1);
               else
                  return Text (Pos + 1 .. Text'Last);
               end if;
            end if;
         end if;

         Pos := Pos + Inc;

         exit when Pos not in Text'Range;
      end loop;

      if Raises then
         raise Constraint_Error with "Not enought separators found";
      else
         return Text;
      end if;
   end Split;

   -------------
   -- Shorten --
   -------------

   function Shorten (Text       : String;
                     Max_Length : Natural;
                     Trim_Side  : Halves := Head)
                     return String
   is
      Ellipsis : constant String := "(...)";
   begin
      if Text'Length <= Max_Length then
         return Text;
      elsif Trim_Side = Head then
         return Ellipsis
                & Ada.Strings.Fixed.Tail (Text, Max_Length - Ellipsis'Length);
      else
         return Ada.Strings.Fixed.Head (Text, Max_Length - Ellipsis'Length)
                & Ellipsis;
      end if;
   end Shorten;

   -----------
   -- Split --
   -----------

   function Split (S         : String;
                   Separator : Character;
                   Trim      : Boolean := False)
                   return Vector
   is
      function Do_Trim (S : String) return String
      is (if Trim then AAA.Strings.Trim (S) else S);

      Prev : Integer := S'First - 1;
   begin
      if Do_Trim (S) = "" then
         return V : constant Vector := Empty;
      end if;

      return V : Vector do
         for I in S'Range loop
            if S (I) = Separator then
               V.Append (Do_Trim (S (Prev + 1 .. I - 1)));
               Prev := I;
            end if;
         end loop;
         V.Append (Do_Trim (S (Prev + 1 .. S'Last)));
      end return;
   end Split;

   ----------
   -- Tail --
   ----------

   function Tail (S : String; Separator : Character) return String is
   begin
      for I in S'Range loop
         if S (I) = Separator then
            return S (I + 1 .. S'Last);
         end if;
      end loop;

      return "";
   end Tail;

   ----------
   -- Tail --
   ----------

   function Tail (S : String; Separator : String) return String is
   begin
      for I in S'Range loop
         if I + Separator'Length - 1 in S'Range and then
           S (I .. I + Separator'Length - 1) = Separator
         then
            return S (I + Separator'Length .. S'Last);
         end if;
      end loop;

      return "";
   end Tail;

   ----------
   -- Tail --
   ----------

   function Tail (V : Vector; Allow_Empty : Boolean := False) return Vector is
   begin
      if V.Is_Empty then
         if Allow_Empty then
            return Empty_Vector;
         else
            raise Constraint_Error with "Cannot take tail of empty vector";
         end if;
      end if;

      return Result : Vector := V do
         Result.Delete_First;
      end return;
   end Tail;

   -------------------
   -- To_Mixed_Case --
   -------------------

   function To_Mixed_Case (S : String) return String is
   begin
      return R : String := S do
         GNAT.Case_Util.To_Mixed (R);

         --  Also change to upper characters after a dot
         for I in R'Range loop
            if I /= R'First and then R (I - 1) = '.' then
               R (I) := GNAT.Case_Util.To_Upper (R (I));
            end if;
         end loop;
      end return;
   end To_Mixed_Case;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (S : String) return Vector is
   begin
      return V : Vector do
         V.Append (S);
      end return;
   end To_Vector;

   ----------
   -- Trim --
   ----------

   function Trim (S : String; Target : Character := ' ') return String is
     (Ada.Strings.Fixed.Trim
        (S,
         Left  => Ada.Strings.Maps.To_Set (Target),
         Right => Ada.Strings.Maps.To_Set (Target)));

   ------------
   -- Crunch --
   ------------

   function Crunch (Text : String) return String is
      Result : String (Text'Range);
      Src    : Natural := Text'First;
      Dst    : Natural := Result'First;
   begin
      --  Trim initial spaces:
      while Src <= Text'Last and then Text (Src) = ' ' loop
         Src := Src + 1;
      end loop;

      --  Remove excess spaces:
      while Src <= Text'Last loop
         if Src = Text'First
           or else
            Text (Src) /= ' '
           or else
            Text (Src - 1) /= ' '
         then
            Result (Dst) := Text (Src);
            Dst := Dst + 1;
         end if;
         Src := Src + 1;
      end loop;

      return Result (Result'First .. Dst - 1);
   end Crunch;

   -----------
   -- Write --
   -----------

   procedure Write (V         : Vector;
                    Filename  : String;
                    Separator : String := ASCII.LF & "")
   is
      use Ada.Streams.Stream_IO;
      F : File_Type;
   begin
      Create (F, Out_File, Filename);

      for Line of V loop
         String'Write (Stream (F), Line);
         String'Write (Stream (F), Separator);
      end loop;

      Close (F);

   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end Write;

   ----------
   -- Diff --
   ----------

   function Diff (A, B        : AAA.Strings.Vector;
                  A_Name      : String := "A";
                  B_Name      : String := "B";
                  Skip_Header : Boolean := False)
                  return AAA.Strings.Vector
   is
      --  Tentative Myers diff implementation

      Max : constant Integer := A.Count + B.Count;

      type Action_Kind is (Keep, Insert, Remove);

      type Action is record
         Kind  : Action_Kind;
         Index : Positive;
      end record;

      package Action_Vectors is new
        Ada.Containers.Bounded_Vectors (Positive, Action);

      subtype History_Vector
      is Action_Vectors.Vector (Ada.Containers.Count_Type (Max));

      type Frontier is record
         X       : Integer := 0;
         History : History_Vector;
      end record;

      V : array (-Max .. Max) of Frontier;

      K : Integer;
      X, Y : Integer := 0;
      Go_Down : Boolean;

      History : History_Vector;

      Result : AAA.Strings.Vector;
   begin
      if A.First_Index /= 1 then
         raise Program_Error;
      elsif B.First_Index /= 1 then
         raise Program_Error;
      end if;

      V (1).X := 0;

      Main_Loop :
      for D in 0 .. Max loop
         K := -D;
         while K <= D loop
            Go_Down := (K = -D)
              or else
                ((K /= D) and then (V (K - 1).X < V (K + 1).X));

            if Go_Down then
               X := V (K + 1).X;
               History := V (K + 1).History;
            else
               X := V (K - 1).X + 1;
               History := V (K - 1).History;
            end if;

            Y := X - K;

            if Go_Down and then Y in 1 .. B.Count then
               History.Append (Action'(Insert, Y));
            elsif X in 1 .. A.Count then
               History.Append (Action'(Remove, X));
            end if;

            while X in 0 .. A.Count - 1
              and then
                Y in 0 .. B.Count - 1
                and then
                  A.Element (X + 1) = B.Element (Y + 1)
            loop
               X := X + 1;
               Y := Y + 1;
               History.Append (Action'(Keep, X));
            end loop;

            if X >= A.Count and then Y >= B.Count then
               exit Main_Loop;
            else
               V (K).X := X;
               V (K).History := History;
            end if;

            K := K + 2;
         end loop;
      end loop Main_Loop;

      if not Skip_Header then
         Result.Append (String'("--- " & A_Name));
         Result.Append (String'("+++ " & B_Name));
      end if;

      for Elt of History loop
         case Elt.Kind is
            when Keep =>
               Result.Append (String'("  " & A.Element (Elt.Index)));
            when Remove =>
               Result.Append (String'("- " & A.Element (Elt.Index)));
            when Insert =>
               Result.Append (String'("+ " & B.Element (Elt.Index)));
         end case;
      end loop;
      return Result;
   end Diff;

end AAA.Strings;
