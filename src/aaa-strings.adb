with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

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

   -------------------
   -- Append_Vector --
   -------------------

   procedure Append_Vector (V : in out Vector; V2 : Vector) is
   begin
      V.Append (V2);
   end Append_Vector;

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

   function Split (S : String; Separator : Character) return Vector is
      Prev : Integer := S'First - 1;
   begin
      return V : Vector do
         for I in S'Range loop
            if S (I) = Separator then
               V.Append (S (Prev + 1 .. I - 1));
               Prev := I;
            end if;
         end loop;
         V.Append (S (Prev + 1 .. S'Last));
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

end AAA.Strings;
