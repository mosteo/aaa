with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

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

   function Append_To_Last_Line (V : Vector;
                                 S : String)
                                 return Vector
   is
   begin
      if V.Is_Empty then
         return To_Vector (S);
      else
         return R : Vector := V do
            R.Delete_Last;
            R.Append_Line (V.Last_Element & S);
         end return;
      end if;
   end Append_To_Last_Line;

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
