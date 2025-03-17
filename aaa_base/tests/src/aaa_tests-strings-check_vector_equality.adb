pragma Style_Checks ("-gnatM120");

with AAA.Strings; use AAA.Strings;

procedure Aaa_Tests.Strings.Check_Vector_Equality is

   procedure Check_Equality is
      A : constant AAA.Strings.Vector := Empty_Vector.Append ("A").Append ("B").Append ("C");
      B : constant AAA.Strings.Vector := Empty_Vector.Append ("A").Append ("B").Append ("C").Append ("D");
      C : constant AAA.Strings.Vector := Empty_Vector.Append ("B").Append ("C");
      D : constant AAA.Strings.Vector := Empty_Vector.Append ("A").Append ("B").Append ("C");
   begin

      Assert (Empty_Vector = Empty_Vector, "Empty_Vector = Empty_Vector");

      Assert (A /= Empty_Vector, "A /= Empty_Vector");
      Assert (B /= Empty_Vector, "B /= Empty_Vector");
      Assert (C /= Empty_Vector, "C /= Empty_Vector");
      Assert (D /= Empty_Vector, "D /= Empty_Vector");

      Assert (A = A, "A = A");
      Assert (B = B, "B = B");
      Assert (C = C, "C = C");
      Assert (D = D, "D = D");

      Assert (A /= B, "A /= B");
      Assert (B /= A, "B /= A");

      Assert (A /= C, "A /= C");
      Assert (C /= A, "C /= A");

      Assert (B /= C, "B /= C");
      Assert (C /= B, "C /= B");

      Assert (A = D, "A = D");
   end Check_Equality;

begin
   Check_Equality;
end Aaa_Tests.Strings.Check_Vector_Equality;
