with AUnit.Assertions; use AUnit.Assertions;

with AAA.Strings; use AAA.Strings;

package body Test_Cases.Strings.Vector is

   --------------------
   -- Check_Equality --
   --------------------

   procedure Check_Equality (Unused : in out Null_Fixture) is
      A : constant AAA.Strings.Vector := Empty_Vector.Append ("A").Append("B").Append ("C");
      B : constant AAA.Strings.Vector := Empty_Vector.Append ("A").Append("B").Append ("C").Append ("D");
      C : constant AAA.Strings.Vector := Empty_Vector.Append ("B").Append ("C");
      D : constant AAA.Strings.Vector := Empty_Vector.Append ("A").Append("B").Append ("C");
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

   ----------------
   -- Check_Diff --
   ----------------

   procedure Check_Diff (Unused : in out Null_Fixture) is

      procedure Check (V1, V2, Expected : AAA.Strings.Vector;
                       Skip_Header      : Boolean := True)
      is
         Result : constant AAA.Strings.Vector :=
           Diff (V1, V2,
                 Skip_Header => Skip_Header);
      begin
         if Result /= Expected then
            Assert (False, "Diff (V1, V2): " & ASCII.LF &
                      "V1:" & ASCII.LF &
                      V1.Flatten (ASCII.LF) & ASCII.LF &
                      "V2:" & ASCII.LF &
                      V2.Flatten (ASCII.LF) & ASCII.LF &
                      "Expected:" & ASCII.LF &
                      Expected.Flatten (ASCII.LF) & ASCII.LF &
                      "Actual:" & ASCII.LF &
                      Result.Flatten (ASCII.LF));
         end if;
      end Check;

      A : constant AAA.Strings.Vector := Empty_Vector.Append ("1").Append ("2").Append ("3");
      B : constant AAA.Strings.Vector := Empty_Vector.Append ("1").Append("2").Append ("3").Append ("4");
      C : constant AAA.Strings.Vector := Empty_Vector.Append ("2").Append ("3");
      D : constant AAA.Strings.Vector := Empty_Vector.Append ("5").Append("6").Append ("7");
   begin

      Check (A, A,
             Empty_Vector
             .Append ("--- A")
             .Append ("+++ B")
             .Append ("  1")
             .Append ("  2")
             .Append ("  3"),
             Skip_Header => False
            );

      Check (A, A,
             Empty_Vector
             .Append ("  1")
             .Append ("  2")
             .Append ("  3"));

      Check (A, B,
             Empty_Vector
             .Append ("  1")
             .Append ("  2")
             .Append ("  3")
             .Append ("+ 4")
            );

      Check (B, A,
             Empty_Vector
             .Append ("  1")
             .Append ("  2")
             .Append ("  3")
             .Append ("- 4")
            );

      Check (B, C,
             Empty_Vector
             .Append ("- 1")
             .Append ("  2")
             .Append ("  3")
             .Append ("- 4")
            );

      Check (A, D,
             Empty_Vector
             .Append ("- 1")
             .Append ("- 2")
             .Append ("- 3")
             .Append ("+ 5")
             .Append ("+ 6")
             .Append ("+ 7")
            );
   end Check_Diff;

begin

   Suite.Add_Test (Null_Caller.Create ("Strings.Vector.Equality",
                   Check_Equality'Access));
   Suite.Add_Test (Null_Caller.Create ("Strings.Vector.Diff",
                   Check_Diff'Access));

end Test_Cases.Strings.Vector;
