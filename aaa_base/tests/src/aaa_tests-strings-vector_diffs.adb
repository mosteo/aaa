with AAA.Strings; use AAA.Strings;

pragma Style_Checks ("-gnatM120");

procedure Aaa_Tests.Strings.Vector_Diffs is

   procedure Check_Diff is

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
      B : constant AAA.Strings.Vector := Empty_Vector.Append ("1").Append ("2").Append ("3").Append ("4");
      C : constant AAA.Strings.Vector := Empty_Vector.Append ("2").Append ("3");
      D : constant AAA.Strings.Vector := Empty_Vector.Append ("5").Append ("6").Append ("7");
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
   Check_Diff;
end Aaa_Tests.Strings.Vector_Diffs;
