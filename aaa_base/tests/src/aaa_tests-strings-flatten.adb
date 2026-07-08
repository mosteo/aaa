with AAA.Strings; use AAA.Strings;

pragma Ignore_Pragma (Alire_Test);
pragma Alire_Test;

pragma Style_Checks ("-gnatM120");

procedure Aaa_Tests.Strings.Flatten is

   procedure Check (V        : AAA.Strings.Vector;
                    Expected : String;
                    Sep      : String := " ")
   is
      Result : constant String := V.Flatten (Sep);
   begin
      Assert (Result = Expected,
             "Flatten (Sep => """ & Sep & """): expected """ &
             Expected & """ but got """ & Result & """");
   end Check;

   procedure Check (V        : AAA.Strings.Vector;
                    Expected : String;
                    Sep      : Character)
   is
      Result : constant String := V.Flatten (Sep);
   begin
      Assert (Result = Expected,
             "Flatten (Sep => '" & Sep & "'): expected """ &
             Expected & """ but got """ & Result & """");
   end Check;

   One   : constant AAA.Strings.Vector := Empty_Vector.Append ("A");
   Three : constant AAA.Strings.Vector :=
             Empty_Vector.Append ("A").Append ("B").Append ("C");

begin

   --  Empty vector: default separator

   Check (Empty_Vector, "");

   --  Empty vector: explicit multi-char separator

   Check (Empty_Vector, "", Sep => ", ");

   --  Empty vector: character separator

   Check (Empty_Vector, "", Sep => ASCII.LF);

   --  Single element: separator is never inserted

   Check (One, "A");
   Check (One, "A", Sep => ", ");
   Check (One, "A", Sep => ASCII.LF);

   --  Several elements: default separator (single space)

   Check (Three, "A B C");

   --  Several elements: explicit multi-char string separator

   Check (Three, "A, B, C", Sep => ", ");

   --  Several elements: empty separator simply concatenates

   Check (Three, "ABC", Sep => "");

   --  Several elements: character separator

   Check (Three, "A" & ASCII.LF & "B" & ASCII.LF & "C", Sep => ASCII.LF);

   --  Elements that are themselves empty strings

   Check (Empty_Vector.Append ("").Append ("").Append (""),
         "  ", -- two single-space separators, no other content
         Sep => " ");

   Check (Empty_Vector.Append ("A").Append ("").Append ("B"),
         "A--B",
         Sep => "-");

   --  Elements containing the separator itself are not treated specially

   Check (Empty_Vector.Append ("A,B").Append ("C"),
         "A,B,C",
         Sep => ",");

end Aaa_Tests.Strings.Flatten;
