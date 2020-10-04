--  with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Expect;
with GNAT.OS_Lib;

package body AAA.Processes is

   package OS renames GNAT.OS_Lib;

   ---------
   -- Run --
   ---------

   function Run
     (Command_Line : Strings.Vector;
      Input        : String := "";
      Err_To_Out     : Boolean := False;
      Raise_On_Error : Boolean := True)
      return Result
   is

      -------------
      -- To_List --
      -------------

      function To_List (V : aliased Strings.Vector) return OS.Argument_List
      --  Reuse the strings in V for the argument list, so this V should
      --  outlive the result usage.
      is
         Pos : Positive := 1;
      begin
         return List : OS.Argument_List (1 .. V.Count) do
            for I in V.First_Index .. V.Last_Index loop
               List (Pos) := V.Constant_Reference (I).Element;
               Pos := Pos + 1;
            end loop;
         end return;
      end To_List;

      use GNAT.Expect;
      Arguments : aliased constant Strings.Vector := Command_Line.Tail;
   begin
      --  Put_Line ("RUNNING: " & Command_Line.Flatten);

      return R : Result do
         R.Output :=
           Strings.Split
             (Strings.Replace
                (Get_Command_Output
                   (Command    => Command_Line.First_Element,
                    Arguments  => To_List (Arguments),
                    Input      => Input,
                    Status     => R.Exit_Code'Access,
                    Err_To_Out => Err_To_Out),
                 Match => ASCII.CR & ASCII.LF,
                 Subst => (1 => ASCII.LF)),
              Separator => ASCII.Lf);

         if R.Exit_Code /= 0 and then Raise_On_Error then
            raise Child_Error with
              "child exited with code" & R.Exit_Code'Image;
         end if;
      end return;
   end Run;

end AAA.Processes;
