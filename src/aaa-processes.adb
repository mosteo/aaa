with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Expect;

package body AAA.Processes is

   package OS renames GNAT.OS_Lib;

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

   -----------------------
   -- Spawn_And_Capture --
   -----------------------

   function Spawn_And_Capture
     (Output              : in out Strings.Vector;
      Command             : String;
      Arguments           : Strings.Vector;
      Err_To_Out          : Boolean := False)
     return Integer
   is
      use GNAT.OS_Lib;
      File     : File_Descriptor;
      Name     : String_Access;

      Arg_List : aliased constant Argument_List := To_List (Arguments);

      Outfile : File_Type;

      Exit_Code : Integer;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
         Ok : Boolean;
      begin
         Delete_File (Name.all, Ok);
         if not Ok then
            Put_Line ("Failed to delete tmp file: " & Name.all);
         end if;

         Free (Name);
      end Cleanup;

      -----------------
      -- Read_Output --
      -----------------

      procedure Read_Output is
      begin
         Open (Outfile, In_File, Name.all);
         while not End_Of_File (Outfile) loop
            Output.Append (Get_Line (Outfile));
         end loop;
         Close (Outfile);
      end Read_Output;

   begin
      Create_Temp_Output_File (File, Name);

      --  Put_Line ("Spawning: "
      --            & Command & " " & Arguments.Flatten
      --            & " > " & Name.all);

      Spawn (Program_Name           => Command,
             Args                   => Arg_List,
             Output_File_Descriptor => File,
             Return_Code            => Exit_Code,
             Err_To_Out             => Err_To_Out);

      Close (File); -- Can't raise
      Read_Output;

      Cleanup;
      return Exit_Code;
   end Spawn_And_Capture;

   ----------------
   -- Get_Output --
   ----------------
   --  This shouldn't exist, but problems with Windows polling force us to
   --  do our own reimplementation. Offending call in GNAT.Expect:700 to
   --  Poll hangs.
   function Get_Output (Command_Line : Strings.Vector;
                        Input        : String := "";
                        Exit_Code    : aliased out Integer;
                        Err_To_Out   : Boolean := False)
                        return String
   is
      use GNAT.Expect;
      Arguments : aliased constant Strings.Vector := Command_Line.Tail;
      Output    : Strings.Vector;
   begin
      if GNAT.OS_Lib.Directory_Separator = '\' then -- Windows

         --  For some unknown reason we get no output on Windows, no matter how
         --  the call to Expect is made. Falling back to plain Spawn without
         --  user input (!).

         if Input /= "" and then GNAT.OS_Lib.Directory_Separator = '\' then
            raise Unimplemented
              with "Spawning with user input is unuspported on Windows";
         end if;

         Exit_Code := Spawn_And_Capture
           (Output              => Output,
            Command             => Command_Line.First_Element,
            Arguments           => Command_Line.Tail,
            Err_To_Out          => Err_To_Out);

         return Output.Flatten (ASCII.LF);

      else
         return Get_Command_Output
           (Command    => Command_Line.First_Element,
            Arguments  => To_List (Arguments),
            Input      => Input,
            Status     => Exit_Code'Access,
            Err_To_Out => Err_To_Out);
      end if;
   end Get_Output;

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

   begin
      --  Put_Line ("RUNNING: " & Command_Line.Flatten);

      return R : Result do
         R.Output :=
           Strings.Split
             (Strings.Replace
                (Get_Output
                   (Command_Line => Command_Line,
                    Input        => Input,
                    Exit_Code    => R.Exit_Code,
                    Err_To_Out   => Err_To_Out),
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
