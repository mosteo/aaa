with AAA.Processes;
with AAA.Strings; use AAA.Strings;

with GNAT.OS_Lib;

pragma Alire_Test (Should_Fail, False);

procedure Aaa_Tests.Processes.Run is

   package Proc renames AAA.Processes;

   On_Windows : constant Boolean :=
                  GNAT.OS_Lib.Directory_Separator = '\';

   function Shell (Cmd : String) return Vector
   is (if On_Windows
       then Empty_Vector & "cmd" & "/c" & Cmd
       else Empty_Vector & "sh" & "-c" & Cmd);
   --  Portable command line to run a one-liner through the OS shell

begin

   --  Successful command: zero exit code and captured output

   declare
      R : constant Proc.Result := Proc.Run (Shell ("echo hello"));
   begin
      Assert (R.Exit_Code = 0,
              "unexpected exit code:" & R.Exit_Code'Image);
      Assert (R.Output.Contains ("hello"),
              "output not captured: " & R.Output.Flatten);
   end;

   --  Multi-line output is split into one vector element per line

   declare
      R : constant Proc.Result := Proc.Run (Shell ("echo a&&echo b"));
   begin
      Assert (R.Output.Contains ("a") and then R.Output.Contains ("b"),
              "missing output lines in: " & R.Output.Flatten);
   end;

   --  A failing command raises Child_Error by default

   begin
      declare
         R : constant Proc.Result := Proc.Run (Shell ("exit 3"));
      begin
         Assert (False,
                 "Child_Error not raised for exit code:"
                 & R.Exit_Code'Image);
      end;
   exception
      when Proc.Child_Error => null; -- expected
   end;

   --  Raise_On_Error => False reports the exit code without raising

   declare
      R : constant Proc.Result :=
            Proc.Run (Shell ("exit 3"), Raise_On_Error => False);
   begin
      Assert (R.Exit_Code = 3,
              "unexpected exit code:" & R.Exit_Code'Image);
   end;

   --  Exit codes listed in Success_Codes do not raise

   declare
      R : constant Proc.Result :=
            Proc.Run (Shell ("exit 3"), Success_Codes => (0, 3));
   begin
      Assert (R.Exit_Code = 3,
              "unexpected exit code:" & R.Exit_Code'Image);
   end;

   --  Err_To_Out captures the child's stderr in the output

   declare
      R : constant Proc.Result :=
            Proc.Run (Shell ("echo oops 1>&2"), Err_To_Out => True);
   begin
      Assert (Contains (R.Output.Flatten, "oops"),
              "stderr not captured: " & R.Output.Flatten);
   end;

   --  Without Err_To_Out, stderr is not part of the output

   declare
      R : constant Proc.Result := Proc.Run (Shell ("echo oops 1>&2"));
   begin
      Assert (not Contains (R.Output.Flatten, "oops"),
              "stderr unexpectedly captured: " & R.Output.Flatten);
   end;

   --  The following uses of Input are unsupported on Windows (see spec)

   if not On_Windows then

      --  Input is fed to the child's stdin

      declare
         R : constant Proc.Result :=
               Proc.Run (Shell ("cat"), Input => "hello");
      begin
         Assert (Contains (R.Output.Flatten, "hello"),
                 "input not echoed back: " & R.Output.Flatten);
      end;

      --  CR & LF sequences are normalized to plain LF, so no element
      --  should keep a trailing CR

      declare
         R : constant Proc.Result :=
               Proc.Run (Shell ("printf 'a\r\nb\n'"));
      begin
         Assert (R.Output.Contains ("a") and then R.Output.Contains ("b"),
                 "CRLF not normalized in: " & R.Output.Flatten);
      end;

   end if;

end Aaa_Tests.Processes.Run;
