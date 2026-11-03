pragma Alire_Test;

with AAA.Filesystem;
with AAA.Strings;
with AAA.Text_IO;

with Ada.Assertions;
with Ada.Directories;

--  Save reports a failure to its caller, whereas on going out of scope a file
--  must not propagate anything, as that is a bounded error (ARM 7.6.1).

procedure Aaa_Texts_Tests.File_Save_Failure is

   use Ada.Assertions;
   use type AAA.Strings.Vector;

   Temp : AAA.Filesystem.Temp_File := AAA.Filesystem.New_Name;

begin
   Ada.Directories.Create_Directory (Temp.Filename);

   declare
      Path : constant String :=
               Ada.Directories.Compose (Temp.Filename, "file");

      Original : constant AAA.Strings.Vector :=
                   AAA.Strings.Empty_Vector & "line";
   begin
      AAA.Filesystem.Write_Lines (Path, Original);

      --  Successful save takes place at once

      declare
         File : AAA.Text_IO.File := AAA.Text_IO.Load (Path, Backup => False);
      begin
         File.Lines.Append ("added");
         File.Save;
         Assert (AAA.Filesystem.Read_Lines (Path) = Original & "added",
                 "Changes not saved");
      end;

      --  Failed save raises, and leaving the scope does not raise again.
      --  Saving needs the file that we are removing here.

      AAA.Filesystem.Write_Lines (Path, Original);

      declare
         File : AAA.Text_IO.File := AAA.Text_IO.Load (Path, Backup => False);
      begin
         File.Lines.Append ("added");
         Ada.Directories.Delete_File (Path);

         begin
            File.Save;
            Assert (False, "Save failure went unnoticed");
         exception
            when Program_Error => -- Raised for a missing file to replace
               null;
         end;
      end;

      --  Without Save, the same failure does not leave the scope

      AAA.Filesystem.Write_Lines (Path, Original);

      declare
         File : AAA.Text_IO.File := AAA.Text_IO.Load (Path, Backup => False);
      begin
         File.Lines.Append ("added");
         Ada.Directories.Delete_File (Path);
      end;
   end;
end Aaa_Texts_Tests.File_Save_Failure;
