pragma Alire_Test;

with AAA.Filesystem;
with AAA.Text_IO;

with Ada.Assertions;
with Ada.Directories;
with Ada.Streams.Stream_IO;

with GNAT.OS_Lib;

--  A text file is kept as the bytes it contains when edited, whatever these
--  are, and independently of the encoding the program was built with.

procedure Aaa_Texts_Tests.File_Bytes is

   use Ada.Assertions;

   package SIO renames Ada.Streams.Stream_IO;

   On_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';

   LF : constant Character := ASCII.LF;

   Valid   : constant String :=
               "t" & Character'Val (16#C3#) & Character'Val (16#AD#) & "tulo";
   --  "título" in UTF-8

   Invalid : constant String := "caf" & Character'Val (16#E9#);
   --  "café" in Latin-1, which is not valid UTF-8

   Temp : AAA.Filesystem.Temp_File := AAA.Filesystem.New_Name;

   -----------
   -- Write --
   -----------

   procedure Write (Path, Text : String) is
      File : SIO.File_Type;
   begin
      SIO.Create (File, SIO.Out_File, Path);
      String'Write (SIO.Stream (File), Text);
      SIO.Close (File);
   end Write;

   ----------
   -- Read --
   ----------

   function Read (Path : String) return String is
      File : SIO.File_Type;
   begin
      SIO.Open (File, SIO.In_File, Path);
      return Text : String (1 .. Natural (SIO.Size (File))) do
         String'Read (SIO.Stream (File), Text);
         SIO.Close (File);
      end return;
   end Read;

begin
   Ada.Directories.Create_Directory (Temp.Filename);

   declare
      Path : constant String :=
               Ada.Directories.Compose (Temp.Filename, "file");
   begin
      Write (Path, Valid & LF & Invalid & LF);

      --  Lines have the bytes in the file

      declare
         File : AAA.Text_IO.File := AAA.Text_IO.Load (Path, Backup => False);
      begin
         Assert (Natural (File.Lines.Length) = 2, "Bad line count");
         Assert (File.Lines.First_Element = Valid, "Bad line: valid UTF-8");
         Assert (File.Lines.Last_Element = Invalid, "Bad line: not UTF-8");

         File.Lines.Append ("added");
      end;

      --  And so does the file after saving it. Line terminators are the
      --  platform's on Windows, so there we rely on the check above.

      if not On_Windows then
         Assert (Read (Path) = Valid & LF & Invalid & LF & "added" & LF,
                 "Bytes changed when saving: " & Read (Path));
      end if;

      declare
         File : AAA.Text_IO.File := AAA.Text_IO.Load (Path, Backup => False);
      begin
         Assert (File.Lines.First_Element = Valid, "Bad saved line: UTF-8");
         Assert (File.Lines.Element (2) = Invalid, "Bad saved line: other");
         Assert (File.Lines.Last_Element = "added", "Bad added line");
      end;
   end;
end Aaa_Texts_Tests.File_Bytes;
