with AAA.Filesystem;
with AAA.Strings;

with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;

with GNAT.OS_Lib;

pragma Alire_Test (Should_Fail, False);

procedure Aaa_Tests.Filesystem.Lines is

   package FS  renames AAA.Filesystem;
   package SIO renames Ada.Streams.Stream_IO;

   use type AAA.Strings.Vector;

   On_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';

   LF : constant Character := ASCII.LF;

   Empty : AAA.Strings.Vector renames AAA.Strings.Empty_Vector;

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
      Path : constant String := Ada.Directories.Compose (Temp.Filename, "f");

      procedure Check (Contents : String; Expected : AAA.Strings.Vector) is
      begin
         Write (Path, Contents);
         Assert (FS.Read_Lines (Path) = Expected,
                 "Unexpected lines: " & FS.Read_Lines (Path).Flatten ("|"));
      end Check;

      Latin_1 : constant String := "caf" & Character'Val (16#E9#);
      UTF_8   : constant String :=
                  "caf" & Character'Val (16#C3#) & Character'Val (16#A9#);
   begin

      --  Lines are split at terminators, with no line after the last one

      Check ("", Empty);
      Check ("" & LF, Empty & "");
      Check ("a", Empty & "a");
      Check ("a" & LF, Empty & "a");
      Check ("a" & LF & "b", Empty & "a" & "b");
      Check ("a" & LF & "b" & LF, Empty & "a" & "b");
      Check ("a" & LF & LF & "b" & LF & LF, Empty & "a" & "" & "b" & "");

      --  Bytes are kept, whatever they are

      Check (Latin_1 & LF & UTF_8 & LF, Empty & Latin_1 & UTF_8);
      Check ("form" & ASCII.FF & "feed" & LF,
             Empty & ("form" & ASCII.FF & "feed"));

      --  Text is saved with those bytes, adding a last terminator if missing.
      --  Windows uses its own terminators, which reading takes care of.

      FS.Write_Lines (Path, Empty & Latin_1 & "" & UTF_8);
      Assert (FS.Read_Lines (Path) = Empty & Latin_1 & "" & UTF_8,
              "Lines not read as saved");

      if not On_Windows then
         Assert (Read (Path) = Latin_1 & LF & LF & UTF_8 & LF,
                 "Unexpected bytes saved: " & Read (Path));
      end if;

      --  Saving replaces what was there, and no lines make an empty file

      FS.Write_Lines (Path, Empty);
      Assert (Natural (Ada.Directories.Size (Path)) = 0, "File not emptied");
      Assert (FS.Read_Lines (Path) = Empty, "No lines expected");

      --  Sizes that would not fit in the stack

      declare
         Line  : constant String (1 .. 1024) := (others => 'x');
         Count : constant := 16 * 1024;
         Many  : AAA.Strings.Vector;
      begin
         for I in 1 .. Count loop
            Many.Append (Line);
         end loop;

         FS.Write_Lines (Path, Many);
         Assert (FS.Read_Lines (Path) = Many, "Big file differs");
      end;

      --  Errors

      begin
         declare
            Unused : constant AAA.Strings.Vector :=
                       FS.Read_Lines (Path & ".missing");
         begin
            Assert (False, "Read of missing file should have failed");
         end;
      exception
         when Ada.IO_Exceptions.Name_Error => null;
      end;

      begin
         FS.Write_Lines (Temp.Filename, Empty & "into a folder");
         Assert (False, "Write into a folder should have failed");
      exception
         when Ada.IO_Exceptions.Use_Error => null;
      end;
   end;
end Aaa_Tests.Filesystem.Lines;
