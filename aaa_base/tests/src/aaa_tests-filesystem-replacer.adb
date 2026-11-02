with AAA.Filesystem;
with AAA.Processes;
with AAA.Strings;

with Ada.Directories;
with Ada.Streams.Stream_IO;

with GNAT.OS_Lib;

pragma Alire_Test (Should_Fail, False);

procedure Aaa_Tests.Filesystem.Replacer is

   package Dirs renames Ada.Directories;
   package OS   renames GNAT.OS_Lib;
   package SIO  renames Ada.Streams.Stream_IO;

   On_Windows : constant Boolean := OS.Directory_Separator = '\';

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

   -------------
   -- Replace --
   -------------

   procedure Replace (Path, Text : String; Backup : Boolean := False) is
      Rep : AAA.Filesystem.Replacer :=
              AAA.Filesystem.New_Replacement (Path,
                                              Backup            => Backup,
                                              Allow_No_Original => True);
   begin
      Write (Rep.Editable_Name, Text);
      Rep.Replace;
   end Replace;

begin
   Dirs.Create_Directory (Temp.Filename);

   declare
      Path : constant String := Dirs.Compose (Temp.Filename, "file");
   begin

      --  The file is created when missing, and replaced when not

      Replace (Path, "first");
      Assert (Read (Path) = "first", "Not created");

      Replace (Path, "second");
      Assert (Read (Path) = "second", "Not replaced");

      --  The backup is kept when requested

      Replace (Path, "third", Backup => True);
      Assert (Read (Path) = "third", "Not replaced with backup");
      Assert (Read (Path & ".prev") = "second", "Bad backup");

      if not On_Windows then
         declare
            Link   : constant String := Dirs.Compose (Temp.Filename, "link");
            Reader : SIO.File_Type;
            Seen   : String (1 .. 5);
         begin

            --  The file is not rewritten in place, so a concurrent reader
            --  keeps seeing the complete previous contents

            SIO.Open (Reader, SIO.In_File, Path);
            Replace (Path, "fourth");
            String'Read (SIO.Stream (Reader), Seen);
            SIO.Close (Reader);
            Assert (Seen = "third", "Rewritten in place: " & Seen);
            Assert (Read (Path) = "fourth", "Not replaced with a reader");

            --  Permissions are kept

            OS.Set_Executable (Path);
            Replace (Path, "fifth");
            Assert (OS.Is_Executable_File (Path), "Permissions were lost");

            --  A symbolic link remains as such, and its target is replaced

            declare
               Unused : constant AAA.Processes.Result :=
                          AAA.Processes.Run
                            (AAA.Strings.To_Vector ("ln")
                             .Append ("-s")
                             .Append (Dirs.Full_Name (Path))
                             .Append (Link));
            begin
               null;
            end;

            Replace (Link, "sixth");
            Assert (OS.Is_Symbolic_Link (Link), "Link was replaced");
            Assert (Read (Path) = "sixth", "Link target was not replaced");
         end;
      end if;

      --  Nothing is left behind

      declare
         procedure Check (Item : Dirs.Directory_Entry_Type) is
         begin
            Assert (False, "Leftover: " & Dirs.Simple_Name (Item));
         end Check;
      begin
         Dirs.Search (Temp.Filename, "aaa-*", Process => Check'Access);
      end;
   end;
end Aaa_Tests.Filesystem.Replacer;
