private with Ada.Finalization;
with Ada.Text_IO;

with AAA.Strings;

package AAA.Text_IO is

   subtype Line_Widths is Positive range 2 .. Positive'Last;
   --  We need to at least be able to
   --  w-
   --  r-
   --  i-
   --  t-
   --  e like this.

   Default_Line_Width : constant := 79;

   type Filling_Modes is (Greedy);
   --  More fancy modes exist, not implemented for now.

   procedure Put_Paragraph (Text        : String;
                            Line_Width  : Line_Widths := Default_Line_Width;
                            Line_Prefix : String := "";
                            Filling     : Filling_Modes := Greedy;
                            File        : Ada.Text_IO.File_Access :=
                              Ada.Text_IO.Standard_Output);
   --  Reformat Text and write it to the given File.
   --  Lines will be broken at either spaces or '-'.
   --  If Line_Width is too short for a word or syllable, the word will be
   --  mercilessly broken wherever the line is completed.
   --  Line_Prefix is prepended to all lines, only if
   --  Line_Prefix'Length < Line_Width - 2.
   --  Caveat: at least one full line will be allocated.

   --  A convenience type to hold a complete text file in memory as a vector of
   --  lines. On destruction, changes to the contents are written back to disk.
   --  A backup ".prev" file is also created by default.

   type File (<>) is tagged limited private;

   function Load (From       : String; -- path to file
                  Backup     : Boolean := True;
                  Backup_Dir : String  := "")
                  return File;
   --  Load a text file into memory. If Backup, when saving takes place the
   --  original is renamed to ".prev". Backup_Dir optionally designates where
   --  the backup file will be moved. When backup dir is empty, the containing
   --  directory of File is used.

   function Lines (This : aliased in out File)
                   return access Strings.Vector;

   procedure Append_Lines (File       : String;
                           Lines      : Strings.Vector;
                           Backup     : Boolean := True;
                           Backup_Dir : String  := "");
   --  Add the given lines to the end of the file. When backup dir is empty,
   --  the containing directory of File is used.

private

   type File (Length, Backup_Len : Natural) is
     new Ada.Finalization.Limited_Controlled
   with record
      Name       : String (1 .. Length);
      Lines      : aliased Strings.Vector; -- The final contents
      Orig       : Strings.Vector;         -- The original contents
      Backup     : Boolean := True;
      Backup_Dir : String (1 .. Backup_Len);
   end record;

   overriding procedure Finalize (This : in out File);

end AAA.Text_IO;
