with Ada.Directories;
private with Ada.Finalization;

package AAA.Filesystem is

   function Is_File (Path : String) return Boolean;

   function Is_Folder (Path : String) return Boolean;

   procedure Traverse_Tree (Start   : String;
                            Doing   : access procedure
                              (Item : Ada.Directories.Directory_Entry_Type;
                               Stop : in out Boolean);
                            Recurse : Boolean := False);

   procedure Backup_If_Existing (File     : String;
                                 Base_Dir : String := "");
   --  If File exists, copy to file.prev. If Base_Dir /= "", it is instead
   --  copied to Base_Dir / Simple_Name (file) & ".prev"

   procedure Remove_Folder_If_Empty (Path : String);
   --  Attempt to remove a folder, but do not complain if unable (because not
   --  empty or not existing).

   --  TEMP_FILE: obtain a temporary name with optional cleanup

   type Temp_File (<>) is tagged limited private;
   --  A RAII scoped type to manage a temporary file name.
   --  Creates an instance with a unique file name. This does nothing on disk.
   --  The user is responsible for using the temp file name as they see fit.
   --  The file is deleted once an object of this type goes out of scope.
   --  If the file/folder was never created on disk nothing will happen.

   function New_Name (In_Folder : String := ".") return Temp_File
     with Pre => In_Folder /= "";
   --  This finds a new random name; it does not create anything.

   function Filename (This : Temp_File) return String;
   --  The filename is a random sequence of 8 characters + ".tmp"

   procedure Keep (This : in out Temp_File);
   --  If Keep is called, the file/dir will not be erased on finalization. This
   --  allows creating a temporary that will be deleted in case of failure but
   --  kept in case of success.

   function With_Name (Name : String) return Temp_File;
   --  Allows initializing the tmp file with a desired name.

   --  REPLACER: Modify a file "in place" in a safe way (keeping old copy)

   type Replacer (<>) is tagged limited private;
   --  A scoped type to ensure that a file is updated and replaced without
   --  trouble. In case of failure, the original file remains untouched. So
   --  what happens is: 1) A copy to a temp file is made. 2) This file is
   --  modified and can be tested as the client sees fit. 3) If the new file is
   --  proper, the old one is renamed to .prev and the new one takes its place.

   function New_Replacement (File       : String;
                             Backup     : Boolean := True;
                             Backup_Dir : String  := "")
                             return Replacer;
   --  Receives a file to be modified, and prepares a copy in a temporary. If
   --  Backup, once the replacement is performed, the original file is kept as
   --  ".prev". Backup_Dir is used for this ".prev" file. When backup dir is
   --  empty, the containing directory of File is used.

   function Editable_Name (This : Replacer) return String;
   --  Obtain the editable copy full name

   procedure Replace (This : in out Replacer);
   --  Replace the original file with the edited copy. If this procedure is not
   --  called, on going out of scope the Replacer will remove the temporary and
   --  the original file remains untouched.

private

   type Temp_File (Name_Len, Folder_Len : Natural) is
     new Ada.Finalization.Limited_Controlled
   with record
      Keep   : Boolean := False;
      Name   : String (1 .. Name_Len);
      Folder : String (1 .. Folder_Len);
   end record;

   type Temp_File_Access is access Temp_File;

   overriding
   procedure Finalize (This : in out Temp_File);

   type Replacer (Length, Backup_Len : Natural) is
     new Ada.Finalization.Limited_Controlled
   with record
      Original   : String (1 .. Length);
      Temp_Copy  : Temp_File_Access;
      Backup     : Boolean := True;
      Backup_Dir : String (1 .. Backup_Len);
   end record;

   overriding procedure Finalize (This : in out Replacer);

end AAA.Filesystem;
