with AAA.Debug;
with AAA.Processes;
with AAA.Strings;

with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;

package body AAA.Filesystem is

   -------------
   -- Is_File --
   -------------

   function Is_File (Path : String) return Boolean
   is (GNAT.OS_Lib.Is_Regular_File (Path));

   ---------------
   -- Is_Folder --
   ---------------

   function Is_Folder (Path : String) return Boolean
   is (GNAT.OS_Lib.Is_Directory (Path));

   ------------------------
   -- Backup_If_Existing --
   ------------------------

   procedure Backup_If_Existing (File     : String;
                                 Base_Dir : String := "")
   is
      use Ada.Directories;
      Dst : constant String :=
              (if Base_Dir /= ""
               then Compose (Base_Dir, Simple_Name (File) & ".prev")
               else File & ".prev");
   begin
      if Exists (File) then
         if not Exists (Base_Dir) then
            Create_Directory (Base_Dir);
         end if;

         Copy_File (File, Dst, "mode=overwrite");
      end if;
   end Backup_If_Existing;

   ----------------------
   -- Ensure_Deletable --
   ----------------------

   procedure Ensure_Deletable (Path : String) is
      use Ada.Directories;
      use GNAT;
      Args   : Strings.Vector;
      Result : Processes.Result;
   begin
      if Exists (Path) and then
        Kind (Path) = Directory and then
        OS_Lib.Directory_Separator = '\'
      then
         Args := Strings.Split ("-R /D /S", ' ').Append (Path & "\*");

         Result := Processes.Run (Strings.To_Vector ("attrib").Append (Args));

         if Result.Exit_Code /= 0 then
            raise Program_Error with "failed to change attributes of " & Path;
         end if;
      end if;
   end Ensure_Deletable;

   -------------------
   -- Relative_Path --
   -------------------

   function Relative_Path (From, Into : String) return String is
      package Adirs renames Ada.Directories;
      package OS renames GNAT.OS_Lib;
      Sep : constant Character := OS.Directory_Separator;
   begin
      if not OS.Is_Absolute_Path (From) or else not OS.Is_Absolute_Path (Into)
      then
         return Relative_Path (Adirs.Full_Name (From), Adirs.Full_Name (Into));
      end if;

      --  We have absolute paths to deal with from here on

      --  If there is not even the first char in common, these are in different
      --  drives (Windows). Cannot happen in UNIX-like systems.

      if From (From'First) /= Into (Into'First) then
         return Into;
      end if;

      --  If from is not a folder, this does not make sense either

      if not OS.Is_Directory (From) then
         return Into;
      end if;

      declare
         use type Strings.Vector;
         From_Parts : Strings.Vector := Strings.Split (From, Sep);
         Into_Parts : Strings.Vector := Strings.Split (Into, Sep);
      begin

         --  Remove spurious final segments in case Full_Name gives "this/"

         if From_Parts.Last_Element = "" then
            From_Parts.Delete_Last;
         end if;

         if Into_Parts.Last_Element = "" then
            Into_Parts.Delete_Last;
         end if;

         --  Remove common prefix

         while not From_Parts.Is_Empty
           and then not Into_Parts.Is_Empty
           and then From_Parts.First_Element = Into_Parts.First_Element
         loop
            From_Parts.Delete_First;
            Into_Parts.Delete_First;
         end loop;

         if From_Parts = Into_Parts then -- They're the same
            return ".";
         else

            --  Now the parts are rooted at a common ancestor. We go up if
            --  necessary and then down.

            declare
               Result : Strings.Vector;
            begin

               --  Up

               for I in 1 .. From_Parts.Length loop
                  Result.Append ("..");
               end loop;

               --  And down!

               Result.Append (Into_Parts); -- May be empty, it'd be OK

               return Result.Flatten (Sep);

            end;

         end if;
      end;
   end Relative_Path;

   ----------------------------
   -- Remove_Folder_If_Empty --
   ----------------------------

   procedure Remove_Folder_If_Empty (Path : String) is
      use Ada.Directories;
   begin
      Ada.Directories.Delete_Directory (Path);
   exception
      when Name_Error | Use_Error =>
         null;
   end Remove_Folder_If_Empty;

   -------------------
   -- Traverse_Tree --
   -------------------

   procedure Traverse_Tree (Start   : String;
                            Doing   : access procedure
                              (Item : Ada.Directories.Directory_Entry_Type;
                               Stop : in out Boolean);
                            Recurse : Boolean := False)
   is
      use Ada.Directories;

      procedure Go_Down (Item : Directory_Entry_Type) is
         Stop : Boolean := False;
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Doing (Item, Stop);
            if Stop then
               return;
            end if;

            if Recurse and then Kind (Item) = Directory then
               Traverse_Tree (Compose (Start, Simple_Name (Item)),
                              Doing, Recurse);
            end if;
         end if;
      end Go_Down;

   begin
      Search (Start,
              "",
              (Directory => True, Ordinary_File => True, others => False),
              Go_Down'Access);
   end Traverse_Tree;

   --------------
   -- New_Name --
   --------------

   function New_Name (In_Folder : String := ".") return Temp_File
   is
      subtype Valid_Character is Character range 'a' .. 'z';
      package Char_Random is new
        Ada.Numerics.Discrete_Random (Valid_Character);
      Gen : Char_Random.Generator;
   begin
      return Result : Temp_File := (Ada.Finalization.Limited_Controlled with
                                    Name_Len   => 12,
                                    Folder_Len => In_Folder'Length,
                                    Keep       => <>,
                                    Name       => "aaa-XXXX.tmp",
                                    Folder     => In_Folder)
      do
         Char_Random.Reset (Gen);
         for I in 5 .. 8 loop
            Result.Name (I) := Char_Random.Random (Gen);
         end loop;
      end return;
   end New_Name;

   --------------
   -- Filename --
   --------------

   function Filename (This : Temp_File) return String
   is (Ada.Directories.Compose
       (Ada.Directories.Full_Name (This.Folder), This.Name));

   ----------
   -- Keep --
   ----------

   procedure Keep (This : in out Temp_File) is
   begin
      This.Keep := True;
   end Keep;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (This : in out Temp_File) is
      use Ada.Directories;
   begin
      if This.Keep then
         return;
      end if;

      --  Force writability of folder when in Windows, as some tools (e.g. git)
      --  that create read-only files will cause a Use_Error

      Ensure_Deletable (This.Filename);

      if Exists (This.Filename) then
         if Kind (This.Filename) = Ordinary_File then
            Delete_File (This.Filename);
         elsif Kind (This.Filename) = Directory then
            Delete_Tree (This.Filename);
         end if;
      end if;
   exception
      when E : others =>
         Debug.Put_Exception (E);
   end Finalize;

   ---------------
   -- With_Name --
   ---------------

   function With_Name (Name : String) return Temp_File is
     (Temp_File'
        (Ada.Finalization.Limited_Controlled with
         Name_Len   => Name'Length,
         Name       => Name,
         Folder_Len => 1,
         Folder     => ".",
         Keep       => <>));

   --------------
   -- REPLACER --
   --------------

   -------------------
   -- Editable_Name --
   -------------------

   function Editable_Name (This : Replacer) return String
   is (This.Temp_Copy.Filename);

   ---------------------
   -- New_Replacement --
   ---------------------

   function New_Replacement (File       : String;
                             Backup     : Boolean := True;
                             Backup_Dir : String  := "";
                             Allow_No_Original : Boolean := False)
                             return Replacer
   is
      Backup_To : constant String :=
                    (if Backup_Dir /= ""
                     then Backup_Dir
                     else Ada.Directories.Containing_Directory (File));
   begin
      return This : constant Replacer :=
        (Ada.Finalization.Limited_Controlled with
         Length     => File'Length,
         Backup_Len => Backup_To'Length,
         Original   => File,
         Backup     => Backup,
         Backup_Dir => Backup_To,
         Temp_Copy  => new Temp_File'(New_Name (In_Folder => Backup_To)))
      do
         if Is_File (File) then
            Ada.Directories.Copy_File (File, This.Temp_Copy.Filename);

         elsif not Allow_No_Original then
            raise Program_Error
              with "Invalid original file for replacement: " & File;
         end if;
      end return;
   end New_Replacement;

   -------------
   -- Replace --
   -------------

   procedure Replace (This : in out Replacer) is
   begin
      if This.Backup then
         Backup_If_Existing (This.Original, This.Backup_Dir);
      end if;
      Ada.Directories.Copy_File (This.Editable_Name, This.Original);

      --  The temporary copy will be cleaned up by This.Temp_Copy finalization
   end Replace;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Replacer) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Temp_File, Temp_File_Access);
   begin
      Free (This.Temp_Copy);
   exception
      when E : others =>
         Debug.Put_Exception (E);
   end Finalize;

end AAA.Filesystem;
