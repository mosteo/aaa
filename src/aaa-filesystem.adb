with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;

package body AAA.Filesystem is

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
      OK   : Boolean := False;
      Args : OS_Lib.Argument_List_Access;
   begin
      if Exists (Path) and then
        Kind (Path) = Directory and then
        OS_Lib.Directory_Separator = '\'
      then
         Args := OS_Lib.Argument_String_To_List ("-R /D /S " & Path & "\*");

         OS_Lib.Spawn ("attrib", Args.all, OK);
         OS_Lib.Free (Args);

         if not OK then
            raise Program_Error with "failed to change attributes of " & Path;
         end if;
      end if;
   end Ensure_Deletable;

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
                             Backup_Dir : String  := ".")
                             return Replacer is
   begin
      return This : constant Replacer :=
        (Ada.Finalization.Limited_Controlled with
         Length     => File'Length,
         Backup_Len => Backup_Dir'Length,
         Original   => File,
         Backup     => Backup,
         Backup_Dir => Backup_Dir,
         Temp_Copy  => new Temp_File'(New_Name (In_Folder => Backup_Dir)))
      do
         Ada.Directories.Copy_File (File, This.Temp_Copy.Filename);
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
   end Finalize;

end AAA.Filesystem;
