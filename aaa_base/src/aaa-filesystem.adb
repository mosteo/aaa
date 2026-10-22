with AAA.Debug;
with AAA.Processes;
with AAA.Strings;

with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;
with GNAT.String_Hash;

with Interfaces;

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

   ----------------------
   -- Tempfile_Support --
   ----------------------

   Epoch : constant Ada.Real_Time.Time :=
             Ada.Real_Time.Time_Of (0, Ada.Real_Time.To_Time_Span (0.0));

   protected Tempfile_Support is
      procedure Next_Name (Name : in out String) with
        Pre => (for all Char of Name => Char = '?');
      --  Replaces '?'s with random letters, never repeating a name already
      --  given by this process.
   private
      Next_Seed  : Interfaces.Unsigned_32 := 0;
      Used_Names : AAA.Strings.Set;
   end Tempfile_Support;

   protected body Tempfile_Support is

      ---------------
      -- Next_Name --
      ---------------

      procedure Next_Name (Name : in out String) is
         subtype Valid_Character is Character range 'a' .. 'z';
         package Char_Random is new
           Ada.Numerics.Discrete_Random (Valid_Character);
         Gen : Char_Random.Generator;

         --  The granularity of the default random seed is implementation
         --  defined (as coarse as 1 second in some cases), so two names
         --  requested close in time could clash. Instead, the seed hashes
         --  the time in nanoseconds, the current directory, and a counter
         --  that changes with every call in this process.

         use Ada.Real_Time;
         use type Interfaces.Unsigned_32;

         Nano : constant String :=
                  AAA.Strings.Replace (To_Duration (Clock - Epoch)'Image,
                                       ".", "");
         --  This gives us an image without loss of precision and without
         --  having to be worried about overflows

         type Hash_Type is mod 2 ** 32;
         pragma Compile_Time_Error (Hash_Type'Size > Integer'Size,
                                    "Hash_Type is too large");

         function Hash is new GNAT.String_Hash.Hash
           (Char_Type => Character,
            Key_Type  => String,
            Hash_Type => Hash_Type);

         function To_Integer is
           new Ada.Unchecked_Conversion (Hash_Type, Integer);
         --  Ensure unsigned -> signed conversion doesn't bite us

         Seed : constant Hash_Type :=
                  Hash (Nano & " at " & Ada.Directories.Current_Directory
                        & "#" & Next_Seed'Image);
      begin
         Next_Seed := Next_Seed + 1;

         Char_Random.Reset (Gen, To_Integer (Seed));

         loop
            for I in Name'Range loop
               Name (I) := Char_Random.Random (Gen);
            end loop;

            --  Make totally sure that not even by random chance we are reusing
            --  a temporary name.

            exit when not Used_Names.Contains (Name);
         end loop;

         Used_Names.Insert (Name);
      end Next_Name;

   end Tempfile_Support;

   --------------
   -- New_Name --
   --------------

   function New_Name (In_Folder : String := ".") return Temp_File
   is
      Folder : constant String := Ada.Directories.Full_Name (In_Folder);
      --  Resolved now, so a later change of the current directory does not
      --  make this temporary point elsewhere.

      ----------------
      -- Fresh_Name --
      ----------------

      function Fresh_Name return String is
         Result : String :=
                    "aaa-"
                    & AAA.Strings.Trim
                      (GNAT.OS_Lib.Pid_To_Integer
                         (GNAT.OS_Lib.Current_Process_Id)'Image)
                    & "-????.tmp";
      begin
         Tempfile_Support.Next_Name (Result (Result'Last - 7 ..
                                             Result'Last - 4));

         --  Do not reuse a name already on disk (e.g. a leftover from a
         --  previous process with the same pid).

         if Ada.Directories.Exists (Ada.Directories.Compose (Folder, Result))
         then
            return Fresh_Name;
         end if;

         return Result;
      end Fresh_Name;

      Name : constant String := Fresh_Name;
   begin
      return (Ada.Finalization.Limited_Controlled with
              Name_Len   => Name'Length,
              Folder_Len => Folder'Length,
              Keep       => <>,
              Name       => Name,
              Folder     => Folder);
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
