with AAA.Filesystem;
with AAA.Strings;

with Ada.Directories;

with GNAT.OS_Lib;

pragma Alire_Test (Should_Fail, False);

procedure Aaa_Tests.Filesystem.Temp_Names is

   Count : constant := 500;

   Names : AAA.Strings.Set;

   Pid : constant String :=
           AAA.Strings.Trim
             (GNAT.OS_Lib.Pid_To_Integer
                (GNAT.OS_Lib.Current_Process_Id)'Image);

   ----------
   -- Nest --
   ----------

   procedure Nest (Depth : Positive) is
      --  Keep all temporaries alive at the same time, in the same folder, as
      --  happens when replacers are nested.
      Temp : constant AAA.Filesystem.Temp_File :=
               AAA.Filesystem.New_Name;
      Name : constant String := Ada.Directories.Simple_Name (Temp.Filename);
   begin
      Assert (not Names.Contains (Name), "Repeated temp name: " & Name);
      Assert (AAA.Strings.Has_Prefix (Name, "aaa-" & Pid & "-"),
              "Temp name without pid: " & Name);
      Names.Insert (Name);

      if Depth < Count then
         Nest (Depth + 1);
      end if;
   end Nest;

   Start : constant String := Ada.Directories.Current_Directory;

begin

   --  Many names requested in a row, all alive at once, are all different

   Nest (1);
   Assert (Natural (Names.Length) = Count, "Unexpected number of names");

   --  The location of a temporary does not change with the current directory

   declare
      Temp : constant AAA.Filesystem.Temp_File := AAA.Filesystem.New_Name;
      Path : constant String := Temp.Filename;
   begin
      Ada.Directories.Set_Directory
        (Ada.Directories.Containing_Directory (Start));
      Assert (Temp.Filename = Path,
              "Temp path changed with current directory: "
              & Path & " -> " & Temp.Filename);
      Ada.Directories.Set_Directory (Start);
   end;

end Aaa_Tests.Filesystem.Temp_Names;
