with AAA.Filesystem.Locks;

with Ada.Directories;

with GNAT.OS_Lib;

pragma Alire_Test;

procedure Aaa_Tests.Filesystem.Locks is

   package Locks renames AAA.Filesystem.Locks;

   Temp : AAA.Filesystem.Temp_File := AAA.Filesystem.New_Name;
   Path : constant String := Temp.Filename;

   First, Second : Locks.Lock;
   --  Declared after Temp, so released before it deletes the file

begin
   --  The file is created when missing

   Assert (First.Try_Acquire (Path), "Could not lock a new file");
   Assert (First.Is_Held, "Lock not reported as held");
   Assert (Ada.Directories.Exists (Path), "Lock file not created");

   --  Nobody else gets it while held, also within the same process

   Assert (not Second.Try_Acquire (Path), "Lock held twice");
   Assert (not Second.Is_Held, "Failed lock reported as held");

   --  Until released

   First.Release;
   Assert (not First.Is_Held, "Released lock reported as held");
   Assert (Second.Try_Acquire (Path), "Released lock not available");

   --  Finalization releases it too

   Second.Release;

   declare
      Third : Locks.Lock;
   begin
      Third.Acquire (Path); -- Free, so it doesn't wait
      Assert (not First.Try_Acquire (Path), "Lock held twice by Acquire");
   end;

   Assert (First.Try_Acquire (Path), "Lock not released on finalization");
   First.Release;

   --  Shared locks coexist, but not with an exclusive one

   Assert (First.Try_Acquire (Path, Locks.Shared), "Could not share a lock");
   Assert (Second.Try_Acquire (Path, Locks.Shared), "Lock not shared");

   declare
      Third : Locks.Lock;
   begin
      Assert (not Third.Try_Acquire (Path), "Exclusive over shared locks");
      First.Release;
      Assert (not Third.Try_Acquire (Path), "Exclusive over a shared lock");
      Second.Release;
      Assert (Third.Try_Acquire (Path), "Exclusive after shared locks");
      Assert (not First.Try_Acquire (Path, Locks.Shared),
              "Shared over an exclusive lock");
   end;

   --  A file that can't be written can still be locked

   declare
      Read_Only : AAA.Filesystem.Temp_File := AAA.Filesystem.New_Name;
      Ok        : Boolean;
      Third     : Locks.Lock;
   begin
      GNAT.OS_Lib.Close (GNAT.OS_Lib.Create_File (Read_Only.Filename,
                                                  GNAT.OS_Lib.Binary),
                         Ok);
      Assert (Ok, "Could not create file");
      GNAT.OS_Lib.Set_Read_Only (Read_Only.Filename);

      Assert (Third.Try_Acquire (Read_Only.Filename, Locks.Shared),
              "Could not lock a read-only file");
      Third.Release;

      GNAT.OS_Lib.Set_Writable (Read_Only.Filename); -- So it can be deleted
   end;
end Aaa_Tests.Filesystem.Locks;
