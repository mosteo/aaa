with AAA.Filesystem.Locks;

with Ada.Directories;

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
end Aaa_Tests.Filesystem.Locks;
