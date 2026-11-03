with Interfaces.C;

with System;

separate (AAA.Filesystem.Locks)
function Lock_File (File : OS.File_Descriptor;
                    Wait : Boolean)
                    return Outcomes
is
   use Interfaces.C;

   subtype DWORD is unsigned_long; -- 32 bits on Windows

   LOCKFILE_FAIL_IMMEDIATELY : constant := 1;
   LOCKFILE_EXCLUSIVE_LOCK   : constant := 2;
   ERROR_LOCK_VIOLATION      : constant := 33;

   type OVERLAPPED is record
      Internal      : System.Address := System.Null_Address;
      Internal_High : System.Address := System.Null_Address;
      Offset        : DWORD          := 0;
      Offset_High   : DWORD          := 0;
      Event         : System.Address := System.Null_Address;
   end record
     with Convention => C;
   --  Unused but required, for the start of the region to lock (0)

   function Get_OS_Handle (File : int) return System.Address;
   pragma Import (C, Get_OS_Handle, "_get_osfhandle");

   function Lock_File_Ex (File        : System.Address;
                          Flags       : DWORD;
                          Reserved    : DWORD;
                          Length_Low  : DWORD;
                          Length_High : DWORD;
                          Start       : access OVERLAPPED)
                          return int;
   pragma Import (Stdcall, Lock_File_Ex, "LockFileEx");

   function Get_Last_Error return DWORD;
   pragma Import (Stdcall, Get_Last_Error, "GetLastError");

   Region : aliased OVERLAPPED;

begin
   --  Lock the first byte, which is enough as everybody locks that one

   if Lock_File_Ex
     (Get_OS_Handle (int (File)),
      Flags       => LOCKFILE_EXCLUSIVE_LOCK
                       + (if Wait then 0 else LOCKFILE_FAIL_IMMEDIATELY),
      Reserved    => 0,
      Length_Low  => 1,
      Length_High => 0,
      Start       => Region'Access) /= 0
   then
      return Locked;
   elsif Get_Last_Error = ERROR_LOCK_VIOLATION then
      return Busy;
   else
      return Unsupported;
   end if;
end Lock_File;
