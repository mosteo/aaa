with Interfaces.C;

with System;

separate (AAA.Filesystem.Locks)
function Lock_File (File : OS.File_Descriptor;
                    Kind : Kinds;
                    Wait : Boolean)
                    return Outcomes
is
   use Interfaces.C;

   --  Same values on Linux, macOS and the BSDs

   LOCK_SH : constant := 1;
   LOCK_EX : constant := 2;
   LOCK_NB : constant := 4;
   EINTR   : constant := 4;

   --  EWOULDBLOCK is 11 on Linux and 35 on macOS and the BSDs. On each of
   --  them, the other value is EDEADLK, which flock doesn't report.

   function Is_Would_Block (Error : Integer) return Boolean
   is (Error = 11 or else Error = 35);

   NFS_Super_Magic : constant := 16#6969#;

   function Flock (File : int; Operation : int) return int;
   pragma Import (C, Flock, "flock");

   function Fstatfs (File : int; Buffer : System.Address) return int;
   pragma Import (C, Fstatfs, "fstatfs");

   ------------
   -- On_NFS --
   ------------

   function On_NFS return Boolean is
      Buffer : array (1 .. 512) of aliased long := (others => 0);
      --  Bigger than struct statfs on any of them (2168 bytes on macOS)
   begin
      --  Only Linux has f_type as the first field. Elsewhere, the first word
      --  holds block sizes or a version, never equal to the NFS magic number.

      return Fstatfs (int (File), Buffer'Address) = 0
        and then Buffer (Buffer'First) = NFS_Super_Magic;
   end On_NFS;

begin
   --  flock on NFS may wait forever even when told not to, so don't try

   if On_NFS then
      return Unsupported;
   end if;

   loop
      if Flock (int (File),
                (case Kind is
                    when Exclusive => LOCK_EX,
                    when Shared    => LOCK_SH)
                + (if Wait then 0 else LOCK_NB)) = 0
      then
         return Locked;
      end if;

      declare
         Error : constant Integer := OS.Errno;
      begin
         if Is_Would_Block (Error) then
            return Busy;
         elsif Error /= EINTR then
            return Unsupported;
         end if;
      end;
   end loop;
end Lock_File;
