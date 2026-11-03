private with Ada.Finalization;
private with GNAT.OS_Lib;

package AAA.Filesystem.Locks is

   --  Exclusive advisory locks on files, to keep processes off a resource that
   --  another process is using. Only processes that lock the same file notice
   --  them. A lock belongs to an open file of the process holding it, so the
   --  OS releases it when that process ends, whatever the reason, and no lock
   --  can be left behind. Two Lock objects exclude each other also within one
   --  process. Programs spawned by the holder don't inherit the lock.

   --  On filesystems where the OS can't lock files, and on NFS under Linux,
   --  where trying may hang, the lock is taken as held without excluding
   --  anyone.

   type Lock is tagged limited private;

   function Is_Held (This : Lock) return Boolean;

   function Try_Acquire (This : in out Lock; Path : String) return Boolean
     with Pre => not This.Is_Held;
   --  Lock Path, which is created empty if missing. Returns False without
   --  waiting if another Lock holds it. Raises Ada.IO_Exceptions.Use_Error if
   --  the file can't be opened or created.

   procedure Acquire (This : in out Lock; Path : String)
     with Pre => not This.Is_Held;
   --  As Try_Acquire, but waits for as long as another Lock holds the file

   procedure Release (This : in out Lock);
   --  Nothing to do if not held. Also done on finalization.

private

   type Lock is new Ada.Finalization.Limited_Controlled with record
      File : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
   end record;

   overriding procedure Finalize (This : in out Lock);

end AAA.Filesystem.Locks;
