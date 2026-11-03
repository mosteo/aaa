with Ada.IO_Exceptions;

package body AAA.Filesystem.Locks is

   package OS renames GNAT.OS_Lib;

   use type OS.File_Descriptor;

   type Outcomes is (Locked, Busy, Unsupported);

   function Lock_File (File : OS.File_Descriptor;
                       Kind : Kinds;
                       Wait : Boolean)
                       return Outcomes is separate;
   --  Lock File with the OS facility, which differs between Unix and Windows.
   --  Without Wait, returns Busy at once if locked in a conflicting way.

   ----------
   -- Open --
   ----------

   procedure Open (This : in out Lock; Path : String) is
      Unused : Boolean;
   begin
      This.File := OS.Open_Read_Write (Path, OS.Binary);

      if This.File = OS.Invalid_FD then
         This.File := OS.Create_New_File (Path, OS.Binary);
      end if;

      --  Another process may have created it in the meantime
      if This.File = OS.Invalid_FD then
         This.File := OS.Open_Read_Write (Path, OS.Binary);
      end if;

      --  Locking doesn't need write access, which we may lack
      if This.File = OS.Invalid_FD then
         This.File := OS.Open_Read (Path, OS.Binary);
      end if;

      if This.File = OS.Invalid_FD then
         raise Ada.IO_Exceptions.Use_Error with
           "Cannot open lock file " & Path & ": " & OS.Errno_Message;
      end if;

      OS.Set_Close_On_Exec (This.File, True, Unused);
   end Open;

   ----------------
   -- Lock_Using --
   ----------------

   function Lock_Using (This : in out Lock;
                        Path : String;
                        Kind : Kinds;
                        Wait : Boolean)
                        return Boolean
   is
   begin
      Open (This, Path);

      case Lock_File (This.File, Kind, Wait) is
         when Locked | Unsupported =>
            return True;
         when Busy =>
            This.Release;
            return False;
      end case;
   end Lock_Using;

   -----------------
   -- Try_Acquire --
   -----------------

   function Try_Acquire (This : in out Lock;
                         Path : String;
                         Kind : Kinds := Exclusive)
                         return Boolean
   is (Lock_Using (This, Path, Kind, Wait => False));

   -------------
   -- Acquire --
   -------------

   procedure Acquire (This : in out Lock;
                      Path : String;
                      Kind : Kinds := Exclusive) is
   begin
      if not Lock_Using (This, Path, Kind, Wait => True) then
         raise Program_Error with "Lock reported as busy after waiting";
      end if;
   end Acquire;

   -------------
   -- Is_Held --
   -------------

   function Is_Held (This : Lock) return Boolean
   is (This.File /= OS.Invalid_FD);

   -------------
   -- Release --
   -------------

   procedure Release (This : in out Lock) is
   begin
      --  Closing the file releases its lock

      if This.Is_Held then
         OS.Close (This.File);
         This.File := OS.Invalid_FD;
      end if;
   end Release;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Lock) is
   begin
      This.Release;
   end Finalize;

end AAA.Filesystem.Locks;
