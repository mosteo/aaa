private with Ada.Calendar;
private with Ada.Directories;

generic
   type Cached is private;
   with function Load (Filename : String) return Cached;
   with procedure Write (Data : Cached; Filename : String);
package AAA.Caches.Files is

   type Cache is new Caches.Cache with private;

   overriding
   procedure Discard (This : in out Cache);

   function Element (This     : in out Cache;
                     Filename : String)
                     return Cached;

   type Cached_Info is record
      Element : Cached;
      Reused  : Boolean; -- True on cache hit
   end record;

   function Element (This     : in out Cache;
                     Filename : String)
                     return Cached_Info;

   overriding
   function Has_Element (This : Cache) return Boolean;

   procedure Set (This     : in out Cache;
                  Value    : Cached;
                  Filename : String);
   --  Store with write-through the cached value. To ensure consistency, the
   --  value is immediately reloaded. Hence, if there is any inconsistency
   --  between load/write, at least the value used will match the one on disk.

private

   type Cache_Data (Valid : Boolean := False) is record
      case Valid is
         when False => null;
         when True =>
            Size  : Ada.Directories.File_Size;
            Time  : Ada.Calendar.Time;
            Value : aliased Cached;
      end case;
   end record;

   type Cache is new Caches.Cache with record
      Data : Cache_Data;
   end record;

   -----------------
   -- Has_Element --
   -----------------

   overriding
   function Has_Element (This : Cache) return Boolean
   is (This.Data.Valid);

end AAA.Caches.Files;
