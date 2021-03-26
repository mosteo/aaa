package body AAA.Caches.Files is

   -------------
   -- Discard --
   -------------

   overriding
   procedure Discard (This : in out Cache) is
   begin
      This.Data := (Valid => False);
   end Discard;

   -------------
   -- Element --
   -------------

   function Element (This     : in out Cache;
                     Filename : String)
                     return Cached is

      use type Ada.Calendar.Time;
      use type Ada.Directories.File_Size;

      function Unchanged return Boolean
      is (This.Data.Valid and then
          This.Data.Size = Ada.Directories.Size (Filename) and then
          This.Data.Time = Ada.Directories.Modification_Time (Filename));

   begin
      if This.Has_Element and then Unchanged then
         return This.Data.Value;
      else
         This.Data :=
           (Valid => True,
            Size  => Ada.Directories.Size (Filename),
            Time  => Ada.Directories.Modification_Time (Filename),
            Value => Load (Filename));

         return This.Data.Value;
      end if;
   end Element;

   ---------
   -- Set --
   ---------

   procedure Set (This     : in out Cache;
                  Value    : Cached;
                  Filename : String)
   is
   begin
      Write (Value, Filename);
      This.Data :=
        (Valid => True,
         Size  => Ada.Directories.Size (Filename),
         Time  => Ada.Directories.Modification_Time (Filename),
         Value => Load (Filename));
   end Set;

end AAA.Caches.Files;
