package AAA.Caches with Preelaborate is

   type Cache is limited interface;

   procedure Discard (This : in out Cache) is abstract with
     Post'Class => not This.Has_Element;
   --  Discard the cached value and force a reload on next use

   function Has_Element (This : Cache) return Boolean is abstract;
   --  Say if there is a cached value

end AAA.Caches;
