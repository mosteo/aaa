with Ada.Containers;

generic
   pragma Warnings (Off); -- For the unreferenced entities
   type Container is private;
   type Element (<>) is private;

   with procedure Append (C : in out Container; E : Element; Count : Ada.Containers.Count_Type);

   type Cursor is private;
   with function First (C : Container) return Cursor;
   with function Next  (Pos : Cursor) return Cursor;
   with function Has_Element (Pos : Cursor) return Boolean;

--     type Reference_Type (E : not null access Element) is limited private;
   with function Reference (Col : aliased in out Container; Pos : Cursor) return not null access Element;

--     type Constant_Reference_Type (E : not null access constant Element) is limited private;
   with function Constant_Reference (Col : aliased Container; Pos : Cursor) return not null access constant Element;
   pragma Warnings (On);
package AAA.Traits.Containers with Preelaborate is

   --  Reuse the gist of standard containers

   --  The above reference types bug out during instatiations (E is not visible)
   --  That's the reason for the alternate Reference signatures

end AAA.Traits.Containers;
