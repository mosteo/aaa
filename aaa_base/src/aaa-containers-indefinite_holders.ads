with Ada.Finalization;

generic
   type Held (<>) is private;
   with function "=" (L, R : Held) return Boolean is <>;
package AAA.Containers.Indefinite_Holders with Preelaborate is

   --  Simple holder to work around GNAT holders bug. Comparison uses the held
   --  value.

   type Holder is tagged private;

   overriding function "=" (L, R : Holder) return Boolean;

   Empty_Holder : constant Holder;

   procedure Clear (This : in out Holder);

   procedure Hold (This : in out Holder; Elem : Held);

   function To_Holder (Elem : Held) return Holder with
     Post => To_Holder'Result.Is_Valid;

   function Is_Empty (This : Holder) return Boolean;

   function Is_Valid (This : Holder) return Boolean is (not This.Is_Empty);

   function Element (This : Holder) return Held with
     Pre => This.Is_Valid;

   type Reference_Value (Element : access Held) is limited null record with
     Implicit_Dereference => Element;

   function Reference (This : in out Holder) return Reference_Value with
     Pre => This.Is_Valid;

   function Ref (This : in out Holder) return Reference_Value
                 renames Reference;

   function Unchecked_Reference (This : Holder) return access Held with
     Pre => This.Is_Valid;

   type Const_Ref_Value (Element : access constant Held) is limited null record
     with Implicit_Dereference => Element;

   function Get (This : Holder) return Const_Ref_Value with
     Pre => This.Is_Valid;

   function Constant_Reference (This : Holder) return Const_Ref_Value
     renames Get;

private

   type Held_Access is access all Held;

   type Holder is new Ada.Finalization.Controlled with record
      Item : Held_Access;
   end record;

   overriding
   procedure Adjust (This : in out Holder);

   overriding
   procedure Finalize (This : in out Holder);

   Empty_Holder : constant Holder := (Ada.Finalization.Controlled with
                                      Item => null);

end AAA.Containers.Indefinite_Holders;
