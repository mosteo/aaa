with Ada.Unchecked_Deallocation;

package body AAA.Containers.Indefinite_Holders is

   ---------
   -- "=" --
   ---------

   overriding function "=" (L, R : Holder) return Boolean is
   begin
      if L.Is_Empty xor R.Is_Empty then
         return False;
      end if;

      if L.Is_Empty and then R.Is_Empty then
         return True;
      end if;

      return "=" (L.Item.all, R.Item.all);
   end "=";

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Holder) is
   begin
      This.Finalize;
   end Clear;

   ---------
   -- Get --
   ---------

   function Get (This : Holder) return Const_Ref_Value
   is (Element => This.Item);

   ----------
   -- Hold --
   ----------

   procedure Hold (This : in out Holder; Elem : Held) is
   begin
      This := To_Holder (Elem);
   end Hold;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder (Elem : Held) return Holder is
     (Ada.Finalization.Controlled with
        Item => new Held'(Elem));

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Holder) return Boolean is (This.Item = null);

   ---------------
   -- Reference --
   ---------------

   function Reference (This : in out Holder) return Reference_Value is
     (Element => This.Item);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (This : in out Holder) is
   begin
      if This.Item /= null then
         This.Item := new Held'(This.Item.all);
      end if;
   end Adjust;

   -------------
   -- Element --
   -------------

   function Element (This : Holder) return Held is (This.Item.all);

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Holder) is
      procedure Free is new Ada.Unchecked_Deallocation (Held, Held_Access);
   begin
      Free (This.Item);
   end Finalize;

   -------------------------
   -- Unchecked_Reference --
   -------------------------

   function Unchecked_Reference (This : Holder) return access Held is
     (This.Item);

end AAA.Containers.Indefinite_Holders;
