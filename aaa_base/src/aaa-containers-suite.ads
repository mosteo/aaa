with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;

generic
   type Keys (<>) is private;
   type Stored (<>) is private;
   with function "<" (L, R : Keys) return Boolean is <>;
   with function "<" (L, R : Stored) return Boolean is <>;
package AAA.Containers.Suite with Preelaborate is

   --  A collection of ready-to-use containers with conversions among them

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps (Keys, Stored);
   package Sets is new Ada.Containers.Indefinite_Ordered_Sets (Stored);
   package Vecs is new Ada.Containers.Indefinite_Vectors (Positive, Stored);

   type Vector is new Vecs.Vector with null record;

   -------------------
   --  Conversions  --
   -------------------

end AAA.Containers.Suite;
