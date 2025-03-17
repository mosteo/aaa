generic
   type T (<>) is private; -- Some general type
   type D      is private; -- A definite alternative for T storage
   with function To_Definite   (V : T) return D is <>;
   with function To_Indefinite (V : D) return T is <>;
package AAA.Traits.Types with Pure is

   function "+" (V : T) return D renames To_Definite;
   function "+" (V : D) return T renames To_Indefinite;

end AAA.Traits.Types;
