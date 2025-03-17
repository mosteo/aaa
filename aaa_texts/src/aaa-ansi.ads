with Ada.Strings.UTF_Encoding;

package AAA.ANSI with Preelaborate is

   package UTF renames Ada.Strings.UTF_Encoding;

   function Count_Extra (Text : Wide_Wide_String) return Natural;
   --  Compute how many characters in Text are actually ANSI control sequences

   function Length (Text : Wide_Wide_String) return Natural;
   --  Compute the real length of Text, without embedded ANSI control
   --  sequences, and considering Unicode grapheme clusters, emoji
   --  sequences and east Asian width property.

   function Length (Text : UTF.UTF_String) return Natural;
   --  Same, but expects latin-1 or UTF-8 encoding

end AAA.ANSI;
