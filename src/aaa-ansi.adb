with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body AAA.ANSI is

   package Chars renames Ada.Characters.Wide_Wide_Latin_1;

   -----------
   -- Count --
   -----------

   function Count_Extra (Text : Wide_Wide_String) return Natural is
      Counting : Boolean := False;
      Extra    : Natural := 0;
   begin
      for Char of Text loop
         if Counting then
            Extra := Extra + 1;
            if Char = 'm' then
               Counting := False;
            end if;
         else
            if Char = Chars.ESC then
               Counting := True;
               Extra := Extra + 1;
            end if;
         end if;
      end loop;

      return Extra;
   end Count_Extra;

   ------------
   -- Length --
   ------------

   function Length (Text : Wide_Wide_String) return Natural is
   begin
      return Text'Length - Count_Extra (Text);
   end Length;

   ------------
   -- Length --
   ------------

   function Length (Text : UTF.UTF_String) return Natural
   is (Length (UTF.Wide_Wide_Strings.Decode (Text)));

end AAA.ANSI;
