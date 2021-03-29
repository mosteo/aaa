package body AAA.Filesystem is

   -------------------
   -- Traverse_Tree --
   -------------------

   procedure Traverse_Tree (Start   : String;
                            Doing   : access procedure
                              (Item : Ada.Directories.Directory_Entry_Type;
                               Stop : in out Boolean);
                            Recurse : Boolean := False)
   is
      use Ada.Directories;

      procedure Go_Down (Item : Directory_Entry_Type) is
         Stop : Boolean := False;
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Doing (Item, Stop);
            if Stop then
               return;
            end if;

            if Recurse and then Kind (Item) = Directory then
               Traverse_Tree (Compose (Start, Simple_Name (Item)),
                              Doing, Recurse);
            end if;
         end if;
      end Go_Down;

   begin
      Search (Start,
              "",
              (Directory => True, Ordinary_File => True, others => False),
              Go_Down'Access);
   end Traverse_Tree;

end AAA.Filesystem;
