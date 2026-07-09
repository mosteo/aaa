package AAA.ANSI with Pure is

   Reset_All : constant String;
   --  Resets the device to its original state. This may include (if
   --  applicable): reset graphic rendition, clear tabulation stops, reset
   --  to default font, and more.

   type States is (Off, On);

   function Scrub (Sequence : String) return String;
   --  Remove all ANSI formatting from a string;

   function Shorten (Sequence : String) return String is (Sequence);
   --  Some consecutive commands can be combined, resulting in a shorter
   --  string. Currently does nothing, but included for future optimization.

   -----------
   -- COLOR --
   -----------

   type Colors is
     (Default, -- Implementation defined according to ANSI

      Black,
      Red,
      Green,
      Yellow,
      Blue,
      Magenta,
      Cyan,
      Grey,

      --  Note: these light variants might not work in older terminals. In
      --  general, the bold + [light] color combination will result in the
      --  same bright color
      Light_Black,
      Light_Red,
      Light_Green,
      Light_Yellow,
      Light_Blue,
      Light_Magenta,
      Light_Cyan,
      Light_Grey);

   Reset : constant String;
   --  Back to defaults. Applies to colors & styles.

   function Foreground (Color : Colors) return String;
   function Background (Color : Colors) return String;
   --  Basic palette, 8/16 colors

   subtype Palette_RGB is Natural range 0 .. 5;
   --  Used for the 256-palette colors. Actual colors in this index-mode
   --  palette can slightly vary from terminal to terminal.

   function Palette_Fg (R, G, B : Palette_RGB) return String;
   function Palette_Bg (R, G, B : Palette_RGB) return String;

   subtype Greyscale is Natural range 0 .. 23;
   --  Drawn from the same palette mode. 0 is black, 23 is white

   function Foreground (Level : Greyscale) return String;
   function Background (Level : Greyscale) return String;

   subtype True_RGB is Natural range 0 .. 255;
   --  Modern terminals support true 24-bit RGB color

   function Foreground (R, G, B : True_RGB) return String;
   function Background (R, G, B : True_RGB) return String;

   Default_Foreground : constant String;
   Default_Background : constant String;

   function Color_Wrap (Text       : String;
                        Foreground : String := "";
                        Background : String := "")
                        return String;
   --  Wraps text between opening color and closing Defaults. See the combo
   --  color+styles below.

   ------------
   -- STYLES --
   ------------

   type Styles is
     (Default,     -- equivalent to Default_Foreground/Background
      Bright,      -- aka Bold
      Dim,         -- aka Faint
      Italic,
      Underline,
      Blink,
      Rapid_Blink, -- ansi.sys only
      Invert,      -- swaps fg/bg, aka reverse video
      Conceal,     -- aka hide
      Strike,      -- aka crossed-out
      Fraktur,     -- rarely supported, gothic style
      Double_Underline);

   function Style (Style : Styles; Active : States := On) return String;
   --  Apply/Remove a style

   function Style_Wrap (Text  : String;
                        Style : Styles) return String;
   --  Wraps Text in the given style between On/Off sequences

   function Wrap (Text : String;
                  Style      : Styles;
                  Foreground : String := "";
                  Background : String := "")
                  return String;

   ------------
   -- CURSOR --
   ------------

   --  Cursor movement. No effect if at edge of screen.

   function Back    (Cells : Positive := 1) return String;
   function Down    (Lines : Positive := 1) return String;
   function Forward (Cells : Positive := 1) return String;
   function Up      (Lines : Positive := 1) return String;

   function Next     (Lines : Positive := 1) return String;
   function Previous (Lines : Positive := 1) return String;
   --  Move to the beginning of the next/prev lines. Not in ansi.sys

   function Horizontal (Column : Positive := 1) return String;
   --  Move to a certain absolute column. Not in ansy.sys

   function Position (Row, Column : Positive := 1) return String;
   --  1, 1 is top-left

   Store     : constant String;
   --  Store cursor position. Private SCO extension, may work in current vts
   Restore   : constant String;
   --  Restore cursor position to the previously stored one

   Hide      : constant String;
   Show      : constant String;
   --  DECTCEM private extension, may work in current vts

   --------------
   -- CLEARING --
   --------------

   Clear_Screen            : constant String;
   Clear_Screen_And_Buffer : constant String;
   --  Clear also the backscroll buffer

   Clear_To_Beginning_Of_Screen : constant String;
   Clear_To_End_Of_Screen       : constant String;
   --  From the cursor position

   Clear_Line : constant String;
   --  Does not change cursor position (neither the two following).

   Clear_To_Beginning_Of_Line : constant String;
   Clear_To_End_Of_Line       : constant String;

   function Scroll_Up   (Lines : Positive) return String;
   --  Adds lines at bottom
   function Scroll_Down (Lines : Positive) return String;
   --  Adds lines at top

private

   ESC        : constant Character := ASCII.ESC;
   CSI        : constant String    := ESC & '[';

   Reset_All : constant String := ESC & "c";

   --  Helpers for the many int-to-str conversions

   function Tail (S : String) return String is
     (S (S'First + 1 .. S'Last));

   function Img (I : Natural) return String is
     (Tail (I'Img));

   ------------
   -- COLORS --
   ------------

   Reset : constant String := CSI & "0m";
   --  Back to defaults. Applies to colors & styles.

   function Foreground (Color : Colors) return String is
     (CSI
      & (case Color is
            when Default       => "39",
            when Black         => "30",
            when Red           => "31",
            when Green         => "32",
            when Yellow        => "33",
            when Blue          => "34",
            when Magenta       => "35",
            when Cyan          => "36",
            when Grey          => "37",
            when Light_Black   => "90",
            when Light_Red     => "91",
            when Light_Green   => "92",
            when Light_Yellow  => "93",
            when Light_Blue    => "94",
            when Light_Magenta => "95",
            when Light_Cyan    => "96",
            when Light_Grey    => "97")
      & "m");
   function Background (Color : Colors) return String is
     (CSI
      & (case Color is
            when Default       => "49",
            when Black         => "40",
            when Red           => "41",
            when Green         => "42",
            when Yellow        => "43",
            when Blue          => "44",
            when Magenta       => "45",
            when Cyan          => "46",
            when Grey          => "47",
            when Light_Black   => "100",
            when Light_Red     => "101",
            when Light_Green   => "102",
            when Light_Yellow  => "103",
            when Light_Blue    => "104",
            when Light_Magenta => "105",
            when Light_Cyan    => "106",
            when Light_Grey    => "107")
      & "m");

   function Bit8 (R, G, B : Palette_RGB) return String is
     (Img (16 + 36 * R + 6 * G + B));

   Fg : constant String := "38";
   Bg : constant String := "48";

   function Palette_Fg (R, G, B : Palette_RGB) return String is
      (CSI & Fg & ";5;" & Bit8 (R, G, B) & "m");
   function Palette_Bg (R, G, B : Palette_RGB) return String is
      (CSI & Bg & ";5;" & Bit8 (R, G, B) & "m");

   function Foreground (Level : Greyscale) return String is
      (CSI & Fg & ";5;" & Img (232 + Level) & "m");
   function Background (Level : Greyscale) return String is
      (CSI & Bg & ";5;" & Img (232 + Level) & "m");

   function Foreground (R, G, B : True_RGB) return String is
      (CSI & Fg & ";2;" & Img (R) & ";" & Img (G) & ";" & Img (B) & "m");
   function Background (R, G, B : True_RGB) return String is
      (CSI & Bg & ";2;" & Img (R) & ";" & Img (G) & ";" & Img (B) & "m");

   Default_Foreground : constant String := CSI & "39m";
   Default_Background : constant String := CSI & "49m";

   function Color_Wrap (Text       : String;
                        Foreground : String := "";
                        Background : String := "")
                        return String is
     ((if Foreground /= "" then Foreground else "")
      & (if Background /= "" then Background else "")
      & Text
      & (if Background /= "" then Default_Background else "")
      & (if Foreground /= "" then Default_Foreground else ""));

   ------------
   -- STYLES --
   ------------

   function Style (Style : Styles; Active : States := On) return String is
     (CSI
      & (case Active is
            when On  =>
           (case Style is
               when Default          => "39",
               when Bright           => "1",
               when Dim              => "2",
               when Italic           => "3",
               when Underline        => "4",
               when Blink            => "5",
               when Rapid_Blink      => "6",
               when Invert           => "7",
               when Conceal          => "8",
               when Strike           => "9",
               when Fraktur          => "20",
               when Double_Underline => "21"
           ),
            when Off =>
           (case Style is
               when Default          => "49",
               when Bright           => "22",
               when Dim              => "22",
               when Italic           => "23",
               when Underline        => "24",
               when Blink            => "25",
               when Rapid_Blink      => "25",
               when Invert           => "27",
               when Conceal          => "28",
               when Strike           => "29",
               when Fraktur          => "23",
               when Double_Underline => "24"
           ))
      & "m");

   function Style_Wrap (Text  : String;
                        Style : Styles) return String is
     (AAA.ANSI.Style (Style, On)
      & Text
      & AAA.ANSI.Style (Style, Off));

   function Wrap (Text       : String;
                  Style      : Styles;
                  Foreground : String := "";
                  Background : String := "")
                  return String is
     (Style_Wrap (Style => Style,
                  Text  => Color_Wrap (Text       => Text,
                                       Foreground => Foreground,
                                       Background => Background)));

   ------------
   -- CURSOR --
   ------------

   function Cursor (N : Positive; Code : Character) return String is
     (CSI & Img (N) & Code) with Inline_Always;
   --  For common Cursor sequences

   function Back    (Cells : Positive := 1) return String is
     (Cursor (Cells, 'D'));
   function Down    (Lines : Positive := 1) return String is
     (Cursor (Lines, 'B'));
   function Forward (Cells : Positive := 1) return String is
     (Cursor (Cells, 'C'));
   function Up      (Lines : Positive := 1) return String is
     (Cursor (Lines, 'A'));

   function Next     (Lines : Positive := 1) return String is
     (Cursor (Lines, 'E'));
   function Previous (Lines : Positive := 1) return String is
     (Cursor (Lines, 'F'));

   function Horizontal (Column : Positive := 1) return String is
     (Cursor (Column, 'G'));

   function Position (Row, Column : Positive := 1) return String is
     (CSI & Img (Row) & ";" & Img (Column) & "H");

   Store     : constant String := CSI & "s";
   Restore   : constant String := CSI & "u";

   Hide      : constant String := CSI & "?25l";
   Show      : constant String := CSI & "?25h";

   --------------
   -- CLEARING --
   --------------

   Clear_Screen            : constant String := CSI & "2J";
   Clear_Screen_And_Buffer : constant String := CSI & "3J";

   Clear_To_Beginning_Of_Screen : constant String := CSI & "2J";
   Clear_To_End_Of_Screen       : constant String := CSI & "0J";

   Clear_Line : constant String := CSI & "2K";

   Clear_To_Beginning_Of_Line : constant String := CSI & "1K";
   Clear_To_End_Of_Line       : constant String := CSI & "0K";

   function Scroll_Up   (Lines : Positive) return String is
     (CSI & Img (Lines) & "S");
   function Scroll_Down (Lines : Positive) return String is
     (CSI & Img (Lines) & "T");

   -----------
   -- Scrub --
   -----------

   function Scrub (Clean    : String;
                   Dirty    : String;
                   Cleaning : Boolean := False)
                   return String
   is (if Dirty = ""
       then Clean
       else
         (if Cleaning and then Dirty (Dirty'First) /= 'm' then
               Scrub (Clean, Dirty (Dirty'First + 1 .. Dirty'Last), True)
          elsif Cleaning and then Dirty (Dirty'First) = 'm' then
               Scrub (Clean, Dirty (Dirty'First + 1 .. Dirty'Last), False)
          elsif not Cleaning and then Dirty (Dirty'First) = ESC then
               Scrub (Clean, Dirty (Dirty'First + 1 .. Dirty'Last), True)
          elsif not Cleaning and then Dirty (Dirty'First) /= ESC then
               Scrub (Clean & Dirty (Dirty'First),
                      Dirty (Dirty'First + 1 .. Dirty'Last), False)
          else
             raise Program_Error with "Unexpected state while scrubbing with "
          & "clean=" & Clean
          & " dirty=" & Dirty
          & " cleaning=" & Cleaning'Image));

   function Scrub (Sequence : String) return String
   is (Scrub ("", Sequence));

end AAA.ANSI;
