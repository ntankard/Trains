-- Swinburne simple windows package for PC (implementation uses GNAT).
-- Subdivides 80x24 Text window indo sub-windows (type Window).
-- Supports non-overlapping sub-windows, string output
-- and Get_Line string input and Get_Char input.  Does not use Text_IO.
-- Original version for Meridian Ada: R K Allen, Swinburne, 22-Sep-92
-- Thread-safe version and
-- modified so that Window is no longer limited private 7-May-93
-- Renamed and converted to GNAT/DJGPP conio.  R K Allen  8-Aug-99
-- Converted to GNAT/AdaGraph.  R K Allen  24,31-Aug-99
-- Get_Char and Putch_At added 21-Sep-99.
-- Reduced to 24 lines for compatibility with DOS version.  7-Mar-02.
-- Remove Scroll_Up and Scroll_Down from spec (not available MaRTE_OS) 8-Feb-13
--
-- Versions of the package body exist for GNAT3.11 Win32 (using AdaGraph),
-- GNAT3.10 DOS (using conio) and MaRTE_OS (using console_management)

package Swindows is

   type Window is private;
   type Color is (Black, Blue, Green, Cyan, Red, Magenta, Brown,
      Light_Gray, Dark_Gray, Light_Blue, Light_Green,
      Light_Cyan, Light_Red, Light_Magenta, Yellow, White);
   subtype Background_Color is Color range Black..White;
   subtype Row_Range is Integer range 0..23;
   subtype Column_Range is Integer range 0..79;

   procedure Open(The_Window : in out Window;
      Top_Left_Row  : in Row_Range;
      Top_Left_Col  : in Column_Range;
      Bot_Right_Row : in Row_Range;
      Bot_Right_Col : in Column_Range;
      Heading       : in String;
      Foreground    : in Color := White;
      Background    : in Background_Color := Black);
   -- clears, frames and heads a window, returning its handle, The_Window.
   -- if heading'length > width of window then Heading is truncated.
   -- First call initialises the package.

   procedure Clear(The_Window : in Window);
   -- fill the window with its background

   procedure Put_Line(The_Window : in Window;
      Text   : in String;
      On_Row : in Row_Range := 0;
      Foreground : in Color := White;
      Background : in Color := Dark_Gray);
   -- outputs Text to window The_Window at row On_Row.  Rows count from 1
   -- within the frame; if zero then the text is appended to the
   -- lines within the window.  If Text is too long it is truncated; if
   -- short then the row is blank filled.
   -- If On_Row is out of range then Put_Line does nothing.
   -- The effective default attributes are those of the window; Background
   -- must be in Background_Color -- Dark_Gray is artificial value here.

   procedure Get_Line(The_Window : in Window;
      Text : out String; Last : out Natural);
   -- effectively does a Text_IO.Get_Line with the cursor starting
   -- at the first unused line in window Win.  The input is
   -- limited to the internal width of the window - 1.

   procedure Get_Char(The_Window : in Window;
      Char : out Character);      -- effectively does a Text_IO.Get with the cursor starting
   -- at the first unused line in window Win.

   procedure Putch_At(C : in Character;
      X : in Column_Range; Y : in Row_Range;
      Foreground  : in Color;
      Background  : in Background_Color);
   -- displays C at row Y, column X
   -- If Open has not been called initialises the package.

   ---------------------------------------------------------
   -- The following two procedures are not thread-safe, ie output may not 
   -- appear at the specified position when there are concurrent tasks:

   procedure Gotoxy(X : in Column_Range; Y : in Row_Range);
   -- sets CurX and CurY

   procedure Putch(C : in Character;
      Foreground  : in Color;
      Background  : in Background_Color);
   -- displays C at row CurY, column CurX and increments CurX
   -- (unless already = 79, i.e. no line-wrap)
   -- This procedure is used internally but can be called directly.
   -- If Open has not been called initialises the package.

private
   type Window_Struct;
   type Window is access Window_Struct;
end Swindows;