-- SWindows implementation v3.2 for GNAT 3.11+ Win32 simrail 1.4+/simrail2
--
-- Swindows = simple windows package for PC (subdivides 80x24 space)
-- simple text windows package for PC (implementation uses AdaGraph)
-- supports non-overlapping windows in text mode, string output
-- and Get_Line string input only.  Does not use Text_IO.
-- R K Allen, Swinburne UT, 22-Sep-92
--
-- concurrency locks added 25-Sep-92
-- polled input for concurrency added 28-Sep-92
-- restructured task KBD 7-May-93
-- Converted to GNAT conio  8-Aug-99
-- Converted to GNAT/AdaGraph.  R K Allen  24-Aug-99
-- Get_Line bug fixed, Lock now protected.  31-Aug-99
-- Revised for simrail 1.4.  17-Apr-01
-- Fixed Put_Line default background 27-May-02
-- Changed height to 24lines*12pixels and offset below simtrack. 18-May-07
-- Remove Scroll_Up and Scroll_Down from spec, comment out bodies. 8-Feb-13
-- todo: replace by a rotating mode where second colour is provided as param.
--
with Ada.Exceptions, Ada.Text_Io;  -- for debugging
with Adagraph;
use  Adagraph;
with Simtrack2.Display;
use Simtrack2.Display;   -- shared Adagraph window for simrail2

package body Swindows is

   type Window_Struct is
   record
      Top_Row   : Row_Range;        -- these coordinates exclude
      Left_Col  : Column_Range;     -- the frame
      Bot_Row   : Row_Range;
      Right_Col : Column_Range;
      Width     : Column_Range;        -- usable columns within the window
      Last_Row_In_Use : Row_Range;
      Foreground : Color := White;
      Background : Background_Color := Black;
   end record;

   X_Size : constant Integer := 641;    -- Horizontal window size
   Y_Size : constant Integer := 288;    -- Vertical window size
   Y_Offset : Integer := 0; -- determined by Simtrack

   ----------------------
   -- Global variables --
   ----------------------
-- these four now provided by Track  (17/04/00)
--   X_Max,  Y_Max  : Integer;  -- Maximum screen coordinates
--   X_Char, Y_Char : Integer;  -- Character size

   Cur_Row : Row_Range;
   Cur_Col : Column_Range;

   type Colored_Char is record
      Ch : Character := ' ';
      Fg : Color := White;
      Bg : Background_Color := Black;
   end record;
   Screen_Buffer : array (0..23, 0..79) of Colored_Char;

   -- task called by procedure Get_Line so that
   -- only one client task can do input at a time
   task Kbd is
      entry Get_Line(The_Window : in Window;
         Text : out String;
         Last : out Natural);
      entry Get_Char(The_Window : in Window;
         Text : out Character);
   end Kbd;

   protected type Lock is  -- binary semaphore
      entry Acquire;
      procedure Release;
   private
      Available : Boolean := True;
   end Lock;

   Bios : Lock;  -- protect from concurrent access
   First_Time : Boolean := True;

   procedure Init is
   begin
      First_Time := False;
      if X_Char = 0 then
         Ada.Text_Io.Put_Line("Error: SimTrack must be init'd before SWindows");
         raise Program_Error;
      end if;
--      Adagraph.Create_Sized_Graph_Window (X_Size, Y_Size, X_Max, Y_Max, X_Char, Y_Char);
--      Adagraph.Set_Window_Title ("Swindows Text Window for GNAT 3.11 NT/AdaGraph");
--      Adagraph.Clear_Window;
      Ada.Text_Io.Put_Line("Swindows v3.2 Text Window for GNAT 3.15 NT/Simrail2");
      Y_Offset := Y_Max - Y_Size;
      Ada.Text_Io.Put_Line("X_Max, Y_Max, X_Char, Y_Char="
         & X_Max'Img & Y_Max'Img &  X_Char'Img & Y_Char'Img );
      Adagraph.Draw_Box(0, Y_Offset, X_Char*80 - 1, Y_Char*24 - 1 + Y_Offset,
         Hue => Adagraph.Color_Type'Val(Color'Pos(Black)),
         Filled => Adagraph.Fill );
   end Init;

   protected body Lock is
      entry Acquire when Available is
      begin
         Available := False;
      end Acquire;

      procedure Release is
      begin
         Available := True;
      end Release;
   end Lock;

   procedure Gotoxy(X : in Column_Range; Y : in Row_Range) is
   begin
      Cur_Col := X;
      Cur_Row := Y;
   end Gotoxy;

   procedure Putch(C : in Character;
         Foreground  : in Color;
         Background  : in Background_Color) is
      --
      Left_Pixel : Integer := Cur_Col*X_Char;
      Top_Pixel  : Integer := Cur_Row*Y_Char + Y_Offset;
      S : String(1..1);
   begin
      if First_Time then Init; end if;
      Adagraph.Draw_Box(Left_Pixel, Top_Pixel, Left_Pixel + X_Char - 1, Top_Pixel + Y_Char - 1,
         Hue => Adagraph.Color_Type'Val(Color'Pos(Background)),
         Filled => Adagraph.Fill );
      S(1) := C;
      Adagraph.Display_Text(Left_Pixel, Top_Pixel,
         Text => S,
         Hue => Adagraph.Color_Type'Val(Color'Pos(Foreground)) );
      Screen_Buffer(Cur_Row, Cur_Col) := (C, Foreground, Background);
      if Cur_Col < Column_Range'Last then
         Cur_Col := Cur_Col + 1;
      end if;
   end Putch;

   procedure Putch_At(C : in Character;
         X : in Column_Range; Y : in Row_Range;
         Foreground  : in Color;
         Background  : in Background_Color) is
   -- displays C at row Y, column X.  Thread-safe.
   -- If Open has not been called initialises the package.
   begin
      Bios.Acquire;
      Gotoxy(x, Y);
      Putch(C, Foreground, Background);
      Bios.Release;
   end Putch_At;

   procedure Draw_Box (X1, Y1, X2, Y2 : in Natural;
         Foreground    : in Color;
         Background    : in Background_Color) is

      -- details (pixels):
      Width  : Integer := (X2 - X1 + 1) * X_Char;
      Height : Integer := (Y2 - Y1 + 1) * Y_Char;
      Left   : Integer := X1 * X_Char;
      Top    : Integer := Y1 * Y_Char + Y_Offset;

      -- for screen_buffer (mainly debugging):
      Space: Colored_Char := (' ', Foreground, Background);
--      Plus : Colored_Char := ('+', Foreground, Background);
--      Horz : Colored_Char := ('-', Foreground, Background);
--      Vert : Colored_Char := ('|', Foreground, Background);


--      procedure Draw_Line (X1, X2, Y : in Natural) is
--      begin
--         --         Gotoxy (X1, Y);
--         for X in X1 .. X2 loop
--            --            Putch ('-', Foreground, Background);
--            Screen_Buffer(Y,X) := Horz;
--         end loop;
--      end Draw_Line;

--      procedure Draw_Column (Y1, Y2, X : in Natural) is
--      begin
--         for Y in Y1 .. Y2 loop
--            --            Gotoxy (X, Y);
--            --            Putch ('|', Foreground, Background);
--            Screen_Buffer(Y,X) := Vert;
--         end loop;
--      end Draw_Column;

   begin
      Adagraph.Draw_Box(Left, Top, Left + Width - 1, Top + Height - 1,
         Hue => Adagraph.Color_Type'Val(Color'Pos(Background)),
         Filled => Adagraph.Fill );
      Adagraph.Draw_Box(Left + X_Char/2, Top + Y_Char/2,
         Left + Width - X_Char/2, Top + Height - Y_Char/2,
         Hue => Adagraph.Color_Type'Val(Color'Pos(Foreground)),
         Filled => Adagraph.No_Fill );
      for Y in Y1 .. Y2 loop
        for X in X1 .. X2 loop
          Screen_Buffer(Y,X) := Space;
        end loop;
      end loop;
--      Screen_Buffer(Y1,X1) := Plus;
--      Draw_Line (X1 + 1, X2 - 1, Y1);
--      Screen_Buffer(Y1,X2) := Plus;
--      Screen_Buffer(Y2, X1) := Plus;
--      Draw_Line (X1 + 1, X2 - 1, Y2);
--      Screen_Buffer(Y2, X2) := Plus;
--      Draw_Column (Y1 + 1, Y2 - 1, X1);
--      Draw_Column (Y1 + 1, Y2 - 1, X2);
   end Draw_Box;


   procedure Open(The_Window : in out Window;
         Top_Left_Row  : in Row_Range;
         Top_Left_Col  : in Column_Range;
         Bot_Right_Row : in Row_Range;
         Bot_Right_Col : in Column_Range;
         Heading       : in String;
         Foreground    : in Color := White;
         Background    : in Background_Color := Black ) is
      --
      -- clears frames and heads a window, returning its ID
      -- if heading'length > width of window then Heading is truncated.

      Len : Natural := Heading'Length;
   begin
      --     Ada.Text_Io.Put_Line("Open..." & heading);
      The_Window := new Window_Struct'(Top_Left_Row+1, Top_Left_Col+1,
         Bot_Right_Row-1, Bot_Right_Col-1,
         Bot_Right_Col - Top_Left_Col - 1,
         Top_Left_Row,
         Foreground, Background);
      Bios.Acquire;
      if First_Time then Init; end if;
      Draw_Box(Top_Left_Col, Top_Left_Row, Bot_Right_Col, Bot_Right_Row,
         Foreground, Background);

      if Len > The_Window.Width then Len := The_Window.Width; end if;

      Gotoxy(The_Window.Left_Col + (The_Window.Width - Len)/2, Top_Left_Row);
      for C in 0 .. Len - 1 loop
         Putch(Heading(Heading'First + C), Foreground, Background);
      end loop;
      Bios.Release;
      -- clear the inside of the window: (25-Aug-99 now done by Draw_Box)
      -- Clear(The_Window);
   exception
      when Constraint_Error =>
         Ada.Text_Io.Put_Line("Open failed, constraint_error");
      when Error : others =>
         Ada.Text_Io.Put_Line("Open failed");
         Ada.Text_Io.Put_Line(Ada.Exceptions.Exception_Message(Error));
   end Open;

   procedure Clear(The_Window : in Window) is
      -- fill the window with its background.
      -- details (pixels):
      Width  : Integer := The_Window.Width * X_Char;
      Height : Integer := (The_Window.Bot_Row - The_Window.Top_Row + 1) * Y_Char;
      Left   : Integer := The_Window.Left_Col * X_Char;
      Top    : Integer := The_Window.Top_Row * Y_Char + Y_Offset;
      Fill   : Colored_Char := (' ', The_Window.Foreground, The_Window.Background);
   begin
      --      Ada.Text_Io.Put_Line("in Clear...");
      Bios.Acquire;
      Adagraph.Draw_Box(Left, Top, Left + Width - 1, Top + Height - 1,
         Hue => Adagraph.Color_Type'Val(Color'Pos(The_Window.Background)),
         Filled => Adagraph.Fill );
      for R in The_Window.Top_Row .. The_Window.Bot_Row loop
         for C in The_Window.Left_Col .. The_Window.Right_Col loop
            Screen_Buffer(R,C) := Fill;
         end loop;
      end loop;
      Bios.Release;
      The_Window.Last_Row_In_Use := The_Window.Top_Row - 1;
   end Clear;

   procedure Put_Line(The_Window : in Window;
         Text   : in String;
         On_Row : in Row_Range := 0;      -- relative
         Foreground : in Color := White;
         Background : in Color := Dark_Gray) is
      --
      -- outputs Text to window The_Window at row On_Row.  Rows count from 1
      -- within the frame; if zero then the text is appended to the
      -- lines within the window.  If Text is too long it is truncated; if
      -- short then the row is blank filled.
      -- If On_Row is out of range then Put_Line does nothing.
      -- The effective default attributes are those of the window.

      The_Row : Row_Range;                -- absolute
      Len : Natural := Text'Length;
      Fg : Color := Foreground;
      Bg : Color := Background;
      Bgnd : Background_Color;
   begin
      --      Ada.Text_Io.Put_Line("Put_Line..." & Text);
      -- set default attributes as per the window:
      if Fg = White then
         Fg := The_Window.Foreground; -- (could be Bright_White)
      end if;
      --(pre 27/05/02)if Color'Pos(Bg) not in 0 .. Background_Color'Pos(Background_Color'Last) then
      if Bg = Dark_Gray then
         Bgnd := The_Window.Background;  -- ignore parameter, take default for window
      else
         Bgnd := Background_Color'Val(Color'Pos(Bg));  -- corresponds
      end if;
      -- handle default row, namely append:
      if On_Row = 0 then
         The_Row := The_Window.Last_Row_In_Use + 1;
      else
         The_Row := The_Window.Top_Row + On_Row - 1;
      end if;
      -- do nothing if off the bottom of the window interior
      if The_Row <= The_Window.Bot_Row then
         -- truncate the string if needed:
         if Len > The_Window.Width then Len := The_Window.Width; end if;
         -- output it:
         Bios.Acquire;
         Gotoxy(The_Window.Left_Col, The_Row);
         for C in 0 .. Len  - 1 loop
            Putch(Text(Text'First + C), Fg, Bgnd);
         end loop;
         for C in Len .. The_Window.Width - 1 loop
            Putch(' ', Fg, Bgnd);
         end loop;

         Bios.Release;
         -- keep track of the bottom-most row used:
         if The_Row > The_Window.Last_Row_In_Use then
            The_Window.Last_Row_In_Use := The_Row;
         end if;
      end if;
   end Put_Line;

--   procedure Scroll_Up(The_Window : in Window) is
--      -- scrolls the contents of the window up one line
--      -- Modified 12-May-94 rka > changed to >= in if below.
--      -- Modified 25-Aug-99 rka for Win32.
--      Cc : Colored_Char;
--   begin
--      if The_Window.Last_Row_In_Use >= The_Window.Top_Row then
--         Bios.Acquire;
--         for R in The_Window.Top_Row .. The_Window.Bot_Row - 1 loop
--            Gotoxy(The_Window.Left_Col, R);
--            for C in The_Window.Left_Col .. The_Window.Right_Col loop
--               Cc := Screen_Buffer(R + 1,C);
--               Putch(Cc.Ch, Cc.Fg, Cc.Bg);
--            end loop;
--         end loop;
--         Gotoxy(The_Window.Left_Col, The_Window.Bot_Row);
--         for C in The_Window.Left_Col .. The_Window.Right_Col loop
--            Putch(' ', The_Window.Foreground, The_Window.Background);
--         end loop;
--         Bios.Release;
--         The_Window.Last_Row_In_Use := The_Window.Last_Row_In_Use - 1;
--      end if;
--   end Scroll_Up;

--   procedure Scroll_Down(The_Window : in Window) is
--      -- scrolls the contents of the window down one line
--      Cc : Colored_Char;
--   begin
--      Bios.Acquire;
--      for R in reverse The_Window.Top_Row + 1 .. The_Window.Bot_Row loop
--         Gotoxy(The_Window.Left_Col, R);
--         for C in The_Window.Left_Col .. The_Window.Right_Col loop
--            Cc := Screen_Buffer(R - 1, C);
--            Putch(Cc.Ch, Cc.Fg, Cc.Bg);
--         end loop;
--      end loop;
--      Gotoxy(The_Window.Left_Col, The_Window.Top_Row);
--      for C in The_Window.Left_Col .. The_Window.Right_Col loop
--         Putch(' ', The_Window.Foreground, The_Window.Background);
--      end loop;
--      Bios.Release;
--      if The_Window.Last_Row_In_Use < The_Window.Bot_Row then
--         The_Window.Last_Row_In_Use := The_Window.Last_Row_In_Use + 1;
--      end if;
--   end Scroll_Down;

   procedure Get_Line(
         The_Window : in Window;
         Text : out String;
         Last : out Natural) is
      -- effectively does a Text_IO.Get_Line with the cursor starting
      -- at the first unused line in window Win.  The input is
      -- limited to the internal width of the window - 1.
      -- Modified 25,31-Aug-99 rka for Win32, including o'flo protection.
   begin
      Kbd.Get_Line(The_Window, Text, Last);
   end Get_Line;

   procedure Get_Char(
         The_Window : in Window;
         Char : out Character) is
      -- effectively does a Text_IO.Get with the cursor starting
      -- at the first unused line in window Win.
      -- Introduced 21-09-99 rka for Win32, including o'flo protection.
   begin
      Kbd.Get_Char(The_Window, Char);
   end Get_Char;

   task body Kbd is
   begin
      loop
         select
            accept Get_Line(The_Window : in Window;
                  Text : out String;
                  Last : out Natural) do
               declare
                  The_Row : Row_Range := The_Window.Last_Row_In_Use + 1;
                  Char : Character;
                  Scan : Integer;     -- code for Keypad keys (unused here)
                  N    : Natural := 0;
                  Max  : Natural := Text'Length;
                  Temp : String(1 .. Max);
               begin
                  if Max >= The_Window.Width then Max := The_Window.Width - 1; end if;
                  --                  Bios.Acquire;
                  --                  Gotoxy(The_Window.Left_Col, The_Row);
                  Put_Line(The_Window, ":");
                  The_Window.Last_Row_In_Use := The_Window.Last_Row_In_Use - 1;
                  --                  Bios.Release;
                  --          simulate a text_io.get_line by
                  loop     -- polling loop for single characters
                     if Adagraph.Key_Hit then
                        Char := Adagraph.Get_Key;
                        if Char=Ascii.Nul then
                           Scan := Character'Pos(Adagraph.Get_Key);
                        else
                           Scan := 0;
                        end if;
                        --      Ada.Text_Io.Put_Line("key='" & Char & "' (" & Scan'img & ").");
                        Bios.Acquire;
                        case Char is
                           when ' '..'~' =>  -- ordinary displayable char
                              if N < Max then N := N + 1; end if;
                              Temp(N) := Char;
                              Gotoxy(The_Window.Left_Col + N, The_Row);
                              Putch(Char,The_Window.Foreground, The_Window.Background);

                           when Ascii.Bs =>  -- backspace
                              if N > 0 then
                                 Gotoxy(The_Window.Left_Col + N, The_Row);
                                 Putch(' ', The_Window.Foreground, The_Window.Background);
                                 N := N - 1;
                              end if;

                           when Ascii.Cr | Ascii.Lf =>   -- Enter key
                              Gotoxy(The_Window.Left_Col, The_Row);
                              Putch(' ', The_Window.Foreground, The_Window.Background);
                              Bios.Release;
                              exit;

                           when others =>
                              null;
                        end case;
                        Bios.Release;
                     else
                        delay 0.02;
                     end if;
                  end loop;

                  if N > 0 then
                     Text(Text'First .. Text'First + N - 1) := Temp(1 .. N);
                  end if;
                  Last := Text'First + N - 1;
                  The_Window.Last_Row_In_Use := The_Row - 1;  -- forget we used one
               end; -- block
            end Get_Line;
         or
            accept Get_Char(The_Window : in Window;
                            Text       : out Character) do
               declare
                  The_Row : Row_Range := The_Window.Last_Row_In_Use + 1;
                  Char : Character;
                  Scan : Integer;     -- code for Keypad keys (unused here)
               begin
                  Put_Line(The_Window, ":");
                  loop     -- polling loop for single characters
                     if Adagraph.Key_Hit then
                        Char := Adagraph.Get_Key;
                        if Char=Ascii.Nul then
                           Scan := Character'Pos(Adagraph.Get_Key);
                        else
                           Scan := 0;
                        end if;
                        --      Ada.Text_Io.Put_Line("key='" & Char & "' (" & Scan'img & ").");
                        Bios.Acquire;
                        case Char is
                           when ' '..'~' =>  -- ordinary displayable char
                              Gotoxy(The_Window.Left_Col + 1, The_Row);
                              Putch(Char,The_Window.Foreground, The_Window.Background);
                              Bios.Release;
                              exit;

                           when Ascii.Bs | Ascii.Cr | Ascii.Lf | Ascii.Esc =>
                              Gotoxy(The_Window.Left_Col, The_Row);
                              Putch(' ', The_Window.Foreground, The_Window.Background);
                              Bios.Release;
                              exit;

                           when others =>
                              Bios.Release;
                        end case;

                     else
                        delay 0.02;
                     end if;
                  end loop;
                  Text := Char;
                  The_Window.Last_Row_In_Use := The_Row - 1;  -- forget we used one
               end; -- block
            end Get_Char;
         or
            terminate;
         end select;
      end loop;

   exception
      when Constraint_Error =>
         Ada.Text_Io.Put_Line("Swindows Kbd died, constraint_error");
      when others =>
         Ada.Text_Io.Put_Line("Swindows Kbd died");
   end Kbd;

end Swindows;