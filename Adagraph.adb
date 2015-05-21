-----------------------------------------------------------------------
--
--  File:        adagraph.adb
--  Description: basic Win32 graphics
--  Rev:         0.5c
--  Date:        23-jan-1999
--  Author:      Jerry van Dijk
--  Mail:        jdijk@acm.org
--
--  Copyright (c) Jerry van Dijk, 1997, 1998, 1999
--  Billie Hollidaystraat 28
--  2324 LK Leiden
--  THE NETHERLANDS
--  tel int +31 (0)71 531 4365
--
--  Permission granted to use for any purpose, provided this copyright
--  remains attached and unmodified.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--
-----------------------------------------------------------------------
-- modified 2005-03-08 Rob Allen, Swinburne: protected object for mutex
-----------------------------------------------------------------------
with Interfaces.C;

package body Adagraph is

   pragma Linker_Options ("-ladagraph");

-----------------------------------------------------------------------
-- declare protected object for mutex  -- added 2005-03-08
-----------------------------------------------------------------------
protected Lock is
   procedure Create_Graph_Window (X_Max,  Y_Max  : out Integer;
      X_Char, Y_Char : out Integer);
   procedure Ext_Create_Graph_Window (X_Max,  Y_Max  : out Integer;
      X_Char, Y_Char : out Integer);
   procedure Create_Sized_Graph_Window (X_Size, Y_Size : in     Integer;
      X_Max,  Y_Max  :    out Integer;
      X_Char, Y_Char :    out Integer);
   procedure Destroy_Graph_Window;
   procedure Is_Open (Result : out Boolean);
   procedure Set_Window_Title (Title : in String);
   procedure Clear_Window (Hue : in Color_Type := Black);
   procedure Get_Pixel (X, Y : in Integer; Result : out Color_Type);
   procedure Put_Pixel (X, Y : in Integer; Hue : in Color_Type := White);
   procedure Draw_Line (X1, Y1, X2, Y2 : in Integer;
      Hue            : in Color_Type := White);
   procedure Draw_Box (X1, Y1, X2, Y2 : in Integer;
      Hue            : in Color_Type := White;
      Filled         : in Fill_Type  := No_Fill);
   procedure Draw_Circle (X, Y, Radius : in Integer;
      Hue          : in Color_Type := White;
      Filled       : in Fill_Type  := No_Fill);
   procedure Draw_Ellipse (X1, Y1, X2, Y2 : in Integer;
      Hue            : in Color_Type := White;
      Filled         : in Fill_Type  := No_Fill);
   procedure Flood_Fill (X, Y : in Integer; Hue : in Color_Type := White);
   procedure Display_Text (X, Y : in Integer;
      Text : in String;
      Hue  : in Color_Type := White);
   procedure Where_X (Result  : out Integer);
   procedure Where_Y (Result  : out Integer);
   procedure Goto_Xy (X, Y : in Integer);
   procedure Draw_To (X, Y : in Integer; Hue : in Color_Type := White);
end Lock;

-----------------------------------------------------------------------
-- call throughs:   -- added 2005-03-08
-----------------------------------------------------------------------
   ------------------
   -- Clear_Window --
   ------------------

   procedure Clear_Window (Hue : in Color_Type := Black) is
   begin
      Lock.Clear_Window (Hue);
   end Clear_Window;

   -------------------------
   -- Create_Graph_Window --
   -------------------------

   procedure Create_Graph_Window
     (X_Max,  Y_Max  : out Integer;
      X_Char, Y_Char : out Integer)
   is
   begin
      Lock.Create_Graph_Window
     (X_Max, Y_Max, X_Char, Y_Char);
   end Create_Graph_Window;

   -------------------------------
   -- Create_Sized_Graph_Window --
   -------------------------------

   procedure Create_Sized_Graph_Window
     (X_Size, Y_Size : in     Integer;
      X_Max,  Y_Max  :    out Integer;
      X_Char, Y_Char :    out Integer)
   is
   begin
      Lock.Create_Sized_Graph_Window
     (X_Size, Y_Size, X_Max, Y_Max, X_Char, Y_Char);
   end Create_Sized_Graph_Window;

   --------------------------
   -- Destroy_Graph_Window --
   --------------------------

   procedure Destroy_Graph_Window is
   begin
      Lock.Destroy_Graph_Window;
   end Destroy_Graph_Window;

   ------------------
   -- Display_Text --
   ------------------

   procedure Display_Text
     (X, Y : in Integer;
      Text : in String;
      Hue  : in Color_Type := White)
   is
   begin
      Lock.Display_Text
     (X, Y, Text, Hue);
   end Display_Text;

   --------------
   -- Draw_Box --
   --------------

   procedure Draw_Box
     (X1, Y1, X2, Y2 : in Integer;
      Hue            : in Color_Type := White;
      Filled         : in Fill_Type  := No_Fill)
   is
   begin
      Lock.Draw_Box
     (X1, Y1, X2, Y2, Hue, Filled);
   end Draw_Box;

   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle
     (X, Y, Radius : in Integer;
      Hue          : in Color_Type := White;
      Filled       : in Fill_Type  := No_Fill)
   is
   begin
      Lock.Draw_Circle
     (X, Y, Radius, Hue, Filled);
   end Draw_Circle;

   ------------------
   -- Draw_Ellipse --
   ------------------

   procedure Draw_Ellipse
     (X1, Y1, X2, Y2 : in Integer;
      Hue            : in Color_Type := White;
      Filled         : in Fill_Type  := No_Fill)
   is
   begin
      Lock.Draw_Ellipse
     (X1, Y1, X2, Y2, Hue, Filled);
   end Draw_Ellipse;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (X1, Y1, X2, Y2 : in Integer;
      Hue            : in Color_Type := White)
   is
   begin
      Lock.Draw_Line
     (X1, Y1, X2, Y2, Hue);
   end Draw_Line;

   -------------
   -- Draw_To --
   -------------

   procedure Draw_To (X, Y : in Integer; Hue : in Color_Type := White) is
   begin
      Lock.Draw_To (X, Y, Hue);
   end Draw_To;

   -----------------------------
   -- Ext_Create_Graph_Window --
   -----------------------------

   procedure Ext_Create_Graph_Window
     (X_Max,  Y_Max  : out Integer;
      X_Char, Y_Char : out Integer)
   is
   begin
      Lock.Ext_Create_Graph_Window
     (X_Max, Y_Max, X_Char, Y_Char);
   end Ext_Create_Graph_Window;

   ----------------
   -- Flood_Fill --
   ----------------

   procedure Flood_Fill (X, Y : in Integer; Hue : in Color_Type := White) is
   begin
      Lock.Flood_Fill (X, Y, Hue);
   end Flood_Fill;

   ---------------
   -- Get_Pixel --
   ---------------

   function Get_Pixel (X, Y : in Integer) return Color_Type is
      C : Color_Type;
   begin
      Lock.Get_Pixel (X, Y, C);
      return C;
   end Get_Pixel;

   -------------
   -- Goto_Xy --
   -------------

   procedure Goto_Xy (X, Y : in Integer) is
   begin
      Lock.Goto_Xy (X, Y);
   end Goto_Xy;

   -------------
   -- Is_Open --
   -------------

   function Is_Open return Boolean is
      Op : Boolean;
   begin
      Lock.Is_Open(Op);
      return Op;
   end Is_Open;

   ---------------
   -- Put_Pixel --
   ---------------

   procedure Put_Pixel (X, Y : in Integer; Hue : in Color_Type := White) is
   begin
      Lock.Put_Pixel (X, Y, Hue);
   end Put_Pixel;

   ----------------------
   -- Set_Window_Title --
   ----------------------

   procedure Set_Window_Title (Title : in String) is
   begin
      Lock.Set_Window_Title (Title);
   end Set_Window_Title;

   -------------
   -- Where_X --
   -------------

   function Where_X return Integer is
      X : Integer;
   begin
      Lock.Where_X(X);
      return X;
   end Where_X;

   -------------
   -- Where_Y --
   -------------

   function Where_Y return Integer is
      Y : Integer;
   begin
      Lock.Where_Y(Y);
      return Y;
   end Where_Y;

  -- end added call-throughs 2005-03-08






   --------------------------------
   -- Make the C types available --
   --------------------------------

   package C renames Interfaces.C;

   ----------------------------
   -- DLL Internal constants --
   ----------------------------

   No_Errors        : constant Integer := 0;
   Default_Window   : constant Integer := 0;
   Mousenone        : constant Integer := 0;
   Mousemove        : constant Integer := 1;
   Maximized_Window : constant Integer := 1;
   Mouseleftup      : constant Integer := 2;
   Mouserightup     : constant Integer := 3;
   Mouseleftdown    : constant Integer := 4;
   Mouserightdown   : constant Integer := 5;

   ------------------------
   -- DLL Internal types --
   ------------------------

   type Mouseeventstruct is
   record
      Event : Integer;
      Xpos  : Integer;
      Ypos  : Integer;
   end record;
   pragma Convention (C, Mouseeventstruct);

   type Mouseeventstructaccess is access all Mouseeventstruct;
   pragma Convention (C, Mouseeventstructaccess);

   type Integer_Access is access all Integer;
   pragma Convention (C, Integer_Access);

   ------------------------------
   -- Import the DLL functions --
   ------------------------------

   function Mouseevent return Integer;
   pragma Import (C, Mouseevent, "MouseEvent");

   function Getmouse (Event : Mouseeventstructaccess) return Integer;
   pragma Import (C, Getmouse, "GetMouse");

   function Getkey return Integer;
   pragma Import (C, Getkey, "GetKey");

   function Keyhit return Integer;
   pragma Import (C, Keyhit, "KeyHit");

   function Isopen return Integer;
   pragma Import (C, Isopen, "IsOpen");

   function Getfontwidth return Integer;
   pragma Import (C, Getfontwidth, "GetFontWidth");

   function Getfontheight return Integer;
   pragma Import (C, Getfontheight, "GetFontHeight");

   function Getdllversion return Integer;
   pragma Import (C, Getdllversion, "GetDLLVersion");

   function Getwindowwidth return Integer;
   pragma Import (C, Getwindowwidth, "GetWindowWidth");

   function Getwindowheight return Integer;
   pragma Import (C, Getwindowheight, "GetWindowHeight");

   function Creategraphwindow (Size : Integer) return Integer;
   pragma Import (C, Creategraphwindow, "CreateGraphWindow");

   function Createsizedgraphwindow (X, Y : Integer) return Integer;
   pragma Import (C, Createsizedgraphwindow, "CreateSizedGraphWindow");

   function Destroygraphwindow return Integer;
   pragma Import (C, Destroygraphwindow, "DestroyGraphWindow");

   function Clearwindow (Hue : Integer) return Integer;
   pragma Import (C, Clearwindow, "ClearWindow");

   function Getcolorpixel (X, Y: Integer) return Integer;
   pragma Import (C, Getcolorpixel, "GetColorPixel");

   function Putpixel (X, Y, Hue : Integer) return Integer;
   pragma Import (C, Putpixel, "PutPixel");

   function Fillflood (X, Y, Hue : Integer) return Integer;
   pragma Import (C, Fillflood, "FillFlood");

   function Setwindowtitle (Title : C.Char_Array) return Integer;
   pragma Import (C, Setwindowtitle, "SetWindowTitle");

   function Drawline (X1, Y1, X2, Y2, Hue : Integer) return Integer;
   pragma Import (C, Drawline, "DrawLine");

   function Drawbox (X1, Y1, X2, Y2, Hue, Filled : Integer) return Integer;
   pragma Import (C, Drawbox, "DrawBox");

   function Drawcircle (X, Y, Radius, Hue, Filled : Integer) return Integer;
   pragma Import (C, Drawcircle, "DrawCircle");

   function Drawellipse (X1, Y1, X2, Y2, Hue, Filled : Integer) return Integer;
   pragma Import (C, Drawellipse, "DrawEllipse");

   function Displaytext (X, Y : Integer; Text : C.Char_Array; Hue : Integer) return Integer;
   pragma Import (C, Displaytext, "DisplayText");

   function Getmaxsize (X, Y : Integer_Access) return Integer;
   pragma Import (C, Getmaxsize, "GetMaxSize");

   function Wherex return Integer;
   pragma Import (C, Wherex, "WhereX");

   function Wherey return Integer;
   pragma Import (C, Wherey, "WhereY");

   function Gotoxy (X, Y : Integer) return Integer;
   pragma Import (C, Gotoxy, "GotoXY");

   function Drawto (X, Y, Hue : Integer) return Integer;
   pragma Import (C, Drawto, "DrawTo");

   ---------------------------------------------------
   -- Translate DLL error codes into Ada exceptions --
   ---------------------------------------------------

   procedure Make_Exception (Error : in Integer) is
   begin
      if Error < No_Errors then
         case Error is
            when  -1    => raise Window_Already_Open;
            when  -2    => raise Window_Already_Closed;
            when  -3    => raise Create_Event_Failed;
            when  -4    => raise Create_Thread_Failed;
            when  -5    => raise Window_Not_Open;
            when  -6    => raise Invalid_Color_Value;
            when  -7    => raise Invalid_Coordinate;
            when  -8    => raise Error_Copying_Title;
            when  -9    => raise Error_Copying_Cmdline;
            when -10    => raise Wait_Failed_Error;
            when -11    => raise Set_Title_Error;
            when -12    => raise Fill_Rect_Error;
            when -13    => raise Invalidate_Rect_Error;
            when -14    => raise Update_Window_Error;
            when -15    => raise Set_Pixel_Error;
            when -16    => raise Select_Pen_Error;
            when -17    => raise Move_To_Error;
            when -18    => raise Line_To_Error;
            when -19    => raise Select_Brush_Error;
            when -20    => raise Rectangle_Error;
            when -21    => raise Ellipse_Error;
            when -22    => raise Get_Pixel_Error;
            when -23    => raise Flood_Fill_Error;
            when -24    => raise Set_Textcolor_Error;
            when -25    => raise Text_Out_Error;
            when -26    => raise Invalid_Window_Size;
            when -27    => raise Get_Position_Error;
            when -28    => raise Close_Handle_Failed;
            when -29    => raise Thread_Status_Error;
            when others => raise Unknown_Adagraph_Error;
         end case;
      end if;
   end Make_Exception;

   ----------------------------
   -- Call the DLL functions --
   ----------------------------


   --------------------------------------------------------------------
   -- these few not protected:  -- moved 2005-03-08
   --------------------------------------------------------------------
   function Get_Dll_Version return Integer is
      Return_Value : Integer;
   begin
      Return_Value := Getdllversion;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      return Return_Value;
   end Get_Dll_Version;

   --------------------------------------------------------------------
   procedure Get_Max_Size (X_Size, Y_Size : out Integer) is
      X, Y         : aliased Integer;
      Return_Value : Integer;
      X_Access     : Integer_Access := X'Unchecked_Access;
      Y_Access     : Integer_Access := Y'Unchecked_Access;
   begin
      Return_Value := Getmaxsize (X_Access, Y_Access);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      X_Size := X;
      Y_Size := Y;
   end Get_Max_Size;

   function Key_Hit return Boolean is
      Return_Value : Integer;
   begin
      Return_Value := Keyhit;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      return Return_Value /= 0;
   end Key_Hit;

   --------------------------------------------------------------------
   function Get_Key return Character is
      Return_Value : Integer;
   begin
      Return_Value := Getkey;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      return Character'Val (Return_Value);
   end Get_Key;

   --------------------------------------------------------------------
   function Mouse_Event return Boolean is
      Return_Value : Integer;
   begin
      Return_Value := Mouseevent;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      return Return_Value /= 0;
   end Mouse_Event;

   --------------------------------------------------------------------
   function Get_Mouse return Mouse_Type is
      Return_Value : Integer;
      Result       : Mouse_Type;
      The_Mouse    : aliased Mouseeventstruct;
      Our_Mouse    : Mouseeventstructaccess := The_Mouse'Unchecked_Access;
   begin
      Return_Value := Getmouse (Our_Mouse);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      if The_Mouse.Event = Mousenone then
         Result.X_Pos := 0;
         Result.Y_Pos := 0;
      else
         Result.X_Pos := The_Mouse.Xpos;
         Result.Y_Pos := The_Mouse.Ypos;
      end if;
      case The_Mouse.Event is
         when Mousenone      => Result.Event := None;
         when Mousemove      => Result.Event := Moved;
         when Mouseleftup    => Result.Event := Left_Up;
         when Mouserightup   => Result.Event := Right_Up;
         when Mouseleftdown  => Result.Event := Left_Down;
         when Mouserightdown => Result.Event := Right_Down;
         when others         => raise Mouse_Event_Error;
      end case;
      return Result;
   end Get_Mouse;


-----------------------------------------------------------------------
-- Implement Lock (previously package level subprogs, now all protected procs
-----------------------------------------------------------------------

protected body Lock is  -- added 2005-03-08

   --------------------------------------------------------------------
   procedure Is_Open (Result : out Boolean) is  -- was function
--   function Is_Open return Boolean is  -- was function
--      Result : Boolean;
   begin
      case Isopen is
         when 0      => Result := False;
         when 1      => Result := True;
         when others => raise Unknown_Adagraph_Error;
      end case;
--      return Result;
   end Is_Open;

   --------------------------------------------------------------------
   procedure Create_Graph_Window (X_Max,  Y_Max  : out Integer;
         X_Char, Y_Char : out Integer) is
      Return_Value : Integer;
   begin
      Return_Value := Creategraphwindow (Default_Window);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      X_Max := Integer (Getwindowwidth);
      Y_Max := Integer (Getwindowheight);
      if (X_Max = 0) or (Y_Max = 0) then
         raise Error_Reading_Size;
      end if;
      X_Char := Integer (Getfontwidth);
      Y_Char := Integer (Getfontheight);
      if (X_Char = 0) or (Y_Char = 0) then
         raise Error_Reading_Font;
      end if;
   end Create_Graph_Window;

   --------------------------------------------------------------------
   procedure Ext_Create_Graph_Window (X_Max,  Y_Max  : out Integer;
         X_Char, Y_Char : out Integer) is
      Return_Value : Integer;
   begin
      Return_Value := Creategraphwindow(Maximized_Window);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      X_Max := Integer (Getwindowwidth);
      Y_Max := Integer (Getwindowheight);
      if X_Max = 0 or Y_Max = 0 then
         raise Error_Reading_Size;
      end if;
      X_Char := Integer (Getfontwidth);
      Y_Char := Integer (Getfontheight);
      if X_Char = 0 or Y_Char = 0 then
         raise Error_Reading_Font;
      end if;
   end Ext_Create_Graph_Window;

   --------------------------------------------------------------------
   procedure Create_Sized_Graph_Window (X_Size, Y_Size : in     Integer;
         X_Max,  Y_Max  :    out Integer;
         X_Char, Y_Char :    out Integer) is
      Return_Value : Integer;
   begin
      Return_Value := Createsizedgraphwindow (X_Size, Y_Size);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      X_Max := Integer (Getwindowwidth);
      Y_Max := Integer (Getwindowheight);
      if X_Max = 0 or Y_Max = 0 then
         raise Error_Reading_Size;
      end if;
      X_Char := Integer (Getfontwidth);
      Y_Char := Integer (Getfontheight);
      if X_Char = 0 or Y_Char = 0 then
         raise Error_Reading_Font;
      end if;
   end Create_Sized_Graph_Window;

   --------------------------------------------------------------------
   procedure Destroy_Graph_Window is
      Return_Value : Integer;
   begin
      Return_Value := Destroygraphwindow;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Destroy_Graph_Window;

   --------------------------------------------------------------------
   procedure Clear_Window (Hue : in Color_Type := Black) is
      Return_Value : Integer;
   begin
      Return_Value := Clearwindow (Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Clear_Window;

   --------------------------------------------------------------------
   procedure Set_Window_Title(Title : in String) is
      Return_Value : Integer;
   begin
      Return_Value := Setwindowtitle (C.To_C (Title));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Set_Window_Title;

   --------------------------------------------------------------------
   procedure Get_Pixel (X, Y : Integer; Result : out Color_Type) is
      Return_Value : Integer;
   begin
      Return_Value := Getcolorpixel (X, Y);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      Result := Color_Type'Val (Return_Value);
   end Get_Pixel;

   --------------------------------------------------------------------
   procedure Put_Pixel (X, Y : in Integer; Hue : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := Putpixel (X, Y, Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Put_Pixel;

   --------------------------------------------------------------------
   procedure Draw_Line (X1, Y1, X2, Y2 : in Integer;
         Hue            : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := Drawline (X1, Y1, X2, Y2, Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_Line;

   --------------------------------------------------------------------
   procedure Draw_Box (X1, Y1, X2, Y2 : in Integer;
         Hue            : in Color_Type := White;
         Filled         : in Fill_Type  := No_Fill) is
      Return_Value : Integer;
   begin
      Return_Value := Drawbox (X1, Y1, X2, Y2, Color_Type'Pos (Hue), Fill_Type'Pos (Filled));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_Box;

   --------------------------------------------------------------------
   procedure Draw_Circle (X, Y, Radius : in Integer;
         Hue          : in Color_Type := White;
         Filled       : in Fill_Type  := No_Fill) is
      Return_Value : Integer;
   begin
      Return_Value := Drawcircle (X, Y, Radius, Color_Type'Pos (Hue), Fill_Type'Pos (Filled));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_Circle;

   --------------------------------------------------------------------
   procedure Draw_Ellipse (X1, Y1, X2, Y2 : in Integer;
         Hue            : in Color_Type := White;
         Filled         : in Fill_Type  := No_Fill) is
      Return_Value : Integer;
   begin
      Return_Value := Drawellipse (X1, Y1, X2, Y2, Color_Type'Pos (Hue), Fill_Type'Pos (Filled));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_Ellipse;

   --------------------------------------------------------------------
   procedure Flood_Fill (X, Y : in Integer; Hue : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := Fillflood (X, Y, Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Flood_Fill;

   --------------------------------------------------------------------
   procedure Display_Text (X, Y : in Integer;
         Text : in String;
         Hue  : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := Displaytext (X, Y, C.To_C (Text), Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Display_Text;

   --------------------------------------------------------------------
   procedure Where_X (Result : out Integer) is  -- was function
      Return_Value : Integer;
   begin
      Return_Value := Wherex;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      Result := Return_Value;
   end Where_X;

   --------------------------------------------------------------------
   procedure Where_Y (Result : out Integer) is  -- was function
      Return_Value : Integer;
   begin
      Return_Value := Wherey;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      Result := Return_Value;
   end Where_Y;

   --------------------------------------------------------------------
   procedure Goto_Xy (X, Y : in Integer) is
      Return_Value : Integer;
   begin
      Return_Value := Gotoxy (X, Y);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Goto_Xy;

   --------------------------------------------------------------------
   procedure Draw_To (X, Y : in Integer; Hue : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := Drawto (X, Y, Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_To;

end Lock;  -- added 2005-03-08

   -------------------------------------------------------
   -- Check that the right version of the DLL is loaded --
   -------------------------------------------------------
begin
   if Get_Dll_Version < Adagraph_Dll_Version then
      raise Dll_Version_Error;
   end if;
end Adagraph;