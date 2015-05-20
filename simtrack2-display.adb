-----------------------------------------------------------------------
--  File:        Simtrack2.display.adb
-- Description: Displays Trains on Track, etc for Railroad Simulation
-- Created:     16 September 1999
-- Modified:    19 April 2000 rka: sized and captions moved to suit SVGA
-- Version 1.0  27 April 2000
-- Version 1.1:  7 May 2000 rka Block info changed, getxy bug fixed
-- Version 1.3:  4 Apr 2001 rka CAB/Volt caption, version reflects simrail.
-- Version 1.4: 22 Apr 2001 rka CAB, Volt separated, rename (was Track.adb).
-- Version 1.6:  6 March 2002 rka  8-block config
-- Version 1.8:  1 May 2004 rka  version string for Sim 1.8
-- Version 1.9:  13-Feb-05 rka  for simrail 1.9 whistle, test-bit
-- version 2.0  1-Jun-07 renamed Simtrack2, conversion started
-- Version 2.01: 5 July 2007 rka  change data structures to support cascading turnouts
-- Version 2.02: 5 Feb 2008 rka  change debugging printout, elim L26
-- version 2.3:  8-Mar-08 use Sensor_Bit instead of Boolean
-- version 2.4: 13-Mar-08 fix bug so red rail on right (as reality)
-- version 2.5: 15-May-08 Train_Color array
-- version 2.6:  3-Jul-08 display sensor numbers
-- version 2.6.1:  9-Mar-12 for raildefs 2.5
-- version 2.6.2: 19-Mar-13 fixed unset out param in Draw_Segment
-- version 2.6.3  9-Apr-15 show individual Horn and Bell bits in table
--
-- Authors:     Rob Allen, Grant Sheppard (1999)
-----------------------------------------------------------------------
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Adagraph;    use Adagraph;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions;  -- v2.6.1
use  Ada.Exceptions;

package body Simtrack2.Display is
   Version_String : constant String := "Train Track Simulation 2.3";
   package Int_Io is new Ada.Text_Io.Integer_Io(Integer);
   package Fio is new Ada.Text_Io.Float_Io(Float);

   Tn_States : Raildefs.Tn_State_Array := (others => Raildefs.Straight);
      -- Tn_States used in graphics to know turnout positions when drawing blocks 1 and 3

   ------------------------------------------------------------------------
   --                  *** DISPLAY CONSTANTS ***                            -
   -- all measurements in pixels (derived from actual using Scaling factor) -
   ------------------------------------------------------------------------
   --  OLD!!!! Constants in package spec for 80x24 display above oval track:
   --   Yshift      : Integer := 326;  -- allows for Swindows
   --   amount to shift the simulation subwindow along the vertical axis
   --   X_Size : constant Integer := 641;    -- Horizontal window size
   --   Y_Size : constant Integer := 550; -- YShift + 224;  -- Vertical window size <560 for SVGA
   --
   --  Variables in package spec:
   --   X_Max,  Y_Max  : Integer;  -- Maximum screen coordinates
   --   X_Char, Y_Char : Integer;  -- Character size normally 8, 12

   -- Change X_Zero, X_Zero and Y_Size to re-position and re-scale track on screen

   X_Zero : constant Integer := 5;    -- top left in pixels
   Y_Zero : constant Integer :=  Yshift;    -- top left in pixels within layout area
   --(was 5 pre 1.6)
   -- v2.0 Y_Zero includes Yshift
   Swindows_Height : constant Integer := 289;  -- 24*Y_Char+1

   --Fyshift : Float := Float(Yshift);

   Scaled_Width   : constant Integer := X_Size - X_Zero - 4;  -- was based on height
   -- pixels, even (212 was 220)
   Scaling_Factor : constant Float := Float(Scaled_Width)/Overall_Width;  -- scales mm -> pixels
   -- approx 0.3

   --Scaled_Height: constant Integer := Integer(Overall_Height* Scaling_Factor);

   --Scaled_R2  : constant Integer := Integer(R2 * Scaling_Factor);

   Scaled_Xcw : constant Integer := Integer(Xcw * Scaling_Factor);
   Scaled_Xcrossw : constant Integer := Integer(Xc_Crossw * Scaling_Factor);
   Scaled_Xcent : constant Integer := Integer(Xcent * Scaling_Factor);
   Scaled_Xcrosse : constant Integer := Integer(Xc_Crosse * Scaling_Factor);
   Scaled_Xcm : constant Integer := Integer(Xcm * Scaling_Factor);
   Scaled_Xcbypass : constant Integer := Integer(Xc_Bypass_Edn * Scaling_Factor);
   Scaled_Xcyardw : constant Integer := Integer(Xc_Yardw * Scaling_Factor);
   Scaled_Xcyards : constant Integer := Integer(Xc_Yards*Scaling_Factor);
   Scaled_Xtramcross : constant Integer := Integer(Xtram_Cross * Scaling_Factor);
   Scaled_Xce : constant Integer := Integer(Xce * Scaling_Factor);

   Scaled_Yntop : constant Integer := Integer((Ycent - R2)*Scaling_Factor);
   Scaled_Ynlower : constant Integer := Integer((Ycent - R1)*Scaling_Factor);
   Scaled_Ycent : constant Integer := Integer(Ycent * Scaling_Factor);
   Scaled_Ysupper : constant Integer := Integer((Ycent + R1)*Scaling_Factor);
   Scaled_Yslower : constant Integer := Integer((Ycent + R2)*Scaling_Factor);
   Scaled_Ybypass : constant Integer := Integer((Ycent + R3)*Scaling_Factor);
   Scaled_Ycyards : constant Integer := Integer(Yc_Yards*Scaling_Factor);

   -- More screen layout constants:  --
   --                                --
   -- Where to place the block id & CAB/pol info (w/o Yshift)
   --
   -- (Note: we assume Y_Char has been initialised to 13, the value it will
   -- get when Create_Sized_Graph_Window is called.  We do not use X_Char
   -- here.)
   --
   -- Note: the reference point for Display_Text is top left and text is X_Char pixels
   -- wide and Y_Char pixels high.

   Cab_X : constant array (Block_Id) of Integer :=
      (8 => 20,
      15|16 => Scaled_Xcw/2 + 8,  -- note cant use X_Char yet
      7 => Scaled_Xcw - 44,
      9 => Scaled_Xcw + 12,
      24 => Scaled_Xcw + 72,
      14|17 => (Scaled_Xcrossw + Scaled_Xcent)/2,
      13|18|10 => Scaled_Xcent + 32,
      6|11 => (Scaled_Xcm + Scaled_Xcyardw)/2 - 16,
      21 => Scaled_Xcyards - 16,
      12|19 => Scaled_Xtramcross - 96,
      1 => Scaled_Xtramcross - 72,
      20 => Scaled_Xce + 32,
      5 => Scaled_Width - 80,
      2 => Scaled_Xtramcross + 96,
      3 => Scaled_Xtramcross + 62,
      4 => Scaled_Xtramcross + 32,
      22 => (Scaled_Xcw + Scaled_Xcent)/2 - 8,
      23 => (Scaled_Xcent + Scaled_Xcm)/2 - 24
      );

   Cab_Y : constant array (Block_Id) of Integer :=
      ( 6|7 => 4,
      5 => Scaled_Yntop + Y_Char - 2,
      12|13|14 => Scaled_Ynlower + Y_Char + 3,
      15 => Scaled_Ynlower + 5*Y_Char,
      1 => Integer((Yc_Tram - R1) * Scaling_Factor) + 3*Y_Char,
      22|23 => (Scaled_Ycent + Scaled_Ysupper)/2 - Y_Char,
      16 => Scaled_Ysupper - 5*Y_Char,
      17|18|19 => Scaled_Ysupper - 2*Y_Char - 2,
      9|10|20 => Scaled_Yslower + Y_Char/3,
      11 => Scaled_Yslower + Y_Char,
      8 => Scaled_Yslower - 2*Y_Char,
      24 => Scaled_Ybypass + 4,
      21 => Scaled_Ycyards + 9*Y_Char + 5,
      4 => Integer((Yc_Tn4 + Siding_Length*0.71)* Scaling_Factor) + Y_Char,
      3 => Integer((Yc_Tn3 + Siding_Length*0.71)* Scaling_Factor) + Y_Char,
      2 => Integer((Yc_Tn2 + Siding_Length*0.71)* Scaling_Factor) );

   -- where to place the turnout status info (w/o Yshift)
   --
   Tn_X : constant array (Turnout_Id) of Integer :=
      (8 => Scaled_Xcw/2, 15|16 => Scaled_Xcw + 8,  -- note cant use X_Char yet
      7|9|14|17 => Scaled_Xcrossw + 24,
      10 => Scaled_Xcrosse - 56,
      6|13|18 => Scaled_Xcrosse - 40,
      12|19 => Scaled_Xcm - 32,
      11 => Scaled_Xcbypass - 32,
      1 => Scaled_Xcyardw + 40,
      2|3|4|5 => Integer((Xce + R2)* Scaling_Factor) + 12);


   Tn_Y : constant array (Turnout_Id) of Integer :=
      (8 => Scaled_Yntop, 6|7 => 6,
      12 => Scaled_Ynlower - Y_Char - 2,
      13|14|15 => Scaled_Ynlower + 5,
      17|18 => Scaled_Ysupper - Y_Char - 2,
      19 => Scaled_Ysupper - Y_Char - 6,
      16 => Scaled_Ysupper - 2*Y_Char + 5,
      1|11 => Scaled_Yslower - Y_Char - 2,
      9|10 => Scaled_Yslower + 5,
      5 => Scaled_Ycent + Y_Char,
      4 => Integer(Yc_Tn4 * Scaling_Factor) + Y_Char,
      3 => Integer(Yc_Tn3 * Scaling_Factor) + Y_Char,
      2 => Integer(Yc_Tn2 * Scaling_Factor) + Y_Char );

   Tn_Position_Chars : constant array (Turnout_Pos) of String(1..19) :=
      (Turned =>  "\/////\//\//\/\/\/\",
      Straight => "-||||--------------",
      Middle =>   "mmmmmmmmmmmmmmmmmmm");
   Tn_Pos_String : constant array (Turnout_Pos) of String(1..8) :=
      (Raildefs.Straight => "Straight", Raildefs.Turned => "Turned  ",
       Raildefs.Middle => "Middle  ");
   Block_Label_Color : constant Color_Type := Light_Red;
   Turnout_Label_Color : constant Color_Type := Fg_Color;
   Sensor_Label_Color : constant Color_Type := Yellow;
   Sensor_Off_Color : constant Color_Type := Red;
   Sensor_On_Color  : constant Color_Type := Yellow;
   Sensor_Size : constant Integer := 4;  -- radius in pixels

   Sensor_States : Sensor_State_Array;   -- copy of data hidden in Simrail

   -- pixel coords relative to Sensor_X(i),Sensor_Y(i)
   -- note: text drawn will be 2 chars with no leading space
   -- 10 pixels wide
   Sensor_Offset_X : constant array (Sensor_Id) of Integer :=
      ( 3|5|7|10|12|14|16|25..27|29|43 => 10,  -- to right
        50|56|58 => 5,
        1|2|4|6|8|19|21|23|48|55|60|61|63 => -20,  -- to left
        others=>0 );
   Sensor_Offset_Y : constant array (Sensor_Id) of Integer :=
      ( 15|17|28|30..34|36..39|44..46|48|49|51|53|60 => -(Y_Char+3),  -- above
        10|12|14|16 => -Y_Char,
        18|20|22|35|41|42|47 => 4,  -- below
        24 => 6,
        40 => 3,
        62|64 => 4,
        others=>0 );

   Sensor_X,
   Sensor_Y : array (Sensor_Id) of Integer;  -- pixel coords,
   --  calculated once by Init_Graphics

   --------------------------------
   -- Open graphic output window --
   --------------------------------
   procedure Init_Display is
   begin
      if X_Char > 0 then return; end if;  -- already done
      Put_Line("Init_Display ... xsize=" & X_Size'img & " ysize=" & Y_Size'img);
      Create_Sized_Graph_Window (X_Size, Y_Size, X_Max, Y_Max, X_Char, Y_Char);
      Put_Line("... xmax=" & X_max'img & " ymax=" & Y_max'img);
      Set_Window_Title (Version_String & "  DLL" & Get_Dll_Version'img);
      -- allows for swindows window....
   end Init_Display;

   ---------------------------------
   -- Close graphic output window --
   ---------------------------------
   procedure Kill_Display is
   begin
      --            Destroy_Graph_Window;
      --commented out to allow for swindows window...
      null;
   end Kill_Display;


   ----------------------------------------------------
   -- Displays DAC voltage at side as table          --
   ----------------------------------------------------
  procedure Show_Dac(Dacno : in Train_Id;
                     Voltage : in Float) is
      Temp : String(1..4);
      X : Integer :=  X_Size - 200 + 3*X_Char;  -- was 90
      Y : Integer := Y_Zero + 60 + 20*Integer(Dacno);
      W : Integer := 5*X_Char;  -- approx 45
   begin
      Temp := "    ";
      Fio.Put(Temp, abs(Voltage),1,0);
      Draw_Box(X, Y, X + W, Y + Y_Char, Bg_Color, Fill);
      Display_Text(X, Y, Temp, Fg_Color);
   end Show_Dac;

   ----------------------------------------------------
   -- Displays sound bits to right of DAC voltages   --
   ----------------------------------------------------
   procedure Show_Horns_Bells(Hb4 : in String) is
      -- Assumes Hb4 is 8 chars usually blank.  Example
      -- if train 3 has both H and B on "    HB  "
      J : Integer;
      X : Integer :=  X_Size - 200 + 8*X_Char;
      Y : Integer := Y_Zero + 60 + 20;
      W : Integer := 2*X_Char;  -- approx 18
   begin
      for T in Train_Id loop   -- 1,2,3,4
         J := Integer(T)*2 - 1;  -- 1,3,5,7
         Draw_Box(X, Y, X + W, Y + Y_Char, Bg_Color, Fill);
         Display_Text(X, Y, Hb4(J..J+1), Fg_Color);
         Y := Y + 20;
      end loop;
   end Show_Horns_Bells;


   procedure Getxy(  -- forward declaration
      Pos: in Train_Position;
      X: in out Integer;
      Y: in out Integer;
      Tn_States : in out Tn_State_Array);

   ------------------------------------------------------------------
   -- Init graphic output window, ie draw track, senesors and text --
   ------------------------------------------------------------------
   procedure Init_Graphics is
      Xpos, Ypos : Integer;
   begin
--      Put_Line("Init_Graphics ... scaled_height,radius,yc,cab_y(4)="
--      & scaled_height'img & scaled_radius'img & yc'img & cab_y(4)'img);
      Init_Display;  -- possibly already done

      Draw_Box(0, Yshift, X_Max, Y_Max, Bg_Color, Fill); --White, Fill);

      -- setup table of DAC voltage values (top right)
      Xpos := X_Size - 200; -- was 120
      Ypos := Y_Zero + 60;
      Display_Text(Xpos - 5, Ypos, "DAC  V", Fg_Color);
      for D in Train_Id loop
         -- line in table
         Display_Text(Xpos, Ypos + 20*Integer(D), D'img, Fg_Color);
      end loop;

      -- setup yellow borders and extra title (deleted for SVGA)
--      Draw_Box(3, 7+Yshift, X_Size-3, Y_Size-3,Yellow, No_Fill);
--      Draw_Box(30, 1+Yshift, 110, Y_Char+2+Yshift, Bg_Color, Fill);
      Display_Text(X_Size - 110, 0+Yshift, "SIMTRACK2", Fg_Color);

      -- draw the track and sensors and captions:
      -- New_Track;  -- 2007: call this separately
   end Init_Graphics;

   ---------------------------------------------------------------
   -- NAME:    Draw_Arc                                         --
   --                                                           --
   -- PURPOSE: Draw an arc of a single line circle              --
   --                                                           --
   -- INPUTS:  X_Pos  - Horizontal position of center in pixels --
   --          Y_Pos  - Vertical position of center in pixels   --
   --          Radius - Circle radius in pixels                 --
   --          Start  - starting angle in radian, zero in east  --
   --          Finish - finish angle in radian, +ve anticlockwise-
   --          Color  - line color                       --
   --                                                           --
   -- EXCEPTS: Not_In_VGA_Error     - if not in VGA mode        --
   --          Outside_Screen_Error - writes outside the screen --
   ---------------------------------------------------------------
   procedure Draw_Arc(
         X_Pos  : in Float;
         Y_Pos  : in Float;
         Radius : in Float;
         Start  : in Float;
         Finish : in Float;
         Color  : in Color_Type) is
   --
      X, Y        : Float;
      Y_Factor    : constant Float := Float(Radius);
      X_Factor    : constant Float := Float(Radius);
      Ang         : Float := Start;
      Finis       : Float := Finish;
      Step        : Float := 0.5 / Sqrt(Y_Factor);
      Nstep       : Integer := Integer(0.49 + abs((Finish - Start)/Step));
      Xp1, Xp2    : Integer;
      Yp1, Yp2    : Integer;
   begin
      --Put_Line("Draw_arc xc,yc,radius=" & x_pos'img & y_pos'img & radius'img);
      if Nstep = 0 then NStep := 1; end if;
      Step := (Finish - Start)/Float(Nstep);
      --Put_Line("Draw_arc radius,ang,nstep,step=" & radius'img & ang'img & nstep'img & step'img);

      X := Cos(Ang)*X_Factor;  -- +ve east
      Y := Sin(Ang)*Y_Factor;  -- +ve north
      Xp1 := Integer(X_Pos + X);  -- +ve east
      Yp1 := Integer(Y_Pos - Y);  -- +ve south (ie screen coords)
      loop
         Ang := Ang + Step;
         -- Put_Line("Draw_arc ang=" & ang'img);
         Xp2 := Integer(X_Pos + Cos(Ang)*X_Factor);
         Yp2 := Integer(Y_Pos - Sin(Ang)*Y_Factor);
         Draw_Line(Xp1, Yp1, Xp2, Yp2, Color);
         exit when abs((Ang - Finis)/Step) < 0.5;
         Xp1 := Xp2;
         Yp1 := Yp2;
      end loop;
   end Draw_Arc;

   ---------------------------------------------------------------
   -- NAME:    Draw_Arc                                         --
   --                                                           --
   -- PURPOSE: Draw an arc of a single line circle coords in mm --
   --                                                           --
   -- INPUTS:  Centre - position of centre of arc in mm         --
   --          Radius - Circle radius in mm                     --
   --          Start  - starting angle in radian, zero in east  --
   --          Finish - finish angle in radian, +ve anticlockwise-
   --          Color  - line color                              --
   ---------------------------------------------------------------
   procedure Draw_Arc(
      Centre : in Simdefs2.Point;
      Radius : in Float; -- mm
      Start  : in Float;  -- radian
      Finish : in Float;  -- radian
      Color  : in Color_Type) is
   begin
      --Put_Line("Draw_arc xc,yc,radius=" & integer(Centre.x)'img & integer(Centre.y)'img & integer(radius)'img);
      Draw_Arc(Float(X_Zero) + Scaling_Factor*Centre.X, Float(Y_Zero) + Scaling_Factor*Centre.Y,
         Scaling_Factor*Radius, Start, Finish, Color);
   end Draw_Arc;

   procedure Print_Curve(
      Arc : in Arc_Type
      ) is
      use Fio;
   begin
      Put(" arc ");
      Put(Arc.Centre.X + Arc.Radius*cos(Arc.Start), 5, 1, 0);
      Put(Arc.Centre.Y - Arc.Radius*sin(Arc.Start), 5, 1, 0);
      Put(Arc.Centre.X + Arc.Radius*cos(Arc.Finish), 5, 1, 0);
      Put(Arc.Centre.Y - Arc.Radius*sin(Arc.Finish), 5, 1, 0);
      Put(Arc.Length, 6, 1, 0);
      Put(Arc.Centre.X, 5, 1, 0);
      Put(Arc.Centre.Y, 5, 1, 0);
      Put(Arc.Radius, 5, 1, 0);
      New_Line;
   end Print_Curve;

   Half_HO : constant Float := Rail_Spacing/2.0; -- 8 mm for HO gauge

   ---------------------------------------------------------------
   -- draws a curved segment of track as a double line separated by
   -- 2.0*Half_Sep mm in specified Colors.  It is single if Half_Sep
   -- is zero.
   ---------------------------------------------------------------
   procedure Draw_Curve(
      Arc : in Arc_Type;
      Left_Color : Color_Type := Track_Color;
      Right_Color : Color_Type := Track_Color;
      Half_Sep : in Float := Half_Ho
      ) is
      use Fio;
      Drad : Float := -Half_Sep;  -- delta radius to left, usually inner arc
   begin
--      Print_Curve(Arc);
--      Put_Line(" " & Left_Color'img & " " & Right_Color'img);
      if Arc.Start > Arc.Finish then  -- normal pol is clockwise
         Drad := -Drad;  -- left is outer
      end if;
      Draw_Arc(Float(X_Zero) + Scaling_Factor*Arc.Centre.X, Float(Y_Zero) + Scaling_Factor*Arc.Centre.Y,
         Scaling_Factor*(Arc.Radius + Drad), Arc.Start, Arc.Finish, Left_Color);
      if Half_Sep /= 0.0 then
         Draw_Arc(Float(X_Zero) + Scaling_Factor*Arc.Centre.X, Float(Y_Zero) + Scaling_Factor*Arc.Centre.Y,
            Scaling_Factor*(Arc.Radius - Drad), Arc.Start, Arc.Finish, Right_Color);
      end if;
   end Draw_Curve;

   ---------------------------------------------------------------
   -- draws part of a curved segment of track as a double line
   -- separated by 2.0*Half_Sep mm in specified Color.  It covers
   -- Mm1 to Mm2 (order unimportant) both in 0.0 .. Arc.Length
   -- If Half_Sep = zero draws a single line.  The default is a
   -- close double line suitable to show a train.
   ---------------------------------------------------------------
   procedure Draw_Part_Curve(
      Arc : in Arc_Type;
      Color  : in Color_Type;
      Mm1, Mm2 : in Float;
      Half_Sep : in Float := 2.0
      ) is
         use Fio;
      Angle1, Angle2 : Float;
   begin
      -- Print_Curve(Arc);
      Angle1 := Arc.Start + Mm1/Arc.Length*(Arc.Finish - Arc.Start);
      Angle2 := Arc.Start + Mm2/Arc.Length*(Arc.Finish - Arc.Start);

      Draw_Arc(Float(X_Zero) + Scaling_Factor*Arc.Centre.X, Float(Y_Zero) + Scaling_Factor*Arc.Centre.Y,
         Scaling_Factor*(Arc.Radius - Half_Sep), Angle1, Angle2, Color);
      if Half_Sep /= 0.0 then
         Draw_Arc(Float(X_Zero) + Scaling_Factor*Arc.Centre.X, Float(Y_Zero) + Scaling_Factor*Arc.Centre.Y,
            Scaling_Factor*(Arc.Radius + Half_Sep), Angle1, Angle2, Color);
      end if;
   end Draw_Part_Curve;

   procedure Print_Straight(
      Line : in Straight_Line_Type
      ) is
   --
      use Fio;
   begin
      Put(" line");
      Put(Line.Points(1).X, 5, 1, 0);
      Put(Line.Points(1).Y, 5, 1, 0);
      Put(Line.Points(2).X, 5, 1, 0);
      Put(Line.Points(2).Y, 5, 1, 0);
      Put(Line.Length, 6, 1, 0);
      New_Line;
   end Print_Straight;

   ---------------------------------------------------------------
   -- draws a straight segment of track as a double line separated by
   -- 2.0*Half_Sep mm in specified Color.  It is single if Half_Sep
   -- is zero.
   ---------------------------------------------------------------
   procedure Draw_Straight(
      Line : in Straight_Line_Type;
      Left_Color : Color_Type := Track_Color;
      Right_Color : Color_Type := Track_Color;
      Half_Sep : in Float := Half_HO
      ) is
   --
      use Fio;
      Dx, Dy : Float;
   begin
      -- Print_Straight(Line);
      -- Put_Line(" " & Left_Color'img & " " & Right_Color'img);

      Dx := Half_Sep*Line.Normal.X;  -- offset to right
      Dy := Half_Sep*Line.Normal.Y;
      Draw_Line(  -- left, usually inner
         X_Zero + Integer(Scaling_Factor*(Line.Points(1).X - Dx)),
         Y_Zero + Integer(Scaling_Factor*(Line.Points(1).Y - Dy)),
         X_Zero + Integer(Scaling_Factor*(Line.Points(2).X - Dx)),
         Y_Zero + Integer(Scaling_Factor*(Line.Points(2).Y - Dy)),
         Left_Color);
      if Half_Sep /= 0.0 then
         Draw_Line(  -- right, usually outer
            X_Zero + Integer(Scaling_Factor*(Line.Points(1).X + Dx)),
            Y_Zero + Integer(Scaling_Factor*(Line.Points(1).Y + Dy)),
            X_Zero + Integer(Scaling_Factor*(Line.Points(2).X + Dx)),
            Y_Zero + Integer(Scaling_Factor*(Line.Points(2).Y + Dy)),
            Right_Color);
      end if;
   end Draw_Straight;

   ---------------------------------------------------------------
   -- draws part of a straight segment of track as a double line
   -- separated by 2.0*Half_Sep mm in specified Color.  It covers
   -- Mm1 to Mm2 (order unimportant) both in 0.0 .. Arc.Length
   -- If Half_Sep = zero draws a single line.  The default is a
   -- close double line suitable to show a train.
   ---------------------------------------------------------------
   procedure Draw_Part_Straight(
      Line : in Straight_Line_Type;
      Color  : in Color_Type;
      Mm1, Mm2 : in Float;
      Half_Sep : in Float := 2.0
      ) is
   --
      use Fio;
      Dx, Dy : Float;
      X, Y, X1, Y1, X2, Y2, Len : Float;
   begin
      --      Put(" partial ");
      --      Print_Straight(Line);
      Dx := Half_Sep*Line.Normal.X;  -- offset outward
      Dy := Half_Sep*Line.Normal.Y;
      X1 := Line.Points(1).X;
      Y1 := Line.Points(1).Y;
      X2 := Line.Points(2).X;
      Y2 := Line.Points(2).Y;
      Len := Line.Length;
      X := X1 + Mm1/Len*(X2 - X1);
      Y := Y1 + Mm1/Len*(Y2 - Y1);
      X2 := X1 + Mm2/Len*(X2 - X1);
      Y2 := Y1 + Mm2/Len*(Y2 - Y1);

      Draw_Line(  -- inner
         X_Zero + Integer(Scaling_Factor*(X - Dx)),
         Y_Zero + Integer(Scaling_Factor*(Y - Dy)),
         X_Zero + Integer(Scaling_Factor*(X2 - Dx)),
         Y_Zero + Integer(Scaling_Factor*(Y2 - Dy)),
         Color);
      if Half_Sep /= 0.0 then
         Draw_Line(  -- outer
            X_Zero + Integer(Scaling_Factor*(X + Dx)),
            Y_Zero + Integer(Scaling_Factor*(Y + Dy)),
            X_Zero + Integer(Scaling_Factor*(X2 + Dx)),
            Y_Zero + Integer(Scaling_Factor*(Y2 + Dy)),
            Color);
      end if;
   end Draw_Part_Straight;

   ------------------------------------------------
   --  Procs to draw many things like blocks     --
   ------------------------------------------------

   procedure Draw_All_Segments(
      Half_Sep : in Float := Half_HO ) is
      use Fio;
      Color  : Color_Type := Track_Color;
   begin
      -- Put_Line("Draw_All_Segments...");

      -- turnouts:
      for I in 1..Num_Turnouts loop
         -- Put(i'img);
         Draw_Curve(Arcs(i), Color, Color);
      end loop;

      -- other arcs:
      for I in Num_Turnouts+1..Num_Arcs loop
         if not (i in 27..29) then
            -- Put(i'img);
            Draw_Curve(Arcs(I), Color, Color);
         end if;
      end loop;

      -- lines:
      for I in 1..Num_Lines loop
         if i /= 26 then
            Draw_Straight(Straight_Lines(I), Color, Color);
         end if;
      end loop;
   end Draw_All_Segments;

   procedure Print_All_Segments is
      use Fio;
   begin
      Put_Line("Print_All_Segments...");

      -- turnouts:
      for I in 1..Num_Turnouts loop
         Put(i'img);
         Print_Curve(Arcs(i));
      end loop;

      -- other arcs:
      for I in Num_Turnouts+1..Num_Arcs loop
         if not (i in 27..29) then
            Put(i'img);
            Print_Curve(Arcs(I));
         end if;
      end loop;

      -- lines:
      for I in 1..Num_Lines loop
         Put(i'img);
         Print_Straight(Straight_Lines(I));
      end loop;
   end Print_All_Segments;

   ------------------------------------------------
   -- Annotate all segments with their numbers each
   -- near the middle and marks the ends with short
   -- ticks.  Looks better if segments are single lines.
   -- (for debugging layout)                                      --
   ------------------------------------------------
   procedure Draw_Segment_Numbers is
      use Fio;
      Ang : Float;
      X, Y : Integer;
      Color  : Color_Type := Fg_Color;
      Dx, Dy, R : Float;

      procedure Draw_Radial_Tick(Arc : in Arc_Type; Ang : Float) is
         X, Y, X2, Y2 : Integer;
         Dx, Dy : Float;
      begin
         Dx := cos(Ang);  -- unit radius
         Dy := -sin(Ang);
         X := X_Zero + Integer(Scaling_Factor*(Arc.Centre.X + Arc.Radius*Dx));
         Y := Y_Zero + Integer(Scaling_Factor*(Arc.Centre.Y + Arc.Radius*Dy));
         X2 := X_Zero + Integer(Scaling_Factor*(Arc.Centre.X + (3.0*Half_HO + Arc.Radius)*Dx));
         Y2 := Y_Zero + Integer(Scaling_Factor*(Arc.Centre.Y + (3.0*Half_HO + Arc.Radius)*Dy));
         Draw_Line(X, Y, X2, Y2, Red);
      end Draw_Radial_Tick;

      procedure Draw_Tick(Line : in Straight_Line_Type) is
         X, Y, X2, Y2 : Integer;
         Dx, Dy : Float;
      begin
         Dx := 3.0*Half_HO*Line.Normal.X;  -- offset outward
         Dy := 3.0*Half_HO*Line.Normal.Y;
         X := X_Zero + Integer(Scaling_Factor*Line.Points(1).X);
         Y := Y_Zero + Integer(Scaling_Factor*Line.Points(1).Y);
         X2 := X_Zero + Integer(Scaling_Factor*(Line.Points(1).X + Dx));
         Y2 := Y_Zero + Integer(Scaling_Factor*(Line.Points(1).Y + Dy));
         Draw_Line(X, Y, X2, Y2, Fg_Color);
      end Draw_Tick;

   begin
      --      Put_Line("Draw_Segment_Numbers...arc yc,radius=" & yc'img & radius'img);

      -- arcs:
      for I in 1..Num_Arcs loop
         if not (i in 27..29) then
            --Put(i'img);
            -- draw ID at middle
            Ang := (Arcs(I).Start+Arcs(I).Finish)/2.0;
            R := Arcs(I).Radius;
            X := X_Zero - X_Char + Integer(Scaling_Factor*(Arcs(I).Centre.X + R*cos(Ang)));
            Y := Y_Zero + Integer(Scaling_Factor*(Arcs(I).Centre.Y - R*sin(Ang)));
            Display_Text(X, Y, I'Img, Color);
            -- draw small radial line
            Draw_Radial_Tick(Arcs(I), Arcs(I).Start);
         end if;
      end loop;

      -- lines:
      for I in 1..Num_Lines loop
         if I /= 26 then
            --Put(I'img);
            -- draw ID at middle
            Dx := Half_HO*Straight_Lines(I).Normal.X;  -- offset outward
            Dy := Half_HO*Straight_Lines(I).Normal.Y;
   --         Fio.Put(Dx, 3,1,0);
   --         Fio.Put(Dy, 3,1,0);
   --         New_Line;
            X := X_Zero - X_Char + Integer(Scaling_Factor*
              (Straight_Lines(I).Points(1).X + Straight_Lines(I).Points(2).X)/2.0 + Dx);
            Y := Y_Zero - Y_Char/2 + Integer(Scaling_Factor*
              (Straight_Lines(I).Points(1).Y + Straight_Lines(I).Points(2).Y)/2.0 + Dy);
            Display_Text(X, Y, I'Img, Color);
            Draw_Tick(Straight_Lines(I));
         end if;
      end loop;
   end Draw_Segment_Numbers;


   ------------------------------------------------
   -- draws all segments of block B as a double line in colors
   -- Left_Color and Right_Color on left and right moving forward.
   -- The algorithm is to traverse the segment graph from a starting
   -- point forward ie normal polarity.
   ------------------------------------------------
   procedure Draw_Block(
      B : in Block_Id;
      Left_Color : Color_Type := Track_Color;
      Right_Color : Color_Type := Track_Color;
      Half_Sep : in Float := Half_HO ) is
   --
--      Color2 : Color_Type := Black;
      Cur, Ndx : Seg_Index;

      procedure Draw_Segment(
         N : Seg_Index;
         Nxt : out Seg_Index;
         Level : Positive ) is
      -- recurses to draw both segments of a turnout and follows
      -- a Turned branch one segment further in case it is part of
      -- the same block B.  Level is used to suppress infinite recursion:
      -- branch-turnout-branch-turnout-....
         S : Segment := Segments(N);
         Tmp : Seg_Index;
      begin
--         Put("Draw_Segment..." & "(" & S.Id'Img & " B=" & S.Blok'Img & ")");
         if S.Blok /= B then
            Put ("(" & S.Id'Img & " B=" & S.Blok'Img & ")");
            Nxt := 0;
            return;
         end if;
         case S.Kind is
            when Aline =>
               -- if this straight-line segment is part of
               -- a (converging) turnout then recurse via
               -- the turnout segment
               if S.Tnid /= No_Turnout and Level = 1 then
                  Draw_Segment(T+Seg_Index(S.Tnid), Nxt, Level + 1);
               else
                  -- just draw it
                  --Put_Line("L" & S.Id'img);
                  Draw_Straight(Straight_Lines(S.Id),
                     Left_Color, Right_Color, Half_Sep);
                  Nxt := S.Next(Normal_Pol);
               end if;
            when Anarc =>
               -- similarly...
               if S.Tnid /= No_Turnout and Level = 1 then
                  Draw_Segment(T+Seg_Index(S.Tnid), Nxt, Level + 1);
               else
                  --Put_Line("A" & S.Id'img);
                  Draw_Curve(Arcs(S.Id), Left_Color, Right_Color, Half_Sep);
                  Nxt := S.Next(Normal_Pol);
               end if;
         when ATurnout => -- diverging(level=1) or converging(Level=2)
--            Put_Line("T" & S.Id'img);
            Draw_Segment(S.Seg_St, Tmp, Level + 1);
            Draw_Segment(S.Seg_Tu, Nxt, Level + 1);
            -- we assume that a block contains anything beyond a diverging
            -- turnouts is turnout then it is via a turned branch (eg
            -- beyond a curve
         end case;
      end Draw_Segment;

   begin
--      Put_Line("Draw_Block..." & B'img);
      case B is
--      when 1=> -- B1: L20,A21,L21
--         Draw_Straight(Straight_Lines(20), Color);
--         Draw_Curve(Arcs(21), Color2);
--         Draw_Straight(Straight_Lines(21), Color);
--      when 2|3|4=> -- B2/3/4: L22/3/4, A22/3/4
--         Draw_Straight(Straight_Lines(20+Integer(B)), Color);
--         Draw_Curve(Arcs(20+Integer(B)), Color2);
--      when 5=> -- B5: (L2;A2),(L3;A3),(L4;A4),L26,(L5;A5),A26,L27
--         Draw_Straight(Straight_Lines(2), Color);
--         Draw_Curve(Arcs(2), Color2);
--         Draw_Straight(Straight_Lines(3), Color);
--         Draw_Curve(Arcs(3), Color2);
--         Draw_Straight(Straight_Lines(4), Color);
--         Draw_Curve(Arcs(4), Color2);
--         Draw_Straight(Straight_Lines(26), Color);
--         Draw_Straight(Straight_Lines(5), Color);
--         Draw_Curve(Arcs(5), Color2);
--         Draw_Curve(Arcs(26), Color);
--         Draw_Straight(Straight_Lines(27), Color);
      when others=>
         Ndx := Block_Starts(B);
         loop
            Cur := Ndx;
            Draw_Segment(Cur, Ndx, 1);
            exit when Ndx = 0 or else Segments(Ndx).Blok /= B;
         end loop;
--         New_Line;
      end case;
   end Draw_Block;

   ----------------------------------------------------
   -- Displays CAB and polarity for a block in situ  --
   ----------------------------------------------------
  procedure Show_Cab(Blockno    : in Block_Id;
                     Signed_Cab : in String2  ) is
      X : Integer := Cab_X(Blockno) + 7*X_Char/2; -- leave room for "Bx "
      Y : Integer := Cab_Y(Blockno);
  begin
     --Put_Line("  Show_Cab" & Blockno'img & X'img & Y'img);
     Draw_Box(X, Y, X + 2*X_Char, Y + Y_Char - 1, Bg_Color, Fill);
     Display_Text(X, Y, Signed_Cab, Block_Label_Color);
     if Signed_Cab(1) = ' ' or Signed_Cab(2) = '0' then
        Draw_Block(Blockno); -- double, track color both sides
     elsif Signed_Cab(1) = '+' then
        Draw_Block(Blockno, Track_Color, Light_Red); -- double, red on right
     else
        Draw_Block(Blockno, Light_Red, Track_Color); -- double, red on left
     end if;
   end Show_Cab;

   ------------------------------------------------
   -- show Blockno (Bnn), CAB/polarity (initially 0+) near to each block
   ------------------------------------------------
   procedure Draw_All_Block_Captions is
      X, Y : Integer;
   begin
--      Put_Line("Draw_All_Block_Captions...");
      for Blockno in Block_Id loop
         X := Cab_X(Blockno); -- for "Bx "
         Y := Cab_Y(Blockno);
--         Put_Line("Draw_Box" & Blockno'img & X'img & Y'img);
         Draw_Box(X, Y, X + 3*X_Char, Y + Y_Char - 1, Bg_Color, Fill);
         if Blockno < 10 then
            Display_Text(X, Y, " B" & Character'Val(48 + Integer(Blockno)), Block_Label_Color);
         else
            Display_Text(X, Y, "B" & Character'Val(48 + Integer(Blockno / 10)) &
                Character'Val(48 + Integer(Blockno mod 10)), Block_Label_Color);
         end if;
         Show_Cab(Blockno, "+0");
      end loop;
   end Draw_All_Block_Captions;

   ------------------------------------------------
   -- show  Turnout no (Tnn), position char (initially
   --  for straight) near to each turnout
   ------------------------------------------------
   procedure Draw_All_Turnout_Captions is
      X, Y, Width : Integer;
      Capt : String(1..3) := "Tnn";
   begin
--      Put_Line("Draw_All_Turnout_Captions...");
      -- setup display of turnout numbers
      Width := 2*X_Char;
      for Tn in Turnout_Id loop
         -- draw ID at defined position
         X := Tn_X(Tn);
         Y := Tn_Y(Tn);
         --Put_Line(X'img & Y'img);
         --Draw_Box(X, Y, X+Width, Y + Y_Char, Bg_Color, Fill);
         if Integer(Tn) < 10 then
            Capt(2) := Character'Val(48+Integer(Tn));
            Display_Text(X, Y, Capt(1..2), Turnout_Label_Color);
         else
            Capt := Tn'Img;  Capt(1) := 'T';
            Display_Text(X, Y, Capt, Turnout_Label_Color);
         end if;
      end loop;

      -- setup position of turnouts and display this info
      for Turnoutno in Turnout_Id loop
         Change_Turnout_Status(Turnoutno, Straight);
      end loop;
   end Draw_All_Turnout_Captions;

   ----------------------------------------------------------------------
   -- Getxy maps Train_Position to screen position (x,y)
   -- (used for calculating the ends of a train and sensor positions)
   ----------------------------------------------------------------------
   procedure Getxy(
      Pos: in Train_Position;
      X: in out Integer;
      Y: in out Integer;
      Tn_States : in out Tn_State_Array) is
   --
      S : Segment := Segments(Pos.Segno);
      Angle : Float;
      X1, Y1, X2, Y2, Len, R : Float;
      --Tempmm : Float; -- used to fix bug: had blk6 direction reversed
      Pos2: Train_Position;
   begin
--      Put("Getxy...");
      case S.Kind is
      when Aline =>
--         Put("L" & S.Id'Img);
--         Fio.Put(Pos.Mm, 4, 1, 0);
         X1 := Straight_Lines(S.Id).Points(1).X;
         Y1 := Straight_Lines(S.Id).Points(1).Y;
         X2 := Straight_Lines(S.Id).Points(2).X;
         Y2 := Straight_Lines(S.Id).Points(2).Y;
         Len := Straight_Lines(S.Id).Length;
         if Pos.Mm < -1.0 or Pos.Mm > Len+1.0 then
            Put_Line(" error: off an end of " & S.Id'Img & Len'Img & " at " & Pos.Mm'Img);
            X := 20;  Y := X;
            return;
         end if;
         X := X_Zero + Integer(Scaling_Factor*(X1 + Pos.Mm/Len*(X2 - X1)));
         Y := Y_Zero + Integer(Scaling_Factor*(Y1 + Pos.Mm/Len*(Y2 - Y1)));

      when Anarc =>
--         Put("A" & S.Id'img);
--         Fio.Put(Pos.Mm, 4, 1, 0);
         Angle := Arcs(S.Id).Start + Pos.Mm/Arcs(S.Id).Length*(Arcs(S.Id).Finish - Arcs(S.Id).Start);
         Len := Arcs(S.Id).Length;
         if Pos.Mm < -1.0 or Pos.Mm > Len+1.0 then
            Put_Line(" error: off an end of " & S.Id'img & Len'Img & " at " & Pos.Mm'img);
            return;
         end if;
         R := Arcs(S.Id).Radius;
         X := X_Zero + Integer(Scaling_Factor*(Arcs(S.Id).Centre.X + R*Cos(Angle)));
         Y := Y_Zero + Integer(Scaling_Factor*(Arcs(S.Id).Centre.Y - R*Sin(Angle)));

      when ATurnout =>
--         Put("T" & S.Id'img & "...warning: should be A or L.  ");
--         Pos2 := Pos;
--         Fio.Put(Pos.Mm, 4, 1, 0);
--         New_Line;
         if Tn_States(Turnout_Id(S.Id)) = Turned then
            Pos2.Segno := S.Seg_Tu;
         else
            Pos2.Segno := S.Seg_St;
         end if;
         Getxy(Pos, X, Y, Tn_States);
         -- this may result in a position along the Straight branch
         -- though the actual position is Middle
      end case;
      if (X <= 0) or (X > X_Size) then
         X := 1;
      end if;
      if (Y <= 0) or (Y > Y_Size) then
         Y := 1;
      end if;
--      Int_Io.Put(X, 5);
--      Int_Io.Put(Y, 5);
--      New_Line;
   end Getxy ;

   -------------------------------------------------------
   -- Hazard  displays message, "CRASHED!!" by default  --
   -------------------------------------------------------

   procedure Hazard(On : Boolean := True; Message : String := "CRASHED!!") is
   begin
      if On then
         Draw_Box(X_Size - 110, 0+Yshift, X_Size - 110 + 9*X_Char - 1, 2 + Y_Char+Yshift, Danger_Color, Fill);
      else
         Draw_Box(X_Size - 110, 0+Yshift, X_Size - 110 + 9*X_Char - 1, 2 + Y_Char+Yshift, Bg_Color, Fill);
      end if;
      Display_Text(X_Size - 110, 1+Yshift, Message, Fg_Color);
      -- replaces "SIMULATOR"
   end Hazard;

   -----------------------
   -- Draw_Sensor       --
   -----------------------
   procedure Draw_Sensor(Sensorno : in Sensor_Id; Status : in Sensor_Bit) is
      Color : Color_Type;
   begin
      if Sensor_Segment_Numbers(Sensorno) /= Nc then
         if Status = On then
            Color := Sensor_On_Color;
         else
            Color := Sensor_Off_Color;
         end if;
         Sensor_States(Sensorno) := Status;
         Draw_Circle(Sensor_X(Sensorno), Sensor_Y(Sensorno), Sensor_Size, Color, Fill);
      else
         Put_Line(Sensorno'Img & " is an invalid or unused sensor");
      end if;
   end Draw_Sensor;

   -----------------------
   -- Draw_Sensor_Label       --
   -----------------------
   procedure Draw_Sensor_Label(Sno : in Sensor_Id) is
      Text : String(1..2) := "  ";
   begin
      if Sensor_Segment_Numbers(Sno) /= Nc then
         if Sno < 10 then
            Text(1) := Character'Val(48 + Integer(Sno));
         else
            Int_Io.Put(Text, Integer(Sno));
         end if;
         Display_Text(
            Sensor_X(Sno) + Sensor_Offset_X(Sno),
            Sensor_Y(Sno) + Sensor_Offset_Y(Sno),
            Text, Sensor_Label_Color);
      end if;
   end Draw_Sensor_Label;


   procedure Draw_All_Sensors is
   begin
      Put_Line("Draw sensors...");
      for N  in Sensor_Id loop
         -- Put("sens" & N'img);
         if Sensor_Segment_Numbers(N) /= nc then
            Draw_Sensor(N,Sensor_States(N));
            Draw_Sensor_Label(N);
         end if;
      end loop;
   end Draw_All_Sensors;

   -----------------------------------------------
   --  Draw_Track  draws all tracks and sensors --
   -----------------------------------------------
   procedure New_Track is
      Temppos : Train_Position;
   begin
--      Print_All_Segments;

      -- init sensors (display versions)
      for N in Sensor_Id loop
         Sensor_States(N) := Off;
         if Sensor_Segment_Numbers(N) /= Nc then
            Temppos.Segno := Sensor_Segment_Numbers(N);
            Temppos.Mm := Sensor_Segment_Mm(N);
            Put(N'img);
            Getxy(Temppos, Sensor_X(N), Sensor_Y(N), Tn_States);
         end if;
      end loop;

      Draw_All_Segments;

--      for B in 1..12 loop
--         Draw_Block(Block_Id(2*B-1), Light_Red);
--      end loop;

--      Draw_Segment_Numbers;

      Draw_All_Block_Captions;

      Draw_All_Turnout_Captions;

      Draw_All_Sensors;
   end New_Track;

   -------------------------------------------------------------------
   -- Change_Turnout_Status                                         --
   -- writes - or | (Straight), m (Middle) or / or \ (Turned)       --
   -- after the turnout caption near the turnout                    --
   -------------------------------------------------------------------
   procedure Change_Turnout_Status(Turnoutno : in Turnout_Id;
                                   Position : in Turnout_Pos) is
   --
   -- (was in table at bottom)
      Xpos, Ypos : Integer;
      Width : Integer := X_Char;
      One_Char : String(1..1);
--      S : Segment;
      Old_Position : Turnout_Pos;
--      Color : Color_Type;
   begin
      --Put_Line("Change_Turnout_Status ... " & Turnoutno'img & " to " & Position'img);
      Old_Position := Tn_States(Turnoutno);
      Tn_States(Turnoutno) := Position;
      Xpos := Tn_X(Turnoutno);
      Ypos := Tn_Y(Turnoutno);
      case Turnoutno is
         when 2|3|4|5 =>     -- on far right so no room beside
            Ypos := Ypos + Y_Char + 1;
         when 1|6|7|8|9 =>   -- one digit numbers
            Xpos := Xpos + 2*X_Char;
         when others =>      -- 2-digit numbers
            Xpos := Xpos + 3*X_Char;
      end case;
      One_Char(1) := Tn_Position_Chars(Position)(Integer(Turnoutno));
      Draw_Box(Xpos, Ypos, Xpos + Width, Ypos + Y_Char - 2, Bg_Color, Fill);
      Display_Text(Xpos, Ypos, One_Char, Turnout_Label_Color);

      -- experimental:
--      S := Segments(T + Seg_Index(Turnoutno));
--      Color := Track_Color;
--      if Position = Turned then
--         S := Segments(S.Seg_Tu);
--      elsif Position = Straight then
--         S := Segments(S.Seg_St);
--      else
--         -- Middle, so erase the previous segment
--         Color := Bg_Color;
--         if Old_Position = Turned then
--            S := Segments(S.Seg_Tu);
--         elsif Old_Position = Straight then
--            S := Segments(S.Seg_St);
--         else
--            return;  -- no change
--         end if;
--      end if;
--      if S.Kind = Anarc then
--         Draw_Curve(Arcs(S.Id), Color);
--      else
--         Draw_Straight(Straight_Lines(S.Id), Color);
--      end if;
   end Change_Turnout_Status;

   --------------------------------------
   --  Draw_Train                      --
   -- (2007: note order of parameters) --
   --------------------------------------
   procedure Draw_Train(
      Has_Carriages : in Boolean;
      Back : in Train_Position;
      Midl : in Train_Position;  -- ignored if not Has_Carriages
      Front: in Train_Position;
      Col : in Adagraph.Color_Type -- of Loco
      ) is
   --

      procedure Draw_Part_Train(
           Pos_Behind,
           Pos_In_Front : in Train_Position;
           Col : in Adagraph.Color_Type -- of this part
      ) is
         Pos : Train_Position;
         S : Segment;
         Next_Polr : Polarity_Type;
         Next_Seg : Seg_Index;
         Inverting : Boolean;
         End_Mm : Float;
         Count : Integer := 0;
      begin
         Pos := Pos_Behind;
         S := Segments(Pos.Segno);
         while Pos.Segno /= Pos_In_Front.Segno and Count < 10 loop
            Count := Count  + 1;  -- to stop runaways!
            --
            --Put("Pos=(" & Pos.Segno'Img & Integer(Pos.Mm)'img & " " & Pos.To_Front'img & ") ");
            if S.Kind = Aturnout then
               Put_Line("error: unexpected turnout segment " & Back.Segno'Img & Pos.Segno'Img);
               return;
            end if;
            Next_Seg := S.Next(Pos.To_Front);
            Inverting := False; -- normal case
            if S.Kind = Aline then
               --Put(" line" & S.Id'img);
               if Pos.To_Front = Normal_Pol then
                  End_Mm := Straight_Lines(S.Id).Length;
               else
                  End_Mm := 0.0;
               end if;
               --Put_Line(" Mm=" & Integer(Pos.Mm)'img & " End_Mm=" & Integer(End_Mm)'img);
               Draw_Part_Straight(Straight_Lines(S.Id), Col, Pos.Mm, End_Mm);
            elsif S.Kind = Anarc then
               --Put(" arc" & S.Id'img);
               if Pos.To_Front = Normal_Pol then
                  End_Mm := Arcs(S.Id).Length;
                  --
                  --   WARNING: MAGIC NUMBERS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                  -- first pair are segments that collide with fwd pol, second with rev pol
                  --
                  Inverting := Pos.Segno = A+16 or Pos.Segno = A+42;
               else
                  End_Mm := 0.0;
                  Inverting := Pos.Segno = A+19 or Pos.Segno = A+41;
               end if;
               --Put(" inverting=" & Inverting'img);
               --Put_Line(" Mm=" & Integer(Pos.Mm)'img & " End_Mm=" & Integer(End_Mm)'img);
               Draw_Part_Curve(Arcs(S.Id), Col, Pos.Mm, End_Mm);
            end if;
            --
            -- advance to next segment
            -- (1)check for inverting join
            Next_Polr := Pos.To_Front;
            if Inverting then Next_Polr := Opposite(Pos.To_Front); end if;

            -- (2) resolve if entering a turnout, firstly converging
            --Put(" next seg=" & Next_Seg'img);
            S := Segments(Next_Seg);
            if S.Kind = Aturnout then
               -- attribute Converging applies to Fwd polarity
               --Put(" aturnout " & S.Id'img);

               if S.Converging = (Next_Polr = Normal_Pol) then
                  Put_Line(" draw_train: converging: ERROR should have come in via branch");
                  return;
               else
                  --Put(" diverging ");
                  -- diverging onto one branch or other:
                  if Tn_States(Turnout_Id(S.Id)) = Turned then
                     Pos.Segno := S.Seg_Tu;
                  else
                     Pos.Segno := S.Seg_St;  -- assume not Middle
                  end if;
                  S := Segments(Pos.Segno);
               end if;
               --Put_Line(" new segno=" & Pos.Segno'img);
            else
               -- maybe or maybe not a turnout branch (dont need to know here)
               --New_Line;
               Pos.Segno := Next_Seg;
            end if;
            -- assert S.Kind /= Aturnout

            -- (3) set new polarity and establish mm based on it
            Pos.To_Front := Next_Polr;
            if Next_Polr = Normal_Pol then
               Pos.Mm := 0.0;
            else
               if S.Kind = Aline then
                  Pos.Mm := Straight_Lines(S.Id).Length;
               else
                  Pos.Mm := Arcs(S.Id).Length;
               end if;
            end if;
         end loop;
         --Put("Pos=(" & Pos.Segno'Img & Integer(Pos.Mm)'img & " " & Pos.To_Front'img & ") ");
         --Put_Line(" Mm=" & Integer(Pos.Mm)'img & " End_Mm=" & Integer(Pos_In_Front.Mm)'img);

         -- now in one segment, so draw it
         if S.Kind = Aline then
            Draw_Part_Straight(Straight_Lines(S.Id), Col, Pos.Mm, Pos_In_Front.Mm);
         elsif S.Kind = Anarc then
            Draw_Part_Curve(Arcs(S.Id), Col, Pos.Mm, Pos_In_Front.Mm);
         else
            Put_Line("error: train on turnout segment " & Pos_In_Front.Segno'Img);
         end if;
         Pos := Pos_In_Front;
         -- assert S corresponds to pos.
      exception
         when E : Constraint_Error=>
            Ada.Text_Io.Put("simtrack2:draw_part_train ");
            Ada.Text_Io.Put_Line(Exception_Information(E));
      end Draw_Part_Train;

   begin
--      Getxy(Front, X1, Y1, Tn_States);
      --      Getxy(Back, X3, Y3, Tn_States);
      if Has_Carriages then
         if Col = Bg_Color then
            Draw_Part_Train(Back, Midl, Bg_Color);
         else
            Draw_Part_Train(Back, Midl, Carriage_Color);
         end if;
         Draw_Part_Train(Midl, Front, Col);
      else
         -- just a loco
         Draw_Part_Train(Back, Front, Col);
      end if;

--   test suppressed 17/04/01:
--      Train_Len := Sqrt( Float(X2-X1)*Float(X2-X1)+Float(Y2-Y1)*Float(Y2-Y1) );
--      -- internal consistency check:
--      if (train_len > SCALING_FACTOR*(Train_Length )+15.0
--         or train_len < SCALING_FACTOR*(Train_Length )-15.0) then
--         Hazard;
--         Put("train too long!!...");
--         Put_Line("frontpos b=" & front.block_Num'img & " mm=" & Integer(front.mm)'img);
--         Put_Line("back pos b=" & back.block_Num'img & " mm=" & Integer(back.mm)'img);
--         Put_Line("tn_State=" & tn_states(0)'img &' '& tn_states(1)'img &' '&
--                  tn_states(2)'img &' '& tn_states(3)'img);
--      end if;
   exception
      when Constraint_Error =>
         null;
         Put_Line("simtrack2:draw_train constraint");
      when others=>
         null;
         Put_Line("simtrack2:draw_train other exception");
   end Draw_Train;

end Simtrack2.Display;
