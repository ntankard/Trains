-----------------------------------------------------------------------
-- File:        Simtrack2.adb
-- Description: Displays Trains on track, etc for simrail2
-- Original:    16-Sep-99
-- Modified:    14 April 2000 rka moved display aspects from simtr_s
-- Modified:    17 April 2000 rka integrate with Swindows
-- Modified:     7 May 2000 rka Change_Cab param changed
-- Modified:    22 April 2001 rka v 1.4 speed up, rename (was Track.ads)
-- Modified:    11 March 2002 rka v 1.6 8-block config, 24-line swindows
-- Modified:    19 May 2002 rka v 1.6.5 train 2 red
-- Version 1.9:  13-Feb-05 rka  for simrail 1.9 whistle, test-bit
-- Version 1.95:  18-May-07 rka  for swindows below graphics
-- version 2.0  1-Jun-07 renamed Simtrack2, conversion started
-- version 2.1  16-Aug-07 renamed Simtrack2.Display, parent introduced to
--   replace most of Simconst2
-- version 2.2  25-Feb-08 Simconst2 split into raildefs and Sim2defs
-- version 2.3   8-Mar-08 use Sensor_Bit instead of Boolean
-- version 2.5  15-May-08 Train_Color array
-- version 2.5.1 14-Aug-08 changed colors
-- version 2.6.3  9-Apr-15 show individual Horn and Bell bits in table
--
-- Authors:     Rob Allen, Grant Sheppard (1999)
-----------------------------------------------------------------------
with Raildefs;  use Raildefs;
with Simdefs2; use Simdefs2;
with Adagraph; use Adagraph;  -- colours

package Simtrack2.Display is
   subtype String2 is String(1..2);

   procedure Init_Display;
   procedure Kill_Display;

   procedure Show_Cab(
      Blockno : in Block_Id;
      Signed_Cab : in String2);
   procedure Show_Dac(
      Dacno : in Train_Id;
      Voltage : in Float);
   procedure Change_Turnout_Status(
      Turnoutno : in Turnout_Id;
      Position : in Turnout_Pos);
   procedure Init_Graphics;
   procedure Draw_Sensor(
      Sensorno : in Sensor_Id;
      Status : in Sensor_Bit);
   procedure New_Track;
   procedure Draw_Train(
      Has_Carriages : in Boolean;
      Back : in Train_Position;
      Midl : in Train_Position;
      Front: in Train_Position;
      Col : in Adagraph.Color_Type );
   procedure Hazard(On : Boolean := True; Message : String := "CRASHED!!");
   procedure Show_Horns_Bells(Hb4 : in String);

   ----------------------
   -- Global constants --
   ----------------------
   Yshift : constant Integer := 0;  -- (was 313 pre vsn 1.95, 326 pre 1.6, 200 if reduced for SVGA)
      -- amount to shift the simulation subwindow along the vertical axis
      -- = height for Swindows part of the window if that is at top

   X_Size : constant Integer := 932;  -- Horizontal window size (641 for Swindows) 932 newtrain

   Y_Size : constant Integer := 600;  -- (550 for svga was 606) total vertical window size

   Train_Color : constant array (Train_Id) of Color_Type :=
      (Light_Cyan, Light_Green, Yellow, Yellow);  --Light_Magenta);
   Carriage_Color : constant Color_Type := White;
   Bg_Color   : constant Color_Type := Light_Gray;
   Fg_Color   : constant Color_Type := Black;
   Track_Color : constant Color_Type := Blue;
   Danger_Color : constant Color_Type := Light_Red;

   -- For reference here are the AdaGraph colours:
   --Black          Dark_Gray
   --Blue           Light_Blue
   --Green          Light_Green
   --Cyan           Light_Cyan
   --Red            Light_Red
   --Magenta        Light_Magenta
   --Brown          Yellow
   --Light_Gray     White
   ----------------------
   -- Global variables --
   ----------------------
   X_Max, Y_Max  : Integer;  -- Maximum screen coordinates
   X_Char : Integer := 0;  -- will be 8 - Character size
   Y_Char : Integer := 12;  -- Character size, set by Adagraph

end Simtrack2.Display;
