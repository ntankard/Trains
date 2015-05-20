-- Package Simdefs2       Simulation Train/Track Specifications 2008

-- Contains constants and definitions for train and track

-- Original: Winston Fletcher, 9/10/99.
-- Revised 12/04/00 - 19/05/02 Rob Allen simconst
-- Revised 31/05/07 Rob Allen renamed SimConst2 for 2007 layout
-- Version 1.1: 31 May 2007 rka  rename wheel-reflector constant, reduce max speed
-- Version 2.0: 5 July 2007 rka  change data structures to support cascading turnouts
-- Version 2.1: 16 Aug 2007 move most of data into Simtrack2
--
-- Simdefs2
-- Version 2.2: 25 Jan 2008 initial version taking info out of Simconst2
-- version 2.3  13-May-08 move constant Max_Segment_Sensors here from Raildefs
--                        and delete some old comments
-- version 2.4  19-Jun-08 add constant Turnout_Tolerance (simrail v2.1.5)
-- version 2.4.1 9-Apr-2015 voltages 0.00-10.0
--
with Raildefs;  use Raildefs;

package Simdefs2 is

   -- the following are still for N-gauge
   Max_Train_Speed    : constant Float := 120.0; -- was 150 mm/s at max voltage
   Max_Coast_Distance : constant := 31.0;  -- mm (measure of friction)
   Drag_Deceleration  : constant := Max_Train_Speed**2 / (Max_Coast_Distance * 2.0);
   -- 380.0 mm/s/s  *** NB: wrong physics!
   -- Note: subsequent measurements of the hardware showed that coast distance is
   -- directly proportional to speed NOT to the square therefore the constants below
   -- and the constant deceleration code in simrail-step is WRONG.  Further the
   -- constants below require 55ms to stop at full speed on top of the sensor but
   -- the hardware needs 220ms to be reliable on or behind. (12/07/01)
   Max_Acceleration : constant := 2.5 * Drag_Deceleration; -- 950.0 mm/s/s

   Max_Cab_Voltage : constant Float := 10.0; -- Volts measured on rails
   --Zero_Cab_Voltage: constant Float := 0.0;  -- was 105.0; (unused vsn 1.2+)
   Volt_Zero_Speed : constant Float := 4.1; -- volts when speed zero
   ------------- all the above subject to change ----------------------------

   Reflector_Width : constant Float := 9.0; -- prev 6mm
   Turnout_Tolerance : constant Float := 25.0;   --(v2.4)

   --25-02-08: many constants & types moved to Raildefs

   type Point is
      record
         X,
         Y : Float := 0.0;
      end record;

   type Arc_Type is
      record
         Radius : Float := 0.0;
         Centre : Point;
         Start,
         Finish : Float := 0.0; --Angles
         Length : Float;
      end record;

   type Point_Pair is array (1..2) of Point;  -- first,last
   type Straight_Line_Type is record
      Points : Point_Pair;
      Length : Float;
      Normal : Point; -- outward unit normal
   end record;

   Max_Segment_Sensors : constant Integer := 6;  -- max of 6 sensors in a segment (diagonals)

   -- the following data structure hooks all the arcs and lines together into
   -- the layout.  No pointers because they would need init code.

   Max_Segments : constant := 108;

   type Seg_Index is range 0 .. Max_Segments;
   type Next_Array is array (Polarity_Type) of Seg_Index;  -- next,prev

   type Segment_Kind is
         (Aline,
          Anarc,
          Aturnout);

   type Segment (Kind : Segment_Kind := Aline) is
   record
      Id : Integer;
      Blok : Block_Id;
      case Kind is
         when Aline|Anarc=>
            Next : Next_Array;
            Tnid : Turnout_Idx := No_Turnout;   -- if non-zero indicates to what turnout this segment belongs
         when Aturnout=>
            Converging : Boolean;
            Seg_St,
            Seg_Tu     : Seg_Index;
      end case;
   end record;

   type Train_Position is
      record
         Segno : Seg_Index;  -- index into Segments array.
         Mm    : Float;      -- no of mm into block (0 .. segment length)
           -- in defined Fwd direction
         To_Front  : Polarity_Type;
      end record;

   Random_Chance : constant := 0.025; --  chance of capacitor failing

end Simdefs2;
