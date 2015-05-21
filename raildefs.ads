-- Package Raildefs  Swinburne Railway global types & constants 2015

-- Contains main constants and definitions for train and track for use in simrail2
-- as well as student-written control software (replacing Rail_Types pre-2008)

-- Original simconst: Winston Fletcher, 9/10/99.
-- Revised 12/04/00 - 19/05/02 Rob Allen derived simconst
-- Revised 31/05/07 Rob Allen renamed SimConst2 for 2007 layout
-- Version 1.1: 31 May 2007 rka  rename wheel-reflector constant, reduce max speed
-- Version 2.0: 5 July 2007 rka  change data structures to support cascading turnouts
-- Version 2.1: 16 Aug 2007 move most of data into Simtrack2
-- Version 2.2: 25 Jan 2008 initial version taking most of info out of Simconst2
-- version 2.2.1 25-Feb-08 renamed Raildefs for use by everyone
-- version 2.3    8-Mar-08 use Sensor_Bit instead of Boolean
-- version 2.4   13-May-08 invert Sensor_Bit for 2008 hardware
-- version 2.4.1 11-May-11 invert Sensor_On, _Off for 2008 hardware -- probably unused
-- version 2.5    9-Mar-12 remove Fwd, Rev from Polarity_Type declaration
-- version 3.0   25-Feb-15 support PWM cabs, ordinary proc for int handling
--
with Unsigned_Types;

package Raildefs is

   Max_Trains : constant := 4;
   type Train_Idx is range 0 .. Max_Trains;
   subtype Train_Id is Train_Idx range 1..Max_Trains;
   No_Train: constant Train_Idx := 0;

   type Cab_Type is mod 8;  -- 'or' range 0..7;
   subtype Dac_Id is Cab_Type range 1..Max_Trains;
   subtype Pwn_Id is Cab_Type range Max_Trains+1..7;

   Num_Blocks : constant := 24;
   type Block_Idx is range 0..Num_Blocks;
   subtype Block_Id is Block_Idx range 1..Num_Blocks;
   No_Block : constant Block_Idx := 0;

   Num_Crossings : constant := 5;
   type Crossing_Idx is range 0..Num_Crossings;
   subtype Crossing_Id is Crossing_Idx range 1..Num_Crossings;
   No_Crossing : constant Crossing_Idx := 0;

   Num_Sensors : constant  := 64; -- 32 sensors per INT32 card (not all used)
   type Sensor_Idx is range 0 .. Num_Sensors;
   subtype Sensor_Id is Sensor_Idx range 1..Num_Sensors;
   No_Sensor : constant Sensor_Idx := 0;

   -- define sensor on off constants to avoid confusion (can change if hardware
   -- changes)
   type Sensor_Bit is (Off, On);  -- note order F=0=off, T=1=on (v2.4)
   Sensor_On  : constant Boolean := True;  --(v2.5.1) was False
   Sensor_Off : constant Boolean := False;   --(v2.5.1)

   type Sensor_State_Array is array (Sensor_Id) of Sensor_Bit;  -- for simtrack

   type Polarity_Type is
     (Normal_Pol,
      Reverse_Pol);

   Opposite : constant array (Polarity_Type) of Polarity_Type := (Reverse_Pol, Normal_Pol);

   Num_Turnouts : constant := 19; -- card allows max of 24 turnouts
   type Turnout_Idx is range 0 .. 24;
   subtype Turnout_Id is Turnout_Idx range 1..Num_Turnouts;
   No_Turnout : constant Turnout_Idx := 0;

   -- logical position of the turnouts:
   --
   type Turnout_Pos is
         (Straight,
          Turned,
          Middle);
   type Tn_State_Array is array (Turnout_Id) of Turnout_Pos;
   -- defines states of turnouts and hence shape of blocks containing turnouts

   --------------------------------------------------------------------------
   --             Declarations for Hall-effect interrupt handling
   --------------------------------------------------------------------------
   type Four_Registers is array (0..4) of Unsigned_Types.Unsigned_8;
     -- last element is timestamp for debugging (valid simrail only)

   type Proc4_Access is access procedure  -- v3: not protected
      (Offset : in Sensor_Idx; R : in Four_Registers);
   -- offset should be 0 or 32

end Raildefs;
