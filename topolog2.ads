-- Topolog2.ads        -- PASSIVE
--
-- Passive object providing block interconnection info, turnout info and
-- code to track the position of each magnet as it moves along the layout.
--
-- Based on a graph model of the topology.  Sensors, turnouts and various
-- kinds of track section are all nodes.  Blocks are attributes of nodes.
--
-- This package is state-free: all calls pass a position record that
-- represents one magnet.  Since all operations require the direction of
-- travel, ie polarity, this is passed as a field of the record.
--
-- Author: Rob Allen, Swinburne University of Technology
--
-- 31-May-00 (Topolog derived from old layout and simrail.block_feature_pkg)
--  8-May-02 (added Check_Leaving_Turnout, Feature_At, Feature_Kind_At)
-- 12-May-02 (added Converging to Check_Leaving_Turnout)
-- 18-Jun-02 (branch version, generalised Crossing to Zone)
--  9-May-03 (revise & move Sensor_Block_Numbers to spec)
--  1-May-04 (mainstreamed branch version: has Zones + Crossing
--                      needs revised Rail_Typ)
-- 11-Mar-07 (v2.0) to match Rail_Typ(v3.3) (Dio48def, Pc14defs packed arrays)
--       Next_Pos now updates Pos.Next_Expected (previously Next_Sensor)
-- 23-Mar-07 improved comments
-- 28-May-07 (v2.1) add Next_Sensor_Expected() and renamed Next_Sensor()
--                     to Next_Sensor_Along()
-- 26-Mar-08 (v2.2) converted to 2008 layout, renamed Topolog2, reengineered,
--                  extra operations for the new layout
-- 15-Apr-08 (v2.3) replaced Next_Pos functions by Advance_Pos procedures
-- 18-Apr-08 (v2.4) added Initialise_After,
-- 22-Apr-08  released
-- 25-Apr-08 (v2.5) Find_Next_Sensor_{Along|Expected} replace one-parameter
--           Next_Sensor_{Along|Expected}, revise comments
-- 20-May-08 (v2.6) Sensor_Block_Numbers now 2-D for boundary sensors
-- 26-Jun-08 (v2.8) fix Turnout_Data(15)
--  7-May-09 (v2.8.1) more comments for Turnout_Data, Find_Next_Sensor_Expected
-- 19-May-11 (v2.8.2) fix comments for Sensor_Block_Numbers, replace remaining
--                  "reflector" by "magnet"
--  9-Mar-12 (v2.8.2) made compatible with raildefs 2.5
-- 11-Feb-13 (v2.8.3)  separate type for Zone_Id, sideswipe zone code
--  9-Mar-13 (v2.9)  stopping distances, improve Advance, Resume comments
-- 23-Apr-13 (v2.9)  improve Resume, Turn_Around comments
--  1-Aug-13 (v2.9)  include s43 (Z3) in stopping distances
--  5-Jun-13 (v2.9a) Group 1 student improvements, added extra crossing segs
-- 12-May-15 (v3.0)  Adopt improvements: extra out parameters for
--                   Check_Entering_Chained_Turnout, Check_Entering_Crossing
--                   and corresponding Leaving.  Adjust some stopping distances.  
--                   Correct comment for Check_Leaving_Chained_Turnout.
--                   Tidy other comments.  Rename Boundary_Zone to Zone.
--
-- This code is copyright of the author and may be used within Swinburne
-- only.  This header comment must be reproduced.
--
with Raildefs;
use Raildefs;
package Topolog2 is

   -- PROVIDED OPERATIONS
   --
   -- All operations have parameter or result Pos: Train_Position
   --
   -- MUTATORS
   --   Initialise_After
   --   Advance_Pos
   --   Resume
   --   Turn_Around
   --
   -- VERY IMPORTANT:
   -- Advance_Pos is meant to be called at each side of a sensor, ie when the
   -- On event occurs and again when the Off event occurs.
   --
   -- QUERIES to call with pos at front of train just AFTER pos has advanced
   -- onto a sensor:
   --    Check_Entering_Turnout
   --    Check_Entering_Chained_Turnout
   --    Check_Entering_Crossing
   --    Check_Entering_Zone
   --    Check_Entering_Sideswipe_Zone
   --    Is_Leaving_Powered_Track
   --
   -- QUERIES to call with pos at rear of train just BEFORE pos advances
   -- off a sensor:
   --    Check_Leaving_Turnout
   --    Check_Leaving_Chained_Turnout
   --    Check_Leaving_Crossing
   --    Check_Leaving_Zone
   --    Check_Leaving_Sideswipe_Zone
   --
   -- OTHER QUERIES:
   --    Feature_At
   --    Feature_Kind_At
   --    Is_Sensor_At
   --    Check_Turnout_Info_Need
   --    Find_Next_Sensor_Along
   --    Find_Next_Sensor_Expected
   --    [legacy: you probably don't need the last two]
   --
   -- PROVIDED TYPES
   --
   -- Major:
   --   Train_Position
   --   Feature_Kind
   --   Feature_Descriptor
   --   Turnout_Descriptor
   --   Zone_Id
   -- Minor:
   --   Feature_Idx
   --   Substate_Type
   --   Chain_Type
   --
   -- PROVIDED CONSTANTS
   -- Major:
   --   Turnout_Data
   --   Is_Boundary_Sensor
   --   Sensor_Block_Numbers
   --   Sensor_Stopping_Distances
   --   values for Crossing_Id: Main_Diagonal, North_Crossover, ...
   --
   -- REQUIRED TYPES
   --   Raildefs.Block_Idx, Raildefs.Crossing_Idx, Raildefs.Polarity_Type,
   --   Raildefs.Sensor_Idx, Raildefs.Turnout_Idx, Raildefs.Turnout_Pos
   --   plus subtypes and values of these types and Opposite(pol)

   -- General note: this package uses dummy values "No_xxx" and extended
   -- types "xxx_idx" that include the dummy value, eg Sensor_Idx is
   -- Sensor_Id plus the dummy value No_Sensor.  These are defined in Raildefs
   -- (except for Feature_Idx)

   -- This array is easy to use - it duplicates information below in
   -- Turnout_Data.
   Straight_Is_Left : constant
   array (Turnout_Id) of Boolean
      := (1..5|7|10|13|15|17|19 => True, others => False);

   Max_Features : constant := 131;  -- internal use
   type Feature_Idx is range 0 .. Max_Features;  -- internal use
      -- (not private to aid testing)

   type Feature_Kind is
         (Sensor,
          Turnout,
          Plain_Track,
          Crossing_Track, -- (v2.2) renamed
          Zone,   -- track round a boundary sensor (v2.2), or sideswipe(v2.8.3)
                  -- (v3.0) renamed from Boundary_Sensor
          Dead_Track);    -- unpowered track at each end of block 1 and
   --                        at ends of sidings. (v2.2)

   -- Train_Position represents the position (in a topological sense) of
   -- ONE magnet on a train.  (A train will need two instances of this
   -- type, front and rear.  When the train reverses direction the roles
   -- must change.)
   -- The track layout is divided into features.  The smallest are sensors,
   -- 10mm approx long but because this is a topological description of the
   -- layout, actual dimensions are not stored -- except stopping distances
   -- (added v2.9).  It is far better to use the various query functions below
   -- than code using feature numbers.
   -- Polarity is stored as a field of Train_Position because most decisions
   -- need to know the direction of travel.
   -- Train_Position is normally modified by either version of Advance_Pos below.
   --
   -- Advance_Pos and Turn_Around maintain Polr, Substate and Next_Expected.
   -- However when you reverse a train you _might_ need to maintain
   -- Next_Expected yourself by calling Next_Sensor_Expected.

   type Substate_Type is
         (Norm,
          Just_After,
          Just_Before);

   type Train_Position is
      record
         Block_Num     : Block_Id      := 1;
         Polr          : Polarity_Type := Normal_Pol;
         Within        : Feature_Idx   := 112;       -- internal use (eg).
         Substate      : Substate_Type := Norm;      -- only relevant to sensors
         Next_Expected : Sensor_Idx    := No_Sensor; -- (init to dummy value)
      end record;
   -- Note: The purpose of Next_Expected is to make handling sensor events
   -- easier; it says which sensor will produce the next interrupt
   -- relevant to this magnet assuming the train keeps moving in the
   -- direction determined by Polr.  If the magnet is over a sensor then
   -- that sensor will next produce an Off event. cf function
   -- Next_Sensor_Along which ignores the current sensor.

   -- Crossing_Idx values -- (v2.2)
   -- No_Crossing : Crossing_Idx := 0; -- defined in raildefs
   Main_Diagonal    : constant Crossing_Id := 1;
   North_Crossover  : constant Crossing_Id := 2;
   South_Crossover  : constant Crossing_Id := 3;
   North_Tram_Cross : constant Crossing_Id := 4;
   South_Tram_Cross : constant Crossing_Id := 5;

   Num_Zones : constant := 6;   -- 4 boundary, 2 sideswipe
   type Zone_Idx is range 0 .. Num_Zones;   -- (v2.8.3) now consec nos
   subtype Zone_Id is Zone_Idx range 1 .. Num_Zones;
   No_Zone : constant Zone_Idx := 0;

   Central_Sensor : constant array (Zone_Id) of Sensor_Idx :=
    (21, 30, 43, 62, 39, 47);
    -- see also Is_Boundary_Sensor (below)

   -- Feature_Descriptor is used internally.  It is defined here because
   -- function Feature_At returns this type.
   --
   type Feature_Descriptor(Kind : Feature_Kind := Plain_Track) is
   record
      Block_Num    : Block_Id;
      case Kind is
         when Sensor =>
            Sid     : Sensor_Id;
            Block2 : Block_Idx;     -- /= No_Block if boundary
         when Turnout =>
            Tid    : Turnout_Id;
            Next_Tu : Feature_Idx;  -- internal use
         when Zone =>
            Zid    : Zone_Id;  -- serial number
            Centre : Sensor_Idx;  -- id of the central 'boundary' sensor (v2.2)
         when Crossing_Track =>
            Kid    : Crossing_Idx;
         when others =>
            null;
      end case;
   end record;

   -- useful static turnout data:
   -- Use this to find out what block is being entered at the other side
   -- of turnout and what polarity, also the relationship between
   -- left-right and straight-turned and the orientation (converging
   -- -diverging of the turnout to the normal polarity direction
   -- of the turnout centre.  Note: topologically a turnout extends out
   -- to the surrounding (guard) sensors.
   type Turnout_Descriptor is
      record
         Block_Num             : Block_Id; -- eg 5 for turnout 2
         Diverging_When_Normal : Boolean;  -- eg True for turnout 12
         Straight_Is_Left      : Boolean;  -- eg True for turnout 13
         Block_St,
         Block_Tu              : Block_Id; -- eg 13,22 for turnout 12
         Swap_Pol_Tu           : Boolean;  -- eg True for turnout 16
      end record;
   --
   --   eg:  turnout 16
   -- == Block_Num=16 ===\====>=====  =>=Block_St=17 ==>===
   --                     \
   --                      ==>=====  =<=Block_Tu=22 ==<==
   --
   -- (The above diagram shows an example of an inverting join: the
   -- arrows represent normal polarity and there is a flip in the Tu branch.)
   -- To maintain continuity polarity must be inverted when moving between
   -- blocks 16 and 22, and 19 and 23 within turnouts 17 and 19.
   -- The Advance_Pos procedures automatically adjust the polr attribute
   -- of Pos.  Knowing what turnout is being entered (discovered by
   -- calling Check_Entering_xxx_Turnout) and the desired turnout setting
   -- you can find what block is ahead by grabbing the fields Block_Num,
   -- Block_Tu and Block_St as appropriate.
   --
   Turnout_Data : constant
   array (Turnout_Id) of Turnout_Descriptor :=
      (1 => (11, True, True, 20, 21, False),
      2 => (5, False, True, 21, 2, False),
      3 => (5, False, True, 5, 3, False),
      4 => (5, False, True, 5, 4, False),
      5 => (5, False, True, 5, 20, False),
      6 => (6, True, False, 7, 14, False),
      7 => (7, False, True, 6, 13, False),
      8 => (7, True, False, 8, 9, False),
      9 => (9, True, False, 10, 18, False),
      10 => (10, False, True, 9, 17, False),
      11 => (11, False, False, 10, 24, False),
      12 => (12, True, False, 13, 22, False),
      13 => (13, True, True, 14, 7, False),
      14 => (14, False, False, 13, 6, False),
      15 => (15, False, True, 14, 23, False),
      16 => (16, True, False, 17, 22, True),
      17 => (17, True, True, 18, 10, False),
      18 => (18, False, False, 17, 9, False),
      19 => (19, False, True, 18, 23, True) );

   -- A boundary sensor (or "true boundary" sensor) is one that is exactly
   -- at the join between blocks.  There are only 4 (2008).
   --
   Is_Boundary_Sensor : constant
   array(Sensor_Id) of Boolean :=
      (21|30|43|62=>True, others=>False);  -- no margin for coasting here
   -- unless the block beyond is owned and set up.
   -- Note 'Boundary Zones' surround these and are associated
   -- with these sensor numbers.  Each such zone has two pieces of track.
   -- Zones 3 and 4 (sensors 43 and 62) are very wide!

   -- The following array tells what block a sensor belongs to.
   -- It is no longer used in the package body but is the only
   -- source of the information.
   -- The boundary sensors, 21,30,43,62, can be considered as
   -- belonging to two blocks, so the block being entered
   -- depends on polarity,eg
   --   Sensor_Block_Numbers(Normal_Pol,21)=12,
   --   Sensor_Block_Numbers(Reverse_Pol,21)=19.
   -- The various subprograms treat them specially.
   -- (No_Block is the dummy value for sensors that are not installed.)
   --
   Sensor_Block_Numbers : constant
   array(Polarity_Type,Sensor_Id) of Block_Idx :=
   ( Normal_Pol=> (
      25|27|29|31|33=>1,
      3|4=>2,
      5|6=>3,
      7|8=>4,
      12|14|16|26|28=>5,
      30|32|34=>6,
      36|38=>7,
      60=>8,
      40|42=>9,
      44|46=>10,
      -- block 11 has no sensors!
      21|23|35=>12,
      37=>13,
      39=>14,
      41=>15,
      43|45=>16,
      47=>17,
      49=>18,
      51|15|17|19=>19,
      18|20|22|24=>20,
      1|2|10=>21,
      58|56|50|48=>22,
      53|55|61|63=>23,
      62|64=>24,
      9|11|13|52|54|57|59=>No_Block  -- todo: install 9,13
     ),
     Reverse_Pol => (
      25|27|29|31|33=>1,
      3|4=>2,
      5|6=>3,
      7|8=>4,
      12|14|16|26|28=>5,
      30=>5, 32|34=>6,
      36|38=>7,
      60=>8,
      40|42=>9,
      44|46=>10,
      -- block 11 has no sensors!
      21=>19, 23|35=>12,
      37=>13,
      39=>14,
      41=>15,
      43=>15, 45=>16,
      47=>17,
      49=>18,
      51|15|17|19=>19,
      18|20|22|24=>20,
      1|2|10=>21,
      58|56|50|48=>22,
      53|55|61|63=>23,
      62=>8, 64=>24,
      9|11|13|52|54|57|59=>No_Block  -- todo: install 9,13
     )
   );

   -- Table of safe distances BEYOND the centre of a sensor (mm).
   -- (These were measured with a tape measure and in most cases
   -- there is another W mm available due to magnet-centre to
   -- wheel-centre separation being about 15mm.  Values without
   -- this extra are where the danger is side-swipe.  However at
   -- sensors 39 and 47 even stopping dead will not be safe from
   -- sideswipe so the limit is electrical, ie distance from block
   -- boundary.  Sideswipe zones were introduced for prevention.)
   -- Values >= Nominal_Long_Dist probably do not need speed
   -- limiting (assuming block power supply <=13volt).
   W : constant := 12;
   Unpowered_Dist : constant := 40; -- must keep some wheels powered
   Nominal_Long_Dist : constant := 125;
   --
   Sensor_Stopping_Distances : constant
   array(Polarity_Type,Sensor_Idx) of Natural :=
   ( Normal_Pol=> (
         4=>28+W,  --was 95+W, -- out of siding
         6=>28+W,  --was 68+W, -- out of siding
         8=>28+W,  --was 105+W, -- out of siding
         10=>97+W,
         12=>42+W,  --was 70+W,
         14=>36+W,  --was 77+W,
         16=>41+W,  --was 51+W,
         28=>165+W,
         21|30|43|62=>W,  -- boundary sensors (assuming block beyond not owned)
         34=>175+W,
         38=>60+W,
         27=>68,  -- tram crossing
         15=>80,  -- tram crossing
         20=>95,  -- tram crossing
         24=>55+W,
         19=>125+W,
         35=>107+W,
         37=>170+W,
         39=>14+W,   -- in a sideswipe zone
         45=>94+W,
         48=>69,   -- entering a sideswipe zone
         42|47=>160+W,
         49=>214+W,
         63=>47,   -- entering a sideswipe zone
         46|64=>50,
         55=>105,  -- main crossing
         56=>87,   -- main crossing
         33=>Unpowered_Dist,
         others=>Nominal_Long_Dist
     ),
      Reverse_Pol => (
         1=>140+W,
         18=>56+W,
         44|49=>103+W,
         47=>55+W,   -- in a sideswipe zone
         41=>74+W,
         21|30|43|62=>W,  -- boundary sensors (assuming block beyond not owned)
         40=>58+W,
         60=>75+W,
         36|39=>48+W,
         37=>32,
         58=>36,
         32=>140+W,
         23=>125+W,
         26=>121+W,
         16=>41+W,
         14=>36+W,
         12=>42+W,
         29=>65,  -- tram crossing
         17=>80,  -- tram crossing
         22=>60,  -- tram crossing
         50=>85,  -- main crossing
         61=>96,  -- main crossing
         3|5|7|25=>Unpowered_Dist,
         others=>Nominal_Long_Dist
     )
   );


   -- type for use in Check_*_Turnout calls (v2.2)
   --
   -- No - this turnout does not connect to another turnout
   --      directly or via one sensor
   -- Sensored - there is exactly one sensor between this and
   --      the next "chained" turnout so you might need to "go
   --      slow" to give the user an option to change the 2nd
   --      turnout
   -- Immediate - this turnout connects directly to another so
   --      that you must set up both before entering the first
   --      and when you leave the second you are also leaving
   --      the first
   --
   type Chain_Type is
         (No,
          Sensored,
          Immediate);

   Topology_Error       : exception;
   Illegal_Position     : exception;
   Illegal_Turnout_Info : exception;
   Missing_Turnout_Info : exception;

   -- OPERATIONS of Train_Position:  QUERIES ------------

   ------------------------------------------------------
   -- Check_Entering_Crossing
   -- For avoiding collisions crossings are mutex regions.
   -- This procedure sets Entering true if Pos is over (or just_after)
   -- a sensor that guards a crossing.  If so then Which will identify
   -- that crossing.
   -- Note that the tram crosses two such crossings.  It
   -- enters North_Tram_Cross and South_Tram_Cross together.
   -- This will be shown in Next; otherwise Next := No_Crossing. (v3.0)
   -- There is no point calling this procedure if Check_Entering_Turnout
   -- has returned Entering true.  In particular this
   -- procedure does not work for the two "crossovers"
   -- North_Crossover between sensors 34-39, 36-37; and
   -- South_Crossover between sensors 42-49, 44-47
   -- BUT Check_Entering_Chained_Turnout has out parameters
   -- for them (v3.0).
   -------------------------------------------------------
   procedure Check_Entering_Crossing (
         Pos      : in     Train_Position;
         Entering :    out Boolean;
         Which    :    out Crossing_Idx;
         Next     :    out Crossing_Idx           );

   ------------------------------------------------------
   -- Check_Entering_Turnout
   -- sets Entering true if Pos is over (or just_after) a sensor that
   -- guards a turnout.  If so then Which will identify that turnout
   -- and Converging and Required_Setting will be changed to match.
   -- NOTE: not converging => Middle
   -- (v2.2) If Which denotes a chained turnout (feeds into, or
   -- _may_ feed into depending on setting) another turnout
   -- (eg 9 into 10 or 18 in normal polarity) then Chained will be
   -- set /= No.
   -------------------------------------------------------
   procedure Check_Entering_Turnout (
         Pos              : in     Train_Position;
         Entering         :    out Boolean;
         Which            :    out Turnout_Idx;
         Converging       :    out Boolean;
         Required_Setting :    out Turnout_Pos;
         Chained          :    out Chain_Type      );

   ------------------------------------------------------
   -- Check_Entering_Chained_Turnout     (v2.2)
   -- pre: 1. Check_Entering_Turnout has set Entering true
   --  and Chained /= No.
   --  2. Setting1 /= Middle and if converging Setting1 is
   --  consistent with Pos
   -- Sets Which2 to identify that the second turnout
   -- and Converging2 and Required_Setting2 will be changed to match.
   -- Setting1 must be set to the (assumed) position of the
   -- first turnout and must be correct whether the first turnout is
   -- diverging or converging, else Illegal_Turnout_Info is raised.
   -- Example: Pos is over sensor 47 in normal polarity.  Entering = true,
   -- Which = 17, Chained = Immediate.  If Setting1 is Turned then
   -- Entering2 := true, Which2 := 10,  Converging2 := true,
   -- Required_Setting2 := Turned,
   -- Entering_Crossing := True, Which_Crossing := South_Crossover.
   -- If pre-condition 1 is false then Entering2 := false,
   --    Which2 := No_Turnout, Entering_Crossing := false, etc.
   -- If precondition 2 is false then throws Illegal_Turnout_Info.
   -------------------------------------------------------
   procedure Check_Entering_Chained_Turnout (
         Pos               : in     Train_Position;
         Setting1          : in     Turnout_Pos;
         Entering2         :    out Boolean;
         Which2            :    out Turnout_Idx;
         Converging2       :    out Boolean;
         Required_Setting2 :    out Turnout_Pos;
         Entering_Crossing :    out Boolean;
         Which_Crossing    :    out Crossing_Idx);

   ------------------------------------------------------
   -- Check_Entering_Zone
   -- (Boundary zones are track sections either side of a boundary sensor
   -- For avoiding collisions they are mutex regions.)
   -- Sets Entering true if Pos is over (or just_after) a sensor that
   -- guards a boundary zone.  If so then Which will identify that zone
   -- (else No_Zone).
   -- There is no point calling this procedure if Check_Entering_Turnout
   -- or Check_Entering_Crossing has returned Entering true.
   -------------------------------------------------------
   procedure Check_Entering_Zone (
         Pos      : in     Train_Position;
         Entering :    out Boolean;
         Which    :    out Zone_Idx       );

   -------------------------------------------------------
   -- Check_Entering_Sideswipe_Zone   (v2.9)
   -- (Sideswipe zones are parts of the layout centered on sensors
   -- 39 and 47 where side-swipe collisions can occur. Trains
   -- stopped at these protrude into turnouts 15 and 16 resp.)
   -- Pre: Pos is over (or just_after) a sensor and
   -- Check_Entering_Turnout has indicated 'entering'.  Setting is
   -- the setting of that Turnout, assumed not Middle if needed.
   -- Sets Entering=True if Pos will go near one of the two sensors,
   -- and Which will denote the sideswipe zone (else No_Zone).
   -------------------------------------------------------
   procedure Check_Entering_Sideswipe_Zone (
         Pos      : in     Train_Position;
         Turnout  : in     Turnout_Id;
         Setting  : in     Turnout_Pos;
         Entering :    out Boolean;
         Which    :    out Zone_Idx       );

   ------------------------------------------------------
   -- Check_Leaving_Crossing
   -- sets Leaving true if Pos is over (or just_after) a sensor that
   -- rear-guards a crossing.  If so then Which will identify that Crossing.
   -- Note that the tram crosses two such crossings.  When it
   -- leaves one, it leaves both; Next shows the other.
   -- There is no point calling this procedure if Check_Leaving_Turnout
   -- has returned Leaving true.  In particular this
   -- procedure does not work for the two "crossovers"
   -- North_Crossover between sensors 34-39, 36-37; and
   -- South_Crossover between sensors 42-49, 44-47
   -- BUT Check_Leaving_Chained_Turnout has out parameters
   -- for them (v3.0).
   -------------------------------------------------------
   procedure Check_Leaving_Crossing (
         Pos     : in     Train_Position;
         Leaving :    out Boolean;
         Which   :    out Crossing_Idx;
         Next    :    out Crossing_Idx    );

   -------------------------------------------------------
   -- Check_Leaving_Turnout
   -- sets Leaving true if Pos is over (or just_after) a sensor that
   -- rear-guards a turnout.  If so then Which will identify that turnout
   -- and Converging will match.  If this is the second of a pair
   -- of Immediate chained turnouts (no sensor between) then
   -- Chained:=Immediate else Chained:=No
   -- (note this is different to Check_Entering_Turnout)
   -------------------------------------------------------
   procedure Check_Leaving_Turnout (
         Pos        : in     Train_Position;
         Leaving    :    out Boolean;
         Which      :    out Turnout_Idx;
         Converging :    out Boolean;
         Chained    :    out Chain_Type   );  -- (v2.2)

   ------------------------------------------------------
   -- Check_Leaving_Chained_Turnout     (v2.2)
   -- pre: Check_Leaving_Turnout has set Chained = Immediate.
   -- We consider the current turnout to be number 2 and the one behind
   -- to be number 1.
   -- Setting2 is the position of the current.
   -- Which1 := identify that the turnout behind, No_Turnout if none.
   -- Converging1 is set to according to the turnout behind.
   -- If Setting2 is inconsistent with that needed by the second turnout
   -- and previos call of Check_Leaving Turnout set Converging=True
   -- then Illegal_Turnout_Info is raised.
   -- Blocks to release can be found using the array Turnout_Data and the known
   -- positions of both turnouts, together with the two Converging values.
   --
   -- Example: Pos is over sensor 47 in reverse polarity.
   -- Check_Leaving_Turnout has set Leaving = true,
   -- Which = 17, Converging = True, Chained = Immediate.
   -- If Setting2 is Straight then Leaving1 := true, Which1 := 18,
   --    Converging1 := false, Leaving_Crossing := false,
   --   Which_Crossing := No_Crossing.
   -- If Setting2 is Turned then Leaving1 := true, Which1 := 10,
   --    Converging1 := False, Leaving_Crossing := true,
   --    Which_Crossing := South_Crossover.
   -- If pre-condition is false then Leaving1 := false,
   -- Which1 := No_Turnout, Converging1 := false,
   -- Leaving_Crossing := False, Which_Crossing := No_Crossing.
   -------------------------------------------------------
   procedure Check_Leaving_Chained_Turnout (
         Pos         : in     Train_Position;
         Setting2    : in     Turnout_Pos;
         Leaving1    :    out Boolean;
         Which1      :    out Turnout_Idx;
         Converging1 :    out Boolean;
         Leaving_Crossing : out Boolean;
         Which_Crossing   : out Crossing_Idx);

   -------------------------------------------------------
   -- Is_Leaving_Powered_Track
   -- returns true if Pos is over a sensor that guards
   -- dead track, eg Reverse_Pol at sensor 3 in siding block 2.
   -------------------------------------------------------
   function Is_Leaving_Powered_Track (
         Pos     :  Train_Position  ) return Boolean;

   -------------------------------------------------------
   -- Check_Leaving_Zone
   -- sets Leaving true if over a sensor that rear-guards
   -- a zone.  If so then Which will identify that zone.
   -- There is no point calling this procedure if Check_Leaving_Turnout
   -- or Check_Leaving_Crossing has returned Entering true.
   -------------------------------------------------------
   procedure Check_Leaving_Zone (
         Pos     : in     Train_Position;
         Leaving :    out Boolean;
         Which   :    out Zone_Idx         );

   -------------------------------------------------------
   -- Check_Leaving_Sideswipe_Zone
   -- Pre: Pos is over (or just_past) a sensor and    (v2.9)
   -- Check_Leaving_Turnout has set Leaving true.  Setting is
   -- the setting of that turnout, assumed not Middle if needed.
   -- Sets Leaving=True if the zone can be released,
   -- and Which will denote that zone (else No_Zone).
   -------------------------------------------------------
   procedure Check_Leaving_Sideswipe_Zone (
         Pos      : in     Train_Position;
         Turnout  : in     Turnout_Id;
         Setting  : in     Turnout_Pos;
         Leaving  :    out Boolean;
         Which    :    out Zone_Idx       );

   ------------------------------------------------------
   -- Check_Turnout_Info_Need
   -- sets Needed true if a following call of Advance_Pos or
   -- Resume will need turnout position data.  If so then Which
   -- will identify that turnout.
   -- Use before Resume.
   -- (Obsolete for Advance_Pos - use 4-parameter Advance_Pos first
   -- and then, if needed, the 3-parameter version.)
   -------------------------------------------------------
   procedure Check_Turnout_Info_Need (
         Pos         : in     Train_Position;
         Needed      :    out Boolean;
         For_Turnout :    out Turnout_Id      );

   ------------------------------------------------------
   -- Feature_At
   -- returns the feature at Pos
   -------------------------------------------------------
   function Feature_At (
         Pos : Train_Position )
     return Feature_Descriptor;

   ------------------------------------------------------
   -- Feature_Kind_At
   -- returns the kind of the feature at Pos (less cpu than Feature_At)
   -------------------------------------------------------
   function Feature_Kind_At (
         Pos : Train_Position )
     return Feature_Kind;

   ------------------------------------------------------
   -- Find_Next_Sensor_Along
   -- finds following sensor along the track moving in the
   -- direction Pos.Polr.  If Pos is actually on a sensor then
   -- this method finds the one after.
   -- Sets The_Sensor to No_Sensor if Is_Leaving_Powered_Track.
   -- If there is a need for turnout position data then
   -- The_Sensor:=No_Sensor, Need_Setting:=True, For_Turnout:=
   -- (the relevant id).  To recover from this, call function
   -- Next_Sensor_Along providing the relevant setting.
   -------------------------------------------------------
      procedure Find_Next_Sensor_Along(
      Pos          : in     Train_Position;
      The_Sensor   :    out Sensor_Idx;
      Need_Setting :    out Boolean;
      For_Turnout  :    out Turnout_Id );

   ------------------------------------------------------
   -- Find_Next_Sensor_Expected
   -- finds following sensor along the track moving in the
   -- direction Pos.Polr.
   -- Sets The_Sensor to No_Sensor if Is_Leaving_Powered_Track.
   -- If there is a need for turnout position data then
   -- The_Sensor:=No_Sensor, Need_Setting:=True, For_Turnout:=
   -- (the relevant id).  To recover from this, call function
   -- Next_Sensor_Expected providing the relevant setting.
   -------------------------------------------------------
   procedure Find_Next_Sensor_Expected(
      Pos          : in     Train_Position;
      The_Sensor   :    out Sensor_Idx;
      Need_Setting :    out Boolean;
      For_Turnout  :    out Turnout_Id );

   ------------------------------------------------------
   -- Is_Sensor_At
   -- returns true if Pos is over The_Sensor.
   -- ( Is_Sensor_At and Is_Sensor_Ahead_Of are disjoint.)
   -------------------------------------------------------
   function Is_Sensor_At (
         Pos        : Train_Position;
         The_Sensor : Sensor_Id       )
     return Boolean;

   ------------------------------------------------------
   -- Is_Sensor_Ahead_Of
   -- returns true if The_Sensor is next ahead of the current Pos.
   -- Calls Next_Sensor(pos)
   -- obsolete? (not implemented)
   -------------------------------------------------------
--   function Is_Sensor_Ahead_Of (
--         Pos        : Train_Position;
--         The_Sensor : Sensor_Id       )
--     return Boolean;

   ------------------------------------------------------
   -- Next_Sensor_Along (pos, turnpos)
   -- returns following sensor id along a block
   -- moving in the direction Pos.Polr
   -- If Pos is actually on a sensor then this method returns
   -- the id of the one after.
   -- Assumes the supplied turnout setting data is for the
   -- relevant turnout eg as reported by Find_Next_Sensor_Along.
   -- Ignores it if not needed.
   -- Raises Missing_Turnout_Info if Middle supplied when
   -- Straight or Turned is needed.
   -- (Rarely needed.)
   -------------------------------------------------------
   function Next_Sensor_Along (
         Pos     : Train_Position;
         Setting : Turnout_Pos     )
     return Sensor_Idx;


   ------------------------------------------------------
   -- Next_Sensor_Expected (pos, turnsetting)
   -- returns id of sensor which will next produce an event (could
   -- be the current one if pos is over a sensor) moving in the
   -- direction Pos.Polr
   -- Assumes the supplied turnout setting data is for the
   -- relevant turnout eg as reported by Find_Next_Sensor_Expected.
   -- Ignores it if not needed.
   -- Raises Missing_Turnout_Info if Middle supplied when
   -- Straight or Turned is needed.
   -- (Rarely needed - may need to be called after Turn_Around.)
   -------------------------------------------------------
   function Next_Sensor_Expected (
         Pos     : Train_Position;
         Setting : Turnout_Pos     )
     return Sensor_Idx;


   -- operations of Train_Position:  MUTATORS ------------

   -------------------------------------------------------
   -- Initialise_After (sensor)
   -- (v2.4)
   -- Returns a Pos that is after the sensor in the normal direction
   -- i.e. in normal polarity
   -- Raises Illegal_Position if the implied position is on a
   -- turnout or if the sensor is not installed.
   -------------------------------------------------------
   function Initialise_After (The_Sensor : Sensor_Id)
      return Train_Position;

   -------------------------------------------------------
   -- Advance_Pos (pos,stopping, need_setting, for_turnout)
   -- (v2.9)
   -- Changes Pos to the following value moving along a block in
   -- the direction Pos.Polr (unless Stopping -- see below).
   -- Assumes a sensor event for sensor number Pos.Next_Expected
   -- has occurred.
   -- If need turnout position data for a diverging turnout
   -- (possibly the second of a pair of chained immediate turnouts)
   -- then Need_Setting:=True and For_Turnout:=<which one>
   -- and Pos will not be changed.
   -- If advancing into an immediate chained pair of turnouts then
   -- Pos may jump (immediately) to the second turnout.   <<<< check!
   --
   -- Pos.Block_Num will be changed when moving off a sensor:
   -- (1) When entering a turnout this will be set to the centre block.
   -- (2) In the case of a boundary sensor it will be the next block.
   --     The value while over the boundarysensor will be notionally where Pos
   --     came from.  (todo: Is that correct after Turn_Around?)
   -- (3) When entering unpowered track it will be the value No_Block.
   --
   -- Pos.Polr will be changed at the edge of the sensor nearest the
   -- inverting branch of a turnout.  (The 2008 layout has two such
   -- inverting joins between blocks both within turnouts.)
   -- When on a turnout, Polr will be appropriate for the central part
   -- of the turnout (consistent with Block_Num).
   --
   -- Stopping means the train has zero power and is skidding
   -- to a stop.  Only use this for Pos at the leading magnet of the train.
   -- (Stopping should always be false for the current rear.)
   -- If Stopping and Feature_Kind_At(Pos)=Sensor then this
   -- operation will change Pos.Substate to Just_After and make
   -- no other changes - these are deferred until procedure
   -- Resume is called later.
   --
   -- Normally this procedure defines result.Next_Expected but
   -- this will be value No_Sensor if the new position is on unpowered
   -- track (eg end of sidings).
   -- This version (v2.9) of Advance_Pos will set Need_Setting and For_Turnout
   -- for other cases involving turnouts.
   -------------------------------------------------------
   procedure Advance_Pos (
         Pos          : in out Train_Position;
         Stopping     : in     Boolean;
         Need_Setting :    out Boolean;
         For_Turnout  :    out Turnout_Id
       );

   -------------------------------------------------------
   -- Advance_Pos (pos, stopping, setting)
   -- (v2.9)
   -- Assuming not Stopping, changes Pos to the following value
   -- moving along a block in the direction Pos.Polr.
   -- Setting is assumed to be the turnout position data for
   -- the diverging turnout.  (When there are immediate chained
   -- turnouts one will be converging the other diverging.  This
   -- software does not support them being the same.)
   -- If Middle is supplied and a value /= Middle is needed to
   -- "advance" then exception Missing_Turnout_Info will be raised
   -- and Pos will not be changed.
   -- If advancing into an immediate chained pair of turnouts then
   -- Pos may be on the first or second turnout.   <<<< check!
   --
   -- Pos.Block_Num will be changed when moving off a sensor:
   -- (1) When entering a turnout this will be set to the centre block.
   -- (2) In the case of a boundary sensor it6 will be the next block.
   --     The value while over the sensor will be notionally where Pos
   --     came from.
   -- (3) When entering unpowered track it will be the value No_Block.
   --
   -- Pos.Polr will be changed at the edge of the sensor nearest the
   -- inverting branch of a turnout.  (The 2008 layout has two such
   -- inverting joins between blocks both within turnouts.)
   -- When on a turnout, Polr will be appropriate for the central part
   -- of the turnout (consistent with Block_Num).
   --
   -- Stopping means the train has zero power and is skidding
   -- to a stop.  Only use this for Pos at the leading end of the train.
   -- NOTE: Stopping must always be false for the rear position.
   --                     ======================================
   -- If Stopping and Feature_Kind_At(Pos)=Sensor then this
   -- operation will change Pos.Substate to Just_After and make
   -- no other changes - these are deferred until procedure
   -- Resume is called later.
   --
   -- Normally this procedure defines result.Next_Expected but
   -- this will be value No_Sensor if the new position
   -- is on unpowered track (eg end of sidings).
   -- Other cases involve turnouts and if they need Setting/=Middle then
   -- exception  Missing_Turnout_Info will be raised.
   -------------------------------------------------------
   procedure Advance_Pos (
         Pos      : in out Train_Position;
         Stopping : in     Boolean;
         Setting  : in     Turnout_Pos
      );


   ------------------------------------------------------
   -- Resume (Pos, turnoutsetting)
   -- If Pos.Substate = Just_After then adjusts Pos to truly
   -- represent the position including changing Substate to Norm and
   -- setting Next_Expected.
   -- Call this to clean up after the current front of the train has
   -- overshot a sensor, ie skidded beyond.  That is, call it when
   -- the train has come to a complete stop and before motion is
   -- resumed and a subsequent sensor event causes Advance_Pos
   -- to be called.
   --
   -- This does nothing if Substate is Norm or Just_Before.  (Just_Before
   -- is cleaned up by the next Advance_Pos.)  If the Stopping parameter
   -- of Advance_Pos has been used correctly for the preceding skidding event,
   -- ie Stopping=True for front pos only, then only the current front pos
   -- can have Substate=Just_After.  (There may have been calls of Turn_Around
   -- in between.)  Hence calling it with the rear pos is harmless.
   --
   -- If in doubt call Check_Turnout_Info_Need before the call
   -- to ensure the appropriate Setting is provided.
   -- TODO: Provide two versions of Resume like Advance_Pos has.
   --       Do general post Turn_Around cleanup.
   --
   -- Raises Illegal_Position if substate = Just_After
   -- and Feature_Kind_At(Pos) /= Sensor -- clearly error.
   -- Raises Missing_Turnout_Info if the value supplied for Setting is
   -- Middle but a non-Middle value is needed (as in Advance_Pos 3 param version).
   -------------------------------------------------------
   procedure Resume (
         Pos     : in out Train_Position;
         Setting : in     Turnout_Pos := Middle     );

   ------------------------------------------------------
   -- Turn_Around (pos)
   -- Adjusts polarity, substate, block_num and next_Expected
   -- attributes of pos.
   -- Assumes client code has swapped the front and
   -- rear position variables of a train and then calls this
   -- operation on each.
   -- If Pos is over an immediate chained pair of turnouts then it
   -- may be over either of the pair; the next Advance_Pos will cope.
   -- If over a boundary sensor then Block_Num could be either.
   --
   -- Sets field Next_Expected to No_Sensor if (Substate is Norm and Pos
   -- is over a turnout and turnout information is needed) or
   -- (if the new direction is into unpowered track eg end of a siding).
   -- Except for unpowered track (where No_Sensor is correct), Next_Expected
   -- must be patched up before train motion can continue. Call
   -- Find_Next_Sensor_Expected and set the field directly.
   -- TODO: generalize Resume for this patchup.
   -------------------------------------------------------
   procedure Turn_Around (
                          Pos : in out Train_Position );


end Topolog2;
