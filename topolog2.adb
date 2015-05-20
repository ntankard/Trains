-- Topolog2.adb
--
-- Passive object providing block interconnection info, turnout info and
-- code to track the position of each magnet as it moves along the layout.
--
-- Based on an array model of the topology.  The non-zero width of sensors
-- is modelled, ie both edges of a sensor count.  Blocks have start and
-- end defined by the direction of travel with normal (fwd) polarity.
--
-- Author: Rob Allen, Swinburne University of Technology
-- version 2.3 10-Apr-08
-- 18-Apr-08 (v2.4) added Initialise_After, Find_Turnout_Exit
-- 22-Apr-08 debugged Resume, Next_Sensor_Along_Check, released
-- 22-Apr-08 (v2.4.1) debugged Entering/Leaving_Zone
-- 28-Apr-08 (v2.5) Find_Next_Sensor_{Along|Expected} replace one-parameter
--           Next_Sensor_{Along|Expected}, fixed 5 errors in Data; other bugs
-- 27-May-08 (v2.7) fix bug in Data(65) ie turnout 1, Data(101) ie zone64
--  9-Mar-12 (v2.8.2) made compatible with raildefs 2.5
-- 11-Feb-13 (v2.9)  separate type for Zone_Id, sideswipe zone code
--  9-Mar-13   Fix Resume from Just_Before
-- 12-May-15 (v3.0)  Adopt student improvements: extra out parameters for
--                   Check_Entering_Chained_Turnout, Check_Entering_Crossing, etc
--
with Ada.Text_Io;  -- for errors

package body Topolog2 is

--   Turnout_Being_Entered : constant  -- unused
--   array(Polarity_Type,Sensor_Id) of Turnout_Idx :=
--      (Normal_Pol=> (4|10=>2, 6|12=>3, 8|14=>4, 16|24=>5, 34=>6, 38=>8, 42=>9, 46|64=>11,
--         35=>12, 37=>13, 39|63=>15, 45|48=>16, 47=>17, 49=>19, others=>No_Turnout),
--      Reverse_Pol=>(1|18=>1, 12=>2, 14=>3, 16=>4, 26=>5, 36=>7, 40|60=>8, 44=>10,
--         37|58=>12, 38=>14, 41=>15, 47=>16, 49=>18, 51|53=>19, others=>No_Turnout) );

   type Next_Array is array (Polarity_Type) of Feature_Idx;
   type Feature_Node is
      record
         Ftr  : Feature_Descriptor;
         Next : Next_Array;
      end record;
   -- we assume Polarity_Type is defined in order Norm,Rev

   S : constant := 0;
   -- sensors are at 1..64
   T : constant := Num_Sensors; -- 64
   -- turnouts are at T+1, T+2, ... ie 65, 66, 67, ..
   D : constant := T + Num_Turnouts; -- 83
   -- dead track is D+1, D+2, ... ie 84, 85, ..
   C : constant := D + 5; -- 88
   -- crossing track at C+1, C+2, ...
   Z : constant := C + 10; -- 98
   -- boundary zone track at Z+1, Z+2, ... z+8  (4 boundary zones)
   P : constant := Z + 8; -- 106
   -- plain track at P+1, P+2,...P+24

   Data : constant
   array(Feature_Idx) of Feature_Node :=
      ( ((Dead_Track, 1),(0,0)),  -- 0 dummy

      ((Sensor, 21, 1, 0), (P+1, T+1)),  -- was T+11 (27/05/08)
      ((Sensor, 21, 2, 0), (P+2, P+1 )),

      ((Sensor,  2, 3, 0), (P+3, D+1 )),
      ((Sensor,  2, 4, 0), (T+2, P+3 )),

      ((Sensor,  3, 5, 0), (P+4, D+2 )),
      ((Sensor,  3, 6, 0), (T+3, P+4)),

      ((Sensor,  4, 7, 0), (P+5, D+3 )),
      ((Sensor,  4, 8, 0), (T+4, P+5 )),

      ((Sensor,  1, 9, 0), (0, 0 )),   -- not installed (& not blk 1)
      ((Sensor, 21, 10, 0), (T+2, P+2 )),--10
      ((Sensor,  1, 11, 0), (0, 0 )),  -- not installed (& not blk 1)
      ((Sensor,  5, 12, 0), (T+3, T+2 )), -- .Next(Reverse_Pol) assumes straight
      ((Sensor,  1, 13, 0), (0, 0 )),  -- not installed (& not blk 1)
      ((Sensor,  5, 14, 0), (T+4, T+3 )),

      ((Sensor, 19, 15, 0), (C+1, P+24 )),  -- c+1 is tram-cross-north
      ((Sensor,  5, 16, 0), (T+5, T+4 )),
      ((Sensor, 19, 17, 0), (P+6, C+1 )),
      ((Sensor, 20, 18, 0), (P+7, T+1 )),  -- (v2.5) was T+11
      ((Sensor, 19, 19, 0), (Z+1, P+6 )),
      ((Sensor, 20, 20, 0), (C+2, P+7 )), --20 -- c+2 is tram-cross-south
      ((Sensor, 19, 21, 12),(Z+2, Z+1 )),
      ((Sensor, 20, 22, 0), (P+8, C+2 )),
      ((Sensor, 12, 23, 0), (P+9, Z+2 )),
      ((Sensor, 20, 24, 0), (T+5, P+8 )),

      ((Sensor,  1, 25, 0), (P+10,D+4 )),
      ((Sensor,  5, 26, 0), (P+13,T+5 )),
      ((Sensor,  1, 27, 0), (C+4, P+10)),  -- c+4 is south tram track that crosses
      ((Sensor,  5, 28, 0), (Z+3, P+13)),
      ((Sensor,  1, 29, 0), (P+11,C+3 )),
      ((Sensor,  5, 30, 6), (Z+4, Z+3 )),--30
      ((Sensor,  1, 31, 0), (P+12,P+11)),
      ((Sensor,  6, 32, 0), (P+14,Z+4 )),
      ((Sensor,  1, 33, 0), (D+5, P+12)),

      ((Sensor,  6, 34, 0), (T+6, P+14)),
      ((Sensor, 12, 35, 0), (T+12,P+9 )),  -- todo: move this sensor
      ((Sensor,  7, 36, 0), (P+15,T+7 )),
      ((Sensor, 13, 37, 0), (T+13,T+12)),  -- todo: install another sensor
      -- for now, reserve p+16
      ((Sensor,  7, 38, 0), (T+8, P+15)),
      ((Sensor, 14, 39, 0), (T+15,T+14)),
      ((Sensor,  9, 40, 0), (P+17,T+8 )),--40
      ((Sensor, 15, 41, 0), (Z+5, T+15)),  -- (v2.5) was T+18
      ((Sensor,  9, 42, 0), (T+9, P+17)),
      ((Sensor, 15, 43,16), (Z+6, Z+5 )),
      ((Sensor, 10, 44, 0), (P+18,T+10)),  -- (v2.5) was T+9
      ((Sensor, 16, 45, 0), (T+16,Z+6 )),
      ((Sensor, 10, 46, 0), (T+11,P+18)),
      ((Sensor, 17, 47, 0), (T+17,T+16)),
      ((Sensor, 22, 48, 0), (T+16,P+21)),
      ((Sensor, 18, 49, 0), (T+19,T+18)),  -- todo: install another sensor
      -- for now, reserve p+19
      ((Sensor, 22, 50, 0), (P+21,C+5 )),--50  -- (v2.5) was P+18
         -- c+5 is main diag crossing on B22
      ((Sensor, 19, 51, 0), (P+24,T+19)),  --todo: move s51
      ((Sensor,  1, 52, 0), (0, 0 )),   -- not installed (& not blk 1)
      ((Sensor, 23, 53, 0), (P+22,T+19)),
      ((Sensor,  1, 54, 0), (0, 0 )),   -- not installed (& not blk 1)
      ((Sensor, 23, 55, 0), (C+6, P+22)), -- c+6 is main diag crossing on B23
      ((Sensor, 22, 56, 0), (C+5, P+20)),
      ((Sensor,  1, 57, 0), (0, 0 )),   -- not installed (& not blk 1)
      ((Sensor, 22, 58, 0), (P+20,T+12)),
      ((Sensor,  1, 59, 0), (0, 0 )),   -- not installed (& not blk 1)
      ((Sensor,  8, 60, 0), (Z+7, T+8 )),--60
      ((Sensor, 23, 61, 0), (P+23,C+6 )),
      ((Sensor,  8, 62,24), (Z+8, Z+7 )),
      ((Sensor, 23, 63, 0), (T+15,P+23)),
      ((Sensor, 24, 64, 0), (T+11,Z+8 )),

      ((Turnout, 11, 1, S+1), (S+18,T+11)), -- s18 follows via straight
      ((Turnout,  5, 2, S+4), (S+12,S+10)), -- s10 precedes via straight
      ((Turnout,  5, 3, S+6), (S+14,S+12)),
      ((Turnout,  5, 4, S+8), (S+16,S+14)),
      ((Turnout,  5, 5, S+24), (S+26,S+16)),
      ((Turnout,  6, 6, C+8), (T+7, S+34)),--70
      ((Turnout,  7, 7, C+7), (S+36,T+6 )),
      ((Turnout,  7, 8, S+40), (S+60,S+38)),
      ((Turnout,  9, 9, C+9), (T+10,S+42)),
      ((Turnout, 10,10, C+10), (S+44,T+9 )),
      ((Turnout, 11,11, S+64), (T+1, S+46)),
      ((Turnout, 12,12, S+58), (S+37,S+35)),    -- todo: move s35
      ((Turnout, 13,13, C+7 ), (T+14,S+37)),
      ((Turnout, 14,14, C+8 ), (S+39,T+13)),
      ((Turnout, 15,15, S+63), (S+41,S+39)),
      ((Turnout, 16,16, S+48), (S+47,S+45)),--80
      ((Turnout, 17,17, C+10), (T+18,S+47)),
      ((Turnout, 18,18, C+9 ), (S+49,T+17)),
      ((Turnout, 19,19, S+53), (S+51,S+49)),   -- todo: install another sensor

      ((Dead_Track, 2), (S+3, 0 )),  --84
      ((Dead_Track, 3), (S+5, 0 )),
      ((Dead_Track, 4), (S+7, 0 )),
      ((Dead_Track, 1), (S+25,0 )),   -- todo: move s25
      ((Dead_Track, 1), (0, S+33)),

      ((Crossing_Track, 19, North_Tram_Cross ), (S+17, S+15 )),  -- 89
      ((Crossing_Track, 20, South_Tram_Cross), (S+22, S+20 )),
      ((Crossing_Track,  1, North_Tram_Cross), (S+29, C+4 )),   -- tram track (North)
      ((Crossing_Track,  1, South_Tram_Cross), (C+3, S+27 )),   -- new v3.0 (South)
      ((Crossing_Track, 22, Main_Diagonal), (S+50, S+56 )),     -- main diag to SW
      ((Crossing_Track, 23, Main_Diagonal), (S+61, S+55 )),     -- main diag to NW
      ((Crossing_Track, 13, North_Crossover), (T+7, T+13 )),    -- new v3.0 (NW/SE)
      ((Crossing_Track, 14, North_Crossover), (T+14, T+6 )),    -- new v3.0 (NE/SW)
      ((Crossing_Track, 18, South_Crossover), (T+18, T+9 )),    -- new v3.0 (NE/SW)
      ((Crossing_Track, 17, South_Crossover), (T+10, T+17 )),   -- new v3.0 (NW/SE)


      ((Zone, 19, 1, 21), (S+21, S+19 )),      -- z1s 99 (explicit ids)
      ((Zone, 12, 1, 21), (S+23, S+21 )),      -- z1n 100
      ((Zone,  5, 2, 30), (S+30, S+28 )),      -- z2e
      ((Zone,  6, 2, 30), (S+32, S+30 )),      -- z2w
      ((Zone, 15, 3, 43), (S+43, S+41 )),      -- z3n qtr circle
      ((Zone, 16, 3, 43), (S+45, S+43 )),      -- z3s
      ((Zone,  8, 4, 62), (S+62, S+60 )),      -- z4n most B8
      ((Zone, 24, 4, 62), (S+64, S+62 )),      -- z4e most B24 12/02/13

      ((Plain_Track, 21), (S+2, S+1 )),      -- p1   107
      ((Plain_Track, 21), (S+10,S+2 )),      -- p2
      ((Plain_Track,  2), (S+4, S+3 )),      -- p3
      ((Plain_Track,  3), (S+6, S+5 )),      -- p4  110
      ((Plain_Track,  4), (S+8, S+7 )),      -- p5

      ((Plain_Track, 19), (S+19,S+17)),      -- p6
      ((Plain_Track, 20), (S+20,S+18)),      -- p7
      ((Plain_Track, 20), (S+24,S+22)),      -- p8
      ((Plain_Track, 12), (S+35,S+23)),      -- p9   todo: move s35
      ((Plain_Track,  1), (S+27,S+25)),      -- p10  todo: move s25
      ((Plain_Track,  1), (S+31,S+29)),      -- p11
      ((Plain_Track,  1), (S+33,S+31)),      -- p12

      ((Plain_Track,  5), (S+28,S+26)),      -- p13
      ((Plain_Track,  6), (S+34,S+32)),      -- p14  120
      ((Plain_Track,  7), (S+38,S+36)),      -- p15
      ((Plain_Track, 13), (0,0)),            -- p16 todo
      ((Plain_Track,  9), (S+42,S+40)),      -- p17
      ((Plain_Track, 10), (S+46,S+44)),      -- p18
      ((Plain_Track, 18), (0,0)),            -- p19 todo
      ((Plain_Track, 22), (S+56,S+58)),      -- p20
      ((Plain_Track, 22), (S+48,S+50)),      -- p21
      ((Plain_Track, 23), (S+55,S+53)),      -- p22 (v2.5) was S+56,
      ((Plain_Track, 23), (S+63,S+61)),      -- p23
      ((Plain_Track, 19), (S+15,S+51)),      -- p24  todo: move s51
      ((Plain_Track,  1), (0,0))             -- p25 rfe  131
      );

   -------- FORWARD DECLARATIONS ----------------
   --------------------------------
   -- Next_Pos_Check (internal)
   --------------------------------
   procedure Next_Pos_Check (
         Pos          : in out Train_Position;
         Setting      : in     Turnout_Pos;
         Need_Setting :    out Boolean;
         For_Turnout  :    out Turnout_Id );

   ----------------------------------------
   -- Next_Sensor_Along_Check (internal) --
   ----------------------------------------
   procedure Next_Sensor_Along_Check (
         Pos          : in     Train_Position;
         Setting      : in     Turnout_Pos;  -- maybe Middle
         The_Sensor   :    out Sensor_Idx;   -- No_Sensor if none or Need_Setting
         Need_Setting :    out Boolean;
         For_Turnout  :    out Turnout_Id );


   procedure Debug(S : String) is
   begin
      null; --Ada.Text_Io.Put_Line("debug " & S);
   end;
   -------- SUBPROGRAMS (ALPHABETICAL ORDER) -----------

   -----------------
   -- Advance_Pos --
   -----------------

   procedure Advance_Pos (
         Pos          : in out Train_Position;
         Stopping     : in     Boolean;
         Need_Setting :    out Boolean;
         For_Turnout  :    out Turnout_Id
       ) is
   --
      Result : Train_Position := Pos;
   begin
      if Pos.Substate = Just_Before then  --(v2.9)
         -- assume rear
         Pos.Substate := Norm;
         Need_Setting := False;
         For_Turnout  := 1;
      elsif Data(Pos.Within).Ftr.Kind = Sensor and Stopping then
         -- assume front pos
         Pos.Substate := Just_After;
         Need_Setting := False;
         For_Turnout  := 1;
      else
         Next_Pos_Check(Result, Middle, Need_Setting, For_Turnout);
         if not Need_Setting then
            Pos := Result;
         end if;
      end if;
   end Advance_Pos;

   -----------------
   -- Advance_Pos --
   -----------------

   procedure Advance_Pos (
         Pos      : in out Train_Position;
         Stopping : in     Boolean;
         Setting  : in     Turnout_Pos
       ) is
   --
      Result : Train_Position := Pos;
      Need_Setting : Boolean;
      For_Turnout  : Turnout_Id;
   begin
      if Pos.Substate = Just_Before then  --(v2.9)
         Pos.Substate := Norm;
      elsif Data(Pos.Within).Ftr.Kind = Sensor and then
            Stopping then
         Result.Substate := Just_After;
      else
         Next_Pos_Check(Result, Setting, Need_Setting, For_Turnout);
         if Need_Setting then
            raise Missing_Turnout_Info;
         end if;
      end if;
      Pos := Result;
   end Advance_Pos;


   ------------------------------------
   -- Check_Entering_Chained_Turnout --
   ------------------------------------

   procedure Check_Entering_Chained_Turnout (
         Pos               : in     Train_Position;
         Setting1          : in     Turnout_Pos;
         Entering2         :    out Boolean;
         Which2            :    out Turnout_Idx;
         Converging2       :    out Boolean;
         Required_Setting2 :    out Turnout_Pos;
         Entering_Crossing :    out Boolean;
         Which_Crossing    :    out Crossing_Idx
                                            ) is
   --
      Converging : Boolean := False;
      --Chained    : Chain_Type;
      Pred,                   -- index of how got into 2nd turnout
      Nxt,                    -- index of the 1st turnout
      Nxt2,                   -- index of the 2nd turnout
      Nxt_Tu : Feature_Idx;   -- attribute of 1st turnout
      Polr   : Polarity_Type; -- current polarity
      Which  : Turnout_Idx;   -- 1st turnout
   begin
      Entering2 := False;
      Which2 := No_Turnout;
      Converging2 := False;
      Required_Setting2 := Middle;
      Entering_Crossing := False;
      Which_Crossing := No_Crossing;
      if Data(Pos.Within).Ftr.Kind /= Sensor then
         return;
      end if;
      Nxt := Data(Pos.Within).Next(Pos.Polr);
      if Data(Nxt).Ftr.Kind /= Turnout then
         return;
      end if;
      Which := Data(Nxt).Ftr.Tid;
      Polr := Pos.Polr;  -- copy in case need to invert it
      Nxt_Tu := Data(Nxt).Ftr.Next_Tu;
      if Nxt_Tu = Pos.Within then
         -- entering via turned branch
         Converging := True;
         if Setting1 /= Turned then
            Ada.Text_Io.Put_Line("ERROR: Check_Entering_Chained_Turnout: Setting1=Turned required");
            raise Illegal_Turnout_Info;
         end if;
         -- check for inverting join:
         if Turnout_Data(Which).Swap_Pol_Tu then
            Polr := Opposite(Polr);
         end if;
      else
         -- entering via straight or centre
         -- assert: no polarity inversion here
         if (Polr = Reverse_Pol) =
               Turnout_Data(Which).Diverging_When_Normal then
            Converging := True;
            if Setting1 /= Straight then
               Ada.Text_Io.Put_Line("ERROR: Check_Entering_Chained_Turnout: Setting1=Straight required");
               raise Illegal_Turnout_Info;
            end if;
         end if;
      end if;
      -- Now double check for chaining.
      if not Converging and Setting1 = Turned then
         Nxt2 := Nxt_Tu;
         -- check for inverting join:
         if Turnout_Data(Which).Swap_Pol_Tu then
            Polr := Opposite(Polr);
         end if;
      elsif Setting1 = Middle then
         Ada.Text_Io.Put_Line("ERROR: Check_Entering_Chained_Turnout: Setting1=Middle illegal");
         raise Illegal_Turnout_Info;
      else
         Nxt2 := Data(Nxt).Next(Polr);
      end if;
      -- now Nxt, Which refer to the first turnout
      -- but nxt2 refers to the next feature, and Polr is the actual polarity on it
      Pred := Nxt;
      if Data(Nxt2).Ftr.Kind = Crossing_Track then
         Entering_Crossing := True;
         Which_Crossing := Data(Nxt2).Ftr.Kid;
         -- crossing immediate : skip over crossing
         Pred := Nxt2;
         Nxt2 := Data(Nxt2).Next(Polr);
      elsif Data(Nxt2).Ftr.Kind = Sensor then
         -- sensored: skip over it
         Pred := Nxt2;
         Nxt2 := Data(Nxt2).Next(Polr); -- could be straight or centre
         -- (in no place in 2008 layout do we need turned 'pointer' here)
      end if;

      -- pred denotes the feature via which we reach the second turnout,
      -- immnediate => first turnout
      -- sensored => the sensor
      if Data(Nxt2).Ftr.Kind = Turnout then
         -- it is actually chained, we dont care what kind
         -- nxt2 is on the second turnout
         Entering2 := True;
         Which2 := Data(Nxt2).Ftr.Tid;
         if (Polr = Reverse_Pol) =
               Turnout_Data(Which2).Diverging_When_Normal then
            Converging2 := True;
            if Data(Nxt2).Ftr.Next_Tu = Pred then
               Required_Setting2 := Turned;
            else
               Required_Setting2 := Straight;
            end if;
         end if;
      --else
      -- it wasnt chained for the provided first setting
      end if;
   end Check_Entering_Chained_Turnout;

   -----------------------------
   -- Check_Entering_Crossing --
   -----------------------------

   procedure Check_Entering_Crossing (
         Pos      : in     Train_Position;
         Entering :    out Boolean;
         Which    :    out Crossing_Idx;
         Next     :    out Crossing_Idx   ) is
   --
      Nxt : Feature_Idx;
   begin
      Entering := False;
      Which := No_Crossing;
      Next := No_Crossing;
      if Data(Pos.Within).Ftr.Kind = Sensor then
         Nxt := Data(Pos.Within).Next(Pos.Polr);
                debug("entering_crossing 1 " & Nxt'img);
         if Data(Nxt).Ftr.Kind = Crossing_Track then
            Entering := True;
                debug("entering_crossing 2");
            Which := Data(Nxt).Ftr.Kid;
            Nxt := Data(Nxt).Next(Pos.Polr);
            if Data(Nxt).Ftr.Kind = Crossing_Track then
               Next := Data(Nxt).Ftr.Kid;
            end if;
         end if;
      end if;
   end Check_Entering_Crossing;

   ----------------------------
   -- Check_Entering_Turnout --
   ----------------------------

   procedure Check_Entering_Turnout (
         Pos              : in     Train_Position;
         Entering         :    out Boolean;
         Which            :    out Turnout_Idx;
         Converging       :    out Boolean;
         Required_Setting :    out Turnout_Pos;
         Chained          :    out Chain_Type      ) is
      Nxt,
      Nxt2 : Feature_Idx;
      Polr   : Polarity_Type;
   begin
      Entering := False;
      Which := No_Turnout;
      Converging := False;
      Required_Setting := Middle;
      Chained := No;
      if Data(Pos.Within).Ftr.Kind = Sensor then
         Nxt := Data(Pos.Within).Next(Pos.Polr);
         if Data(Nxt).Ftr.Kind = Turnout then
            Entering := True;
            Which := Data(Nxt).Ftr.Tid;
            Polr := Pos.Polr;
            if Data(Nxt).Ftr.Next_Tu = Pos.Within then
               -- entering via turned branch
               Converging := True;
               Required_Setting := Turned;
               -- check for inverting join:
               if Turnout_Data(Which).Swap_Pol_Tu then
                  Polr := Opposite(Polr);
               end if;
            else
               -- entering via straight or centre
               -- assert: no polarity inversion here
               if (Polr = Reverse_Pol) =
                     Turnout_Data(Which).Diverging_When_Normal then
                  Converging := True;
                  Required_Setting := Straight;
               end if;
            end if;
            -- Now check for chaining.  Note following the straight or
            -- centre branch is enough to discover kind of chaining
            -- in all cases of the 2008 layout
            Nxt2 := Data(Nxt).Next(Polr); -- could be straight or centre
            if Data(Nxt2).Ftr.Kind = Turnout then
               Chained := Immediate;
            elsif Data(Nxt2).Ftr.Kind = Sensor then
               Nxt2 := Data(Nxt2).Next(Polr);
               if Data(Nxt2).Ftr.Kind = Turnout then
                  Chained := Sensored;
               end if;
            end if;
         end if;
      end if;
   end Check_Entering_Turnout;

   -------------------------
   -- Check_Entering_Zone --
   -------------------------

   procedure Check_Entering_Zone (
         Pos      : in     Train_Position;
         Entering :    out Boolean;
         Which    :    out Zone_Idx         ) is
   --
      Nxt : Feature_Idx;
   begin
      Entering := False;
      Which := No_Zone;
      if Data(Pos.Within).Ftr.Kind = Sensor then
         Nxt := Data(Pos.Within).Next(Pos.Polr);
         if Data(Nxt).Ftr.Kind = Zone and then
            Data(Pos.Within).Ftr.Sid /= Data(Nxt).Ftr.Centre then
            Entering := True;
            Which := Data(Nxt).Ftr.Zid;
         end if;
      end if;
   end Check_Entering_Zone;

   -----------------------------------
   -- Check_Entering_Sideswipe_Zone --
   -----------------------------------

   procedure Check_Entering_Sideswipe_Zone (   --(v2.9)
         Pos      : in     Train_Position;
         Turnout  : in     Turnout_Id;
         Setting  : in     Turnout_Pos;
         Entering :    out Boolean;
         Which    :    out Zone_Idx       ) is
   --
      Sens : Sensor_Idx := Pos.Next_Expected;  -- assume on it
   begin
      Entering := False;
      Which := No_Zone;
      if (Turnout = 15 and Sens /= 39) or else
         (Turnout = 6 and Setting = Turned) or else
         (Turnout = 14 and Setting = Turned) or else  --v3.0 but should be irrel
         (Turnout = 13 and Setting = Straight) then
         Entering := True;
         Which := 5;
      elsif (Turnout = 16 and Sens /= 47) or else
         (Turnout = 10 and Setting = Turned) or else
         (Turnout = 17 and Setting = Turned) or else  --v3.0 but should be irrel
         (Turnout = 18 and Setting = Straight) then
         Entering := True;
         Which := 6;
      end if;
   end Check_Entering_Sideswipe_Zone;

   ----------------------------
   -- Check_Leaving_Crossing --
   ----------------------------

   procedure Check_Leaving_Crossing (
         Pos     : in     Train_Position;
         Leaving :    out Boolean;
         Which   :    out Crossing_Idx;
         Next    :    out Crossing_Idx) is
   --
      Pred : Feature_Idx;
   begin
      Leaving := False;
      Which := No_Crossing;
      Next := No_Crossing;
      -- this code is like Check_Entering_Crossing
      -- but with opposite polarity, ie going backward
      if Data(Pos.Within).Ftr.Kind = Sensor then
                --debug("leaving_crossing 1");
         Pred:= Data(Pos.Within).Next(Opposite(Pos.Polr));
                --debug("leaving_crossing 2" & Pred'img);
                --debug("leaving_crossing 2" & Data(Pred).Ftr.Kind'img);
         if Data(Pred).Ftr.Kind = Crossing_Track then
            Leaving := True;
                --debug("leaving_crossing 3" & Leaving'img);
            Which := Data(Pred).Ftr.Kid;
                --debug("leaving_crossing 4" & Which'img);
            Pred:= Data(Pred).Next(Opposite(Pos.Polr));
            if Data(Pred).Ftr.Kind = Crossing_Track then
               Next := Data(Pred).Ftr.Kid;
            end if;
         end if;
      end if;
   end Check_Leaving_Crossing;

   -----------------------------------
   -- Check_Leaving_Chained_Turnout --
   -----------------------------------

   procedure Check_Leaving_Chained_Turnout (
         Pos         : in     Train_Position;
         Setting2    : in     Turnout_Pos;
         Leaving1    :    out Boolean;
         Which1      :    out Turnout_Idx;
         Converging1 :    out Boolean;
         Leaving_Crossing :    out Boolean;
         Which_Crossing   :    out Crossing_Idx ) is
   --
      Converging : Boolean := True;
      Nxtb2,                  -- index of the 2nd turnout
      Nxtb1,                  -- index of the 1st turnout
      Nxt_Tu : Feature_Idx;   -- attribute of 2nd turnout
      Polr   : Polarity_Type; -- backward polarity
      Which2  : Turnout_Idx;   -- 2nd turnout (same as found in
         -- preceding Check_Leaving_Turnout call)
   begin
      Leaving1 := False;
      Which1 := No_Turnout;
      Converging1 := False;
      Leaving_Crossing := False;
      Which_Crossing := No_Crossing;
      -- the following code is like Check_Entering_Chained_Turnout
      -- but going backward
      if Data(Pos.Within).Ftr.Kind /= Sensor then
         return;
      end if;
      Polr := Opposite(Pos.Polr);
      Nxtb2 := Data(Pos.Within).Next(Polr);
      if Data(Nxtb2).Ftr.Kind /= Turnout then
         return;
      end if;
      Which2 := Data(Nxtb2).Ftr.Tid;
      Nxt_Tu := Data(Nxtb2).Ftr.Next_Tu;
      if Nxt_Tu = Pos.Within then
         -- leaving via turned branch
         Converging := False;
         if Setting2 /= Turned then
            Ada.Text_Io.Put_Line("ERROR: Check_Leaving_Chained_Turnout: Setting1=Turned required");
            raise Illegal_Turnout_Info;
         end if;
         -- check for inverting join:
         if Turnout_Data(Which2).Swap_Pol_Tu then
            Polr := Opposite(Polr);
         end if;
      else
         -- leaving via straight or centre
         -- assert: no polarity inversion here
         if (Polr = Reverse_Pol) =
               Turnout_Data(Which2).Diverging_When_Normal then
            Converging := False;
            if Setting2 /= Straight then
               Ada.Text_Io.Put_Line("ERROR: Check_Leaving_Chained_Turnout: Setting1=Straight required");
               raise Illegal_Turnout_Info;
            end if;
         end if;
      end if;
      -- Now double check for chaining.
      if Converging and Setting2 = Turned then
         -- diverging going backward
         Nxtb1 := Nxt_Tu;
         -- check for inverting join:
         if Turnout_Data(Which2).Swap_Pol_Tu then
            Polr := Opposite(Polr);
         end if;
         -- now Nxtb1 is the feature before the turned branch of the
         -- 2nd turnout
      elsif Setting2 = Middle then
         Ada.Text_Io.Put_Line("ERROR: Check_Entering_Chained_Turnout: Setting1=Middle illegal");
         raise Illegal_Turnout_Info;
      else
         Nxtb1 := Data(Nxtb2).Next(Polr);
         -- the feature before the middle or straight branch
      end if;
      -- we are only interested in it if it's immediately a turnout
      -- but must check for crossing track
      if Data(Nxtb1).Ftr.Kind = Crossing_Track then
         Leaving_Crossing := True;
         Which_Crossing := Data(Nxtb1).Ftr.Kid;
         --Immediate with crossing: Skip over the crossing
         Nxtb1 := Data(Nxtb1).Next(Polr);
      end if;

      -- Pred := Nxtb2;
      -- pred denotes the feature via which we reach the first turnout
      -- going backward - here it's the 2nd turnout (no sensor)
      if Data(Nxtb1).Ftr.Kind = Turnout then
         -- it is actually chained, we dont care what kind
         Leaving1 := True;
         Which1 := Data(Nxtb1).Ftr.Tid;
         if (Polr = Reverse_Pol) =
               Turnout_Data(Which1).Diverging_When_Normal then
            Converging1 := False;
         else
            Converging1 := True;
         end if;
      --else
      -- it wasnt immediate chained for the provided 2nd setting
      end if;
   end Check_Leaving_Chained_Turnout;

   ---------------------------
   -- Check_Leaving_Turnout --
   ---------------------------

   procedure Check_Leaving_Turnout (
         Pos        : in     Train_Position;
         Leaving    :    out Boolean;
         Which      :    out Turnout_Idx;
         Converging :    out Boolean;
         Chained    :    out Chain_Type      ) is
      Nxt,
      Nxt2 : Feature_Idx;
      Polr   : Polarity_Type;
   begin
      Leaving := False;
      Which := No_Turnout;
      Converging := False;
      Chained := No;
      -- the following code is similar to that for Check_Entering_Turnout
      -- but with opposite polarity, ie going backward
      if Data(Pos.Within).Ftr.Kind = Sensor then
         Polr := Opposite(Pos.Polr);

         Nxt := Data(Pos.Within).Next(Polr);
         if Data(Nxt).Ftr.Kind = Turnout then
            Leaving := True;
            Which := Data(Nxt).Ftr.Tid;
            Converging := True;
            if Data(Nxt).Ftr.Next_Tu = Pos.Within then
               -- left via turned branch
               Converging := False;
               -- check for inverting join:
               if Turnout_Data(Which).Swap_Pol_Tu then
                  Polr := Opposite(Polr);
               end if;
            else
               -- leaving via straight or centre
               -- assert: no polarity inversion here
               if (Polr = Reverse_Pol) =
                     Turnout_Data(Which).Diverging_When_Normal then
                  Converging := False;
               end if;
            end if;
            -- Now check for chaining.  Note following the straight or
            -- centre branch is enough to discover kind of chaining
            -- in all cases of the 2008 layout
            Nxt2 := Data(Nxt).Next(Polr); -- could be straight or centre
            if Data(Nxt2).Ftr.Kind = Turnout then
               Chained := Immediate;
            end if;
         end if;
      end if;

   end Check_Leaving_Turnout;

   ------------------------
   -- Check_Leaving_Zone --
   ------------------------

   procedure Check_Leaving_Zone (
         Pos     : in     Train_Position;
         Leaving :    out Boolean;
         Which   :    out Zone_Idx        ) is
   --
      Pred   : Feature_Idx;
   begin
      Leaving := False;
      Which := No_Zone;
      -- this code is like Check_Entering_Zone
      -- but with opposite polarity, ie going backward
      if Data(Pos.Within).Ftr.Kind = Sensor then
         Pred:= Data(Pos.Within).Next(Opposite(Pos.Polr));
         if Data(Pred).Ftr.Kind = Zone and then
            Data(Pos.Within).Ftr.Sid /= Data(Pred).Ftr.Centre then
            Leaving := True;
            Which := Data(Pred).Ftr.Zid;
         end if;
      end if;
   end Check_Leaving_Zone;

   -----------------------------------
   -- Check_Leaving_Sideswipe_Zone  --
   -----------------------------------
   procedure Check_Leaving_Sideswipe_Zone (   --(v2.9)
         Pos      : in     Train_Position;
         Turnout  : in     Turnout_Id;
         Setting  : in     Turnout_Pos;
         Leaving  :    out Boolean;
         Which    :    out Zone_Idx       ) is
   --
   begin
      Check_Entering_Sideswipe_Zone(Pos, Turnout, Setting, Leaving, Which);
      -- it is simply the same code as entering except for the param name!
   end Check_Leaving_Sideswipe_Zone;

   -----------------------------
   -- Check_Turnout_Info_Need --
   -----------------------------

   procedure Check_Turnout_Info_Need (
         Pos         : in     Train_Position;
         Needed      :    out Boolean;
         For_Turnout :    out Turnout_Id      ) is
   --
      Copy : Train_Position := Pos;
   begin
      Next_Pos_Check(Copy, Middle, Needed, For_Turnout);
   end Check_Turnout_Info_Need;

   ----------------
   -- Feature_At --
   ----------------

   function Feature_At (
         Pos : Train_Position )
     return Feature_Descriptor is
   begin
      return Data(Pos.Within).Ftr;
   end Feature_At;

   ---------------------
   -- Feature_Kind_At --
   ---------------------

   function Feature_Kind_At (
         Pos : Train_Position )
     return Feature_Kind is
   begin
      return Data(Pos.Within).Ftr.Kind;
   end Feature_Kind_At;

   ----------------------------
   -- Find_Next_Sensor_Along -- (v2.5)
   ----------------------------

   procedure Find_Next_Sensor_Along (
      Pos          : in     Train_Position;
      The_Sensor   :    out Sensor_Idx;
      Need_Setting :    out Boolean;
      For_Turnout  :    out Turnout_Id    ) is
   --
   begin
      Next_Sensor_Along_Check(Pos, Middle,
         The_Sensor, Need_Setting, For_Turnout);
   end Find_Next_Sensor_Along;


   -------------------------------
   -- Find_Next_Sensor_Expected -- (v2.5)
   -------------------------------

   procedure Find_Next_Sensor_Expected(
      Pos          : in     Train_Position;
      The_Sensor   :    out Sensor_Idx;
      Need_Setting :    out Boolean;
      For_Turnout  :    out Turnout_Id    ) is
   --
   begin
      if Feature_Kind_At(Pos) = Sensor then
         The_Sensor := Data(Pos.Within).Ftr.Sid;
         -- this includes all substate values.
         Need_Setting := False;
         For_Turnout := 1;
      else
         Next_Sensor_Along_Check(Pos, Middle,
            The_Sensor, Need_Setting, For_Turnout);
      end if;
   end Find_Next_Sensor_Expected;


   ----------------
   -- Initialise_After --
   ----------------

   function Initialise_After (The_Sensor : Sensor_Id)
         return Train_Position is
      Result : Train_Position;
      Nxt    : Feature_Idx;
   begin
      Result.Within := Feature_Idx(The_Sensor);
      if Data(Result.Within).Next = (0,0) then
         -- not installed
         raise Illegal_Position;
      end if;
               Debug("Initialise_After 1");
      Result.Block_Num := Data(Result.Within).Ftr.Block_Num;
      Result.Polr := Normal_Pol;
      Result.Substate := Norm;
      Result.Next_Expected := The_Sensor;
      Nxt := Data(Result.Within).Next(Normal_Pol);
      if Data(Nxt).Ftr.Kind = Turnout then
         -- entering turnout
         raise Illegal_Position;
      end if;
               Debug("Initialise_After 2");
      Advance_Pos(Result, False, Middle);
               Debug("Initialise_After 3");
      if Result.Next_Expected = No_Sensor
          and Feature_Kind_At(Result) /= Dead_Track then
         Ada.Text_Io.Put_Line("ERROR: Initialise_After logic failed");
         raise Topology_Error;
      end if;
      return Result;
   end Initialise_After;

   ------------------------------
   -- Is_Leaving_Powered_Track --
   ------------------------------

   function Is_Leaving_Powered_Track (
         Pos     :  Train_Position  )
     return Boolean is
   --
      Nxt : Feature_Idx;
   begin
      if Data(Pos.Within).Ftr.Kind = Sensor then
         Nxt := Data(Pos.Within).Next(Pos.Polr);
         if Data(Nxt).Ftr.Kind = Dead_Track then
            return True;
         end if;
      end if;
      return False;
   end Is_Leaving_Powered_Track;

   ------------------------
   -- Is_Sensor_Ahead_Of -- not implemented
   ------------------------

--   function Is_Sensor_Ahead_Of (
--         Pos        : Train_Position;
--         The_Sensor : Sensor_Id       )
--     return Boolean is
--   begin
--      return False;  -- todo
--   end Is_Sensor_Ahead_Of;

   ------------------
   -- Is_Sensor_At --
   ------------------

   function Is_Sensor_At (
         Pos        : Train_Position;
         The_Sensor : Sensor_Id       )
     return Boolean is
   begin
      return Data(Pos.Within).Ftr.Kind = Sensor and then
         Data(Pos.Within).Ftr.Sid = The_Sensor;
   end Is_Sensor_At;


   --------------------------------
   -- Next_Pos_Check (internal)
   -- raises Illegal_Turnout_Info if Setting is wrong
   -- for converging turnout.
   -- Prints warning if Pos.Substate /= Norm and adjusts it to Norm
   --------------------------------

   procedure Next_Pos_Check (
      Pos           : in out Train_Position;
      Setting       : in     Turnout_Pos;  -- maybe Middle
      Need_Setting  :    out Boolean;  -- true if Middle unsatisfactory
      -- (for either turnout when immediate chained)
      For_Turnout   :    out Turnout_Id
     ) is
   --

      procedure Find_Turnout_Exit(
         Node  : in Feature_Node;  -- assumed a turnout
         Pol1   : in Polarity_Type;
         Setting : in     Turnout_Pos;  -- maybe Middle
         Need_Setting :    out Boolean;  -- true if Middle unsatisfactory
         Nxt    : out Feature_Idx;
         Polr   : out Polarity_Type ) is
      --
         Which  : Turnout_Idx;
      begin
         if Node.Ftr.Kind /= Turnout then
            Ada.Text_Io.Put_Line("ERROR: Find_Turnout_Exit precondition");
            raise Topology_Error;
         end if;
         Polr := Pol1;
         Need_Setting := False;

         Which := Node.Ftr.Tid;
         if (Polr = Normal_Pol) =
               Turnout_Data(Which).Diverging_When_Normal then
            -- diverging
               Debug("find 2");
            case Setting is
               when Middle =>
                  Need_Setting := True;
                  return;
               when Straight =>
                  -- assert: polr is correct
                  Nxt := Node.Next(Polr);
               when Turned =>
                  Nxt := Node.Ftr.Next_Tu;
                  if Turnout_Data(Which).Swap_Pol_Tu then
                     -- leaving via turned branch with inversion
                     Polr := Opposite(Polr);
                  end if;
            end case;
         else
            -- converging, Next is ok, ignore Setting
            Nxt := Node.Next(Polr);
         end if;
      end Find_Turnout_Exit;

      Node1,
      Node2  : Feature_Node;
      Nxt,
      Nxt2    : Feature_Idx;
      Polr   : Polarity_Type := Pos.Polr;
      Polr2  : Polarity_Type;
      Pred   : Feature_Idx := Pos.Within;
      Was_Sensor,
      Was_Turnout : Boolean := False;
      Which  : Turnout_Idx;
   begin
      Need_Setting := False;
      For_Turnout := 1;
--      if Pos.Substate = Just_After then
--         Ada.Text_Io.Put_Line(
--            "WARNING: missing Resume before Next_Pos, Within was"
--            & Pos.Within'Img);
--         Pos.Substate := Norm;
--      end if;
               Debug("advance 1");
      Node1 := Data(Pos.Within);
      Nxt := Node1.Next(Polr);
      if Node1.Ftr.Kind = Sensor then
         Was_Sensor := True;
         if Pos.Substate = Just_Before then
            Nxt := Pos.Within;
            Pos.Substate := Norm;
         end if;
      elsif Node1.Ftr.Kind = Turnout then
               Debug("advance 2");
         Was_Turnout := True;

         Find_Turnout_Exit(Node1, Polr, Setting, Need_Setting, Nxt, Polr);
         if Need_Setting then
            For_Turnout := Node1.Ftr.Tid;
            return;
         end if;
         Pos.Polr := Polr;
         -- if there is a crossing next, skip over it now
         if Data(Nxt).Ftr.Kind = Crossing_Track then
            Nxt := Data(Nxt).Next(Polr);
         end if;

      end if; -- leaving a turnout
      -- assert: Nxt is the next along ok no matter what we're leaving
      -- note: Pred remembers Pos.Within
               Debug("advance 3");
      Pos.Within := Nxt;  -- simple case
      Node2 := Data(Nxt);
      case Node2.Ftr.Kind is
         when Turnout =>
            -- entering a turnout:
            Which := Node2.Ftr.Tid;
               Debug("advance 4" & which'img);
            if Node2.Ftr.Next_Tu = Pred then
               -- converging, ie entering via turned branch
               -- check for inverting join:
               if Turnout_Data(Which).Swap_Pol_Tu then
                  Polr := Opposite(Polr);
                  Pos.Polr := Polr;
               end if;
            end if;
               Debug("advance 5");
            -- If we came from a turnout then it must have been the first
            -- of an immediate chained pair and we must have turned around
            -- because that is abnormal.  Action: proceed to the next sensor.
            --
            -- If we came from a sensor AND this is a double turnout then
            -- we must end up on the centre of the second, but must find
            -- the next sensor anyway!

            Find_Turnout_Exit(Node2, Polr, Setting, Need_Setting, Nxt2, Polr2);

            if Was_Turnout then
               -- immediate chained, move through second turnout to sensor:
               -- note: we havent used Setting twice as there are no
               -- chained immediate diverging pairs of turnout
               if Need_Setting then
                  For_Turnout := Node2.Ftr.Tid;
                  return;
               end if;
               -- assert: Nxt2 is a sensor
               Node2 := Data(Nxt2);
               if Node2.Ftr.Kind /= Sensor then
                  Ada.Text_Io.Put_Line("ERROR: Next_Pos_Check logic failed");
                  raise Topology_Error;
               end if;
               Pos.Within := Nxt2;
               Pos.Next_Expected := Node2.Ftr.Sid;
               Pos.Block_Num := Node2.Ftr.Block_Num;
            elsif Was_Sensor then
               if Need_Setting then
                  For_Turnout := Node2.Ftr.Tid;
                  return;
               end if;
               -- it might be chained
               -- check first for crossing in the middle
               if Data(Nxt2).Ftr.Kind = Crossing_Track then
                  Nxt2 := Data(Nxt2).Next(Polr2);
               end if;
               if Data(Nxt2).Ftr.Kind = Turnout then
                  -- yes, immediate chained
                  Polr := Polr2;
                  Pos.Polr := Polr2;
                  Pos.Within := Nxt2;
                  Node2 := Data(Nxt2);
                  Which := Node2.Ftr.Tid;
                  Pos.Block_Num := Node2.Ftr.Block_Num;
                  -- look further for next sensor
                     Debug("advance 6" & which'img);
                  Find_Turnout_Exit(Node2, Polr, Setting, Need_Setting, Nxt2, Polr2);
                  if Need_Setting then
                     For_Turnout := Node2.Ftr.Tid;
                     return;
                  end if;
               else
                  Pos.Block_Num := Node2.Ftr.Block_Num;
                  Pos.Polr := Polr;
                  Pos.Within := Nxt;
               end if;
               if Data(Nxt2).Ftr.Kind = Sensor then
                  Pos.Next_Expected := Data(Nxt2).Ftr.Sid;
               else
                  Ada.Text_Io.Put_Line("ERROR: Next_Pos_Check logic failed");
                  raise Topology_Error;
               end if;
            end if;

          when Sensor =>
               Debug("advance 7");
            Pos.Substate := Norm;
            Pos.Next_Expected := Node2.Ftr.Sid;
            if not Is_Boundary_Sensor(Node2.Ftr.Sid) then
               -- dont adjust block_num for a sensor yet if it
               -- is a boundary sensor
               Pos.Block_Num := Node2.Ftr.Block_Num;
            end if;

         when Dead_Track =>
               Debug("advance 8");
            Pos.Block_Num := Node2.Ftr.Block_Num;
            Pos.Next_Expected := No_Sensor;

         when Crossing_Track =>
               Debug("advance 9");
            Pos.Block_Num := Node2.Ftr.Block_Num;
            -- assert: mext node is next sensor along:
            -- no longer true
            Nxt2 := Node2.Next(Polr);
            if Data(Nxt2).Ftr.Kind = Crossing_Track then
                Pos.Within := Nxt2;
                Nxt2 := Data(Nxt2).Next(Polr);
            end if;
            Pos.Next_Expected := Data(Nxt2).Ftr.Sid;

         when others =>
               Debug("advance 10");
            Pos.Block_Num := Node2.Ftr.Block_Num;
            -- assert: mext node is next sensor along:
            Nxt2 := Node2.Next(Polr);
            Pos.Next_Expected := Data(Nxt2).Ftr.Sid;

      end case;
               Debug("advance 11");
   end Next_Pos_Check;


   ----------------------------------------
   -- Next_Sensor_Along_Check (internal) --
   -- Helper for the two Next_Sensor_Along functions
   -- If Pos is a sensor advances past it.
   -- Setting may be Middle but if needed then
   -- The_Sensor := No_Sensor, Need_Setting := True;
   -- For_Turnout := which one.
   -- Raises Illegal_Position if layout is not as
   -- expected or algorithm has failed.
   ----------------------------------------

   procedure Next_Sensor_Along_Check (
         Pos          : in     Train_Position;
         Setting      : in     Turnout_Pos;  -- maybe Middle
         The_Sensor   :    out Sensor_Idx;   -- No_Sensor if none or Need_Setting
         Need_Setting :    out Boolean;      -- true if Middle unsatisfactory
         For_Turnout  :    out Turnout_Id    -- possibly 2nd of a chain
         ) is
   --
      Node,
      Node2  : Feature_Node;
      Nxt,
      Pred   : Feature_Idx;
      Polr   : Polarity_Type := Pos.Polr;
   begin
               Debug("Next_Sensor_Along_Check 1");
      The_Sensor := No_Sensor;
      Need_Setting := False;
      For_Turnout := 1;
      Pred := Pos.Within;
      Node := Data(Pos.Within);
      if Node.Ftr.Kind = Sensor then
         -- advance beyond
               Debug("Next_Sensor_Along_Check 2");
         Pred := Node.Next(Polr);
         Node := Data(Pred);
         if Node.Ftr.Kind = Turnout then
               Debug("Next_Sensor_Along_Check 2A " & Pred'img & Node.Ftr.Next_Tu'img);
            if Pos.Within = Node.Ftr.Next_Tu
               -- converging via turned branch
               -- need polarity flip?
               and then Turnout_Data(Node.Ftr.Tid).Swap_Pol_Tu then
               Polr := Opposite(Polr);  -- yes
               Debug("Next_Sensor_Along_Check 2B " & Polr'img);
            end if;
         end if;
      end if;
      -- assert Node isnt a sensor
               Debug("Next_Sensor_Along_Check 3");
      if Node.Ftr.Kind = Turnout then
         if (Polr = Normal_Pol) =
            Turnout_Data(Node.Ftr.Tid).Diverging_When_Normal then
            -- diverging so need turnout info
               Debug("Next_Sensor_Along_Check 4 " & Polr'img & Setting'img);
            case Setting is
               when Middle =>
                  Need_Setting := True;
                  For_Turnout := Node.Ftr.Tid;
                  return;
               when Straight =>
                  -- assert: polr is correct
                  Nxt := Node.Next(Polr);
               when Turned =>
                  Nxt := Node.Ftr.Next_Tu;
            end case;
         else
            -- must be converging so ordinary Next data is ok
            -- assert: any polarity flip has already been done
               Debug("Next_Sensor_Along_Check 5");
            Nxt := Node.Next(Polr);
         end if;
         -- assert: past the (first) turnout
         -- Check for crossing between chained turnouts
         if Data(Nxt).Ftr.Kind = Crossing_Track then
               Debug("Next_Sensor_Along_Check 5a");
            Nxt := Data(Nxt).Next(Polr);
         end if;
         -- assert now on sensor or at second turnout
      else
               Debug("Next_Sensor_Along_Check 6");
         Nxt := Node.Next(Polr);
      end if;
               Debug("Next_Sensor_Along_Check 7" & Nxt'img);
      Node2 := Data(Nxt);
      -- Special case: if Node was dead track then
      -- Node2 is now a sensor or the dummy node Data(0).
               Debug("Next_Sensor_Along_Check 8" & Node2.Ftr.Kind'img);
      case Node2.Ftr.Kind is
         when Sensor =>
            The_Sensor := Node2.Ftr.Sid;
         when Dead_Track =>   -- includes dummy node
            The_Sensor := No_Sensor;
         when Turnout =>
            -- immediate chained turnouts, either back-to-back
            -- as at the crossovers or diverging both ends (block 11)
               Debug("Next_Sensor_Along_Check 9");
            if (Polr = Normal_Pol) =
               Turnout_Data(Node2.Ftr.Tid).Diverging_When_Normal then
               -- diverging so need turnout info (block 11)
               -- assert: we havent used the Setting value already
               -- (no immediate chained turnouts, both diverging)
               case Setting is
                  when Middle =>
                     Need_Setting := True;
                     For_Turnout := Node2.Ftr.Tid;
                     return;
                  when Straight =>
                     Nxt := Node2.Next(Polr);
                  when Turned =>
                     Nxt := Node2.Ftr.Next_Tu;
               end case;
            else
               -- converging:
               Debug("Next_Sensor_Along_Check 10");
               Nxt := Node2.Next(Polr);
            end if;
               Debug("Next_Sensor_Along_Check 11");
            Node2 := Data(Nxt);
            if Node2.Ftr.Kind = Sensor then
               The_Sensor := Node2.Ftr.Sid;
            else
               -- with current layout
               -- we shouldnt reach here as immediate chained turnouts
               -- (the crossovers) must resolve to a sensor
               raise Illegal_Position;
            end if;
            -- end Turnout case
         when others =>
            -- only turnouts can follow immediately without an
            -- intermediate sensor so we shouldnt reach here
            raise Illegal_Position;
      end case;
               Debug("Next_Sensor_Along_Check 12");

   end Next_Sensor_Along_Check;



   -----------------------
   -- Next_Sensor_Along --
   -----------------------

   function Next_Sensor_Along (
         Pos     : Train_Position;
         Setting : Turnout_Pos     )
     return Sensor_Idx is
   --
      The_Sensor   : Sensor_Idx;
      Need_Setting : Boolean;
      For_Turnout  : Turnout_Id;
   begin
      Next_Sensor_Along_Check(Pos, Setting, The_Sensor, Need_Setting, For_Turnout);
      if Need_Setting then
         raise Missing_Turnout_Info;
      end if;
      return The_Sensor;
   end Next_Sensor_Along;

   --------------------------
   -- Next_Sensor_Expected -- (obsolete)
   --------------------------

   function Next_Sensor_Expected (
         Pos : Train_Position )
     return Sensor_Idx is
   --
      The_Sensor   : Sensor_Idx;
      Need_Setting : Boolean;
      For_Turnout  : Turnout_Id;
   begin
      if Feature_Kind_At(Pos) = Sensor then
         return Data(Pos.Within).Ftr.Sid;
         -- this includes all substate values.
      end if;
      Next_Sensor_Along_Check(Pos, Middle, The_Sensor, Need_Setting, For_Turnout);
      return The_Sensor;
   end Next_Sensor_Expected;

   --------------------------
   -- Next_Sensor_Expected --
   --------------------------

   function Next_Sensor_Expected (
         Pos     : Train_Position;
         Setting : Turnout_Pos     )
      return Sensor_Idx is
   --
      The_Sensor   : Sensor_Idx;
      Need_Setting : Boolean;
      For_Turnout  : Turnout_Id;
   begin
      if Feature_Kind_At(Pos) = Sensor then
         return Data(Pos.Within).Ftr.Sid;
         -- this includes all substate values.
      end if;
      Next_Sensor_Along_Check(Pos, Setting, The_Sensor, Need_Setting, For_Turnout);
      if Need_Setting then
         raise Missing_Turnout_Info;
      end if;
      return The_Sensor;
   end Next_Sensor_Expected;

   ------------
   -- Resume --
   ------------

   procedure Resume (
         Pos     : in out Train_Position;
         Setting : in     Turnout_Pos := Middle ) is
   --
      Result : Train_Position;
      --Leaving, Conv : Boolean;
      --Ignore : Chain_Type;
      --Tn : Turnout_Idx;
   begin
      case Pos.Substate is
         when Norm =>
            return;
         when Just_Before =>
            if Feature_Kind_At(Pos) /= Sensor then
               raise Illegal_Position;
            end if;
            Result := Pos;
            --
            -- (v2.9) defer other processing to Advance_Pos
--              Result.Substate := Norm;
--              -- assert Kind = Sensor and Next_Expected is correct
--              Check_Leaving_Turnout(Result, Leaving, Tn, Conv, Ignore);
--              if Leaving then
--                 -- topologically we are on the turnout
--                 Result.Within := T + Feature_Idx(Tn);
--                 -- but physically on the exit block
--                 -- Block_Num has already been set to that of the sensor
--                 --(v2.9)omit: Result.Block_Num := Turnout_Data(Tn).Block_Num;
--
--                 -- if we are at near an inverting join then the next Advance
--                 -- will flip Polarity so need to flip it first
--                 if not Conv then
--                    -- diverging: check if 'turned' sensor
--                    if Data(Result.Within).Ftr.Next_Tu = Pos.Within then
--                       -- via turned branch: is it one of those turnouts?
--                       if Turnout_Data(Tn).Swap_Pol_Tu then
--                             Result.Polr := Opposite(Pos.Polr);
--                       end if;
--                       -- else must be Straight - nothing to do
--                    end if;
--                 end if;
--              else
--                 Result.Within := Data(Pos.Within).Next(Opposite(Pos.Polr));
--                 -- assert this is not a turnout, but could be a boundary sensor
--                 -- assert we dont need to check
--                 --if Is_Boundary_Sensor(Data(Pos.Within).Ftr.Sid) then
--                 Result.Block_Num := Data(Result.Within).Ftr.Block_Num;
--              end if;
            Pos := Result;
         when Just_After =>
            if Feature_Kind_At(Pos) /= Sensor then
               raise Illegal_Position;
            end if;
            Pos.Substate := Norm;
            Advance_Pos(Pos, False, Setting);
      end case;
   end Resume;

   -----------------
   -- Turn_Around --
   -----------------

   procedure Turn_Around (
         Pos : in out Train_Position ) is
   --
      Polr : Polarity_Type := Pos.Polr;
   begin
      Pos.Polr := Opposite(Polr);
      case Pos.Substate is
         when Just_After =>
            Pos.Substate := Just_Before;
         when Just_Before =>
            Pos.Substate := Just_After;
         when Norm =>
            Pos.Next_Expected := Next_Sensor_Expected(Pos);
            -- maybe No_Sensor.  If so client must call
            -- the two-param version of Next_Sensor_Expected
      end case;
   end Turn_Around;

end Topolog2;

