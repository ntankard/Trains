-----------------------------------------------------------------------
-- File:        Simtrack2.adb
-- Description: Displays Trains on track, etc for simrail2
-- Original:    16-Sep-99 track
-- Modified:    14 April 2000 rka moved display aspects from simtr_s
-- Modified:    17 April 2000 rka integrate with Swindows
-- Modified:     7 May 2000 rka Change_Cab param changed
-- Modified:    22 April 2001 rka v 1.4 speed up, rename (was Track.ads)
-- Modified:    11 March 2002 rka v 1.6 8-block config, 24-line swindows
-- Modified:    19 May 2002 rka v 1.6.5 train 2 red
-- Version 1.9:  13-Feb-05 rka  for simrail 1.9 whistle, test-bit
-- Version 1.95:  18-May-07 rka  for swindows below graphics
-- version 2.0  1-Jun-07 renamed Simtrack2, conversion started
-- version 2.1  16-Aug-07 contents moved into Simtrack2.Display,
--   most of Simdefs2 definitions of 2007 layout moved into here.
-- version 2.2  23,24-Jan-08 adjusted dimensions closer to reality
-- version 2.3  6-Feb-08 much closer to reality, elim L26
-- version 2.3  25-Feb-08 split simconst2 into raildefs and simdefs2
-- version 2.4  28-Apr-08 fix segment linkage error in blocks 21, 24
-- version 2.5  28-May-08 move sensors 16, 47 back to geometric ends of
--              turnouts (contrary to reality), fix tramway vert error
--
-- Author:     Rob Allen, Swinburne University of Technology
-----------------------------------------------------------------------
with Raildefs;  use Raildefs;
with Simdefs2; use Simdefs2;

with Ada.Numerics;
use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

package Simtrack2 is


   ------------------------------------------------------------------------
   --                  *** TRACK CONSTANTS ***                            -
   --                all measurements in actual track mm                  -
   ------------------------------------------------------------------------

   -- Constants for idealised physical layout (mm):
   --
   Overall_Height : constant Float := 1950.0;
   Overall_Width  : constant Float := 3200.0;

   Rail_Spacing : constant := 15.0;  -- HO gauge
   Margin        : constant := 75.0;
   Track_Spacing : constant := 67.0;
   Siding_Length : constant := 675.0;  -- nominal, obsolete
   Len_Cross_Overall : constant := 558.0;  -- measured


   R1 : constant := 371.0;
   R2 : constant := 438.0;
   R3 : constant := 505.0;
   R4 : constant := 610.0; -- for sidings
   R5 : constant := 510.1; -- for turnout 8 outer  (actually 505 for ST-245
                           -- but this gives a smoother join)
   R6 : constant := 914.0; -- for Xover turnouts, main diags SL-95, SL-96
   Rd : constant := 728.0; -- for main diags best fit for 45deg

   -- turnouts are measured from centre branch at a cardinal point
   Tlen1 : constant := 168.0;
   Tlen2 : constant := 168.0;
   Tlen3 : constant := 175.0;
   Tlen4 : constant := 186.0; -- 6/02/08 sidings
   Tlen6 : constant := 220.0; -- Xover and diags SL-95 nom 219
   --Tlen5 : constant := 350.0; -- est ST-245 double curved + extra
   Tlen8in : constant := 325.0; -- inner double curved turnout to cut
   Tlen8out : constant := 340.0; -- outer double curved turnout to cut

   Leaf_Part : constant Float := 105.0;

   -- generally arcs are numbered by the turnouts they define:

   -- graphics origin is top left, angles are measured anticlockwise from east
   Xcw   : constant := Margin + R3;               -- centre of W circles
   Ycent : constant := Margin + R2;
   Xcm   : constant := 1930.0;        -- 23/01/08 centre of broken circles of tnouts 12,19
   Xce   : constant := 1950.0 + 22.0 + 3.0*R1 - R2; -- centre of E circles

   Xc_Yardw    : constant := 1950.0 + 22.0;             -- 1972
   Yc_Yardw    : constant := Margin + 2.0 * R2 + R1;    -- 1322
   Xc_Tram     : constant := 1950.0 + 254.0;            -- 2204 tram curve
   Yc_Tram     : constant := 445.0 + R1;                -- 816
   Xtram_Cross : constant := Xc_Tram + R1;              -- 2575

   Xc_Crossw    : constant := Xcw + Tlen6 + 40.0;          -- start of turnouts 7,14,17,9 (6/02/08)
   Yc_Cross_Ndn : constant := Margin + R6;                 -- turnouts 7,6
   Yc_Cross_Nup : constant := Margin + Track_Spacing - R6; -- turnouts 13,14
   Yc_Cross_Sdn : constant := Margin + R2 + R1 + R6;       -- turnouts 17,18
   Yc_Cross_Sup : constant := Margin + 2.0 * R2 - R6;      -- turnouts 9,10
   Xc_Crosse    : constant := Xc_Crossw + Len_Cross_Overall;           -- end of turnouts 6,10,13,18

   Xc_Bypass_Edn : constant := 1930.0;                     -- 23/01/08 turnout 11 sim turnout 19
   Yc_Bypass_Edn : constant := Ycent + R2 + R6;
   Xc_Bypass_Eup : constant := Xc_Bypass_Edn - 490.11 + 1.0;   -- end of bypass straight,
   Yc_Bypass_Eup : constant := Ycent + R3 - R6;                       -- start wriggle
   Xjoin_Bypass   : constant := (Xc_Bypass_Edn + Xc_Bypass_Eup) / 2.0;

   Xc_Yards : constant := Xce + (R2 - R1);    -- siding loop 1/2 circle
   Yc_Yards : constant := Overall_Height - Margin - R1;

   -- siding turnouts
   Xcs      : constant := Xce - (R4 - R2);        -- 6/02/08 bigger radius for these
   Yc_Tn4   : constant := Ycent  + Tlen2;         -- turnout 4 (6/02/08 elim L26)
   Yc_Tn3   : constant := Yc_Tn4 + Tlen4;         -- turnout 3
   Yc_Tn2   : constant := Yc_Tn3 + Tlen4;         -- turnout 2

   Xcsvr   : constant := (Xc_Crossw + Xc_Crosse) / 2.0; -- centre of crossovers
   Xcent   : constant := (Xcw + Xcm) / 2.0;             -- centre of diag crosssing
   Xne_Cut : constant := 2250.0;              -- cut between B5, B6

--   type Point is
--      record
--         X,
--         Y : Float := 0.0;
--      end record;

   E_Centre   : constant Point := (Xce, Ycent);
   Mid_Centre : constant Point := (Xcm, Ycent);
   Mid_Centre_N : constant Point := (Xcm, Ycent+Rd-R1);  -- 23/01/08 centre for N turnout 12
   Mid_Centre_S : constant Point := (Xcm, Ycent-Rd+R1);  -- and S turnout 19
   W_Centre   : constant Point := (Xcw, Ycent);
   W_Centre_N : constant Point := (Xcw, Ycent+Rd-R1);  -- 23/01/08 centre for N turnout 15
   W_Centre_S : constant Point := (Xcw, Ycent-Rd+R1);  -- and S turnout 16

   -- angles for arcs (radian) found by iteratively solving the equations
   --
   --  s = t - 2r sin Th >= 0
   --  g = h - 2r (1 - cos Th) >= 0
   --  tan Th = g/s
   -- where t is horiz separation of centres, h is vert separation of opposite
   -- top circle vs bottom circle (2r for the diag or "broken" arcs,
   -- Track_Spacing = 67 for cross-overs).  g and s define the inner rectangle
   -- containing straight line connectors.
   --
   Theta_Crossover : constant := 0.164061;
   Lenarc_Crossover : constant := R6*Theta_Crossover;  -- 150 << 219
   -- displacment of straight part of crossovers (shallow)
   Dx_Crossover : constant := 128.856; -- was 133.0439.. Xc_Crosse - Xcsvr - R6*Sin(Theta_Crossover);
   Dy_Crossover : constant := 21.332;  -- R2 - R1 - R6*(1.0 - Cos(Theta_Crossover));
   Len_Crossover : constant Float :=
      sqrt(Dx_Crossover*Dx_Crossover + Dy_Crossover*Dy_Crossover);

   Theta_Diag      : constant := 0.784459;   -- 6/02/08 using Rd=728 turnouts
   -- displacment of straight part of main diagonals (should be 45 deg, adjusted Rd to make it)
   Dx_Diag : constant := 157.679;   -- 6/02/08 Xcm - Xcent - Rd*Sin(Theta_Diag);
   Dy_Diag : constant := 157.7265;  -- 6/02/08 R1 - Rd*(1.0 - Cos(Theta_Diag));
   Len_Diag : constant Float := sqrt(Dx_Diag*Dx_Diag + Dy_Diag*Dy_Diag);

   Theta_Bypass    : constant := 0.269779+1.0/R6; -- for r= 914 t = 490.11, h = 67, s=g=0
   Theta_Siding    : constant := 0.55860;    -- 6/02/08 measured (32 not 45deg)
   Pion2           : constant := Pi / 2.0;

   -- turnout angles
   Tang1 : constant Float := Tlen1 / R1;
   Tang2 : constant Float := Tlen2 / R2;
   Tang3 : constant Float := Tlen3 / R3;
   Tang4 : constant Float := Tlen4 / R4;  -- 5/02/08 only for sidings
   Tang6 : constant Float := Tlen6 / R6;  -- 5/02/08
   --Tang5 : constant Float := Tlen5 / R3;
   Tang8in : constant Float := Tlen8in / R2;
   Tang8out : constant Float := Tlen8out / R5;
   Tangd : constant Float := Tlen6 / Rd;  -- 6/02/08 diags
   -- in radians, angle turnout subtends at nearest centre

   Len_Siding_Arc : constant Float := R4 * (Theta_Siding - Tang4);    -- 6/02/08
   Len_Siding_Str2 : constant Float := 410.0;   -- 6/02/08 measured
   Len_Siding_Str3 : constant Float := 630.0;   -- 6/02/08 measured
   Len_Siding_Str4 : constant Float := 810.0;   -- 6/02/08 measured
   Len_Diag_Arc   : constant Float := Rd*(Theta_Diag - Tangd);    -- 6/02/08

   Ltram_Vert  : constant := 750.0;  -- length tram vertical L20
   Ltram_Horiz : constant := 655.0;  -- length tram horizontal L21


--   type Arc_Type is
--      record
--         Radius : Float := 0.0;
--         Centre : Point;
--         Start,
--         Finish : Float := 0.0; --Angles
--         Length : Float;
--      end record;

   Num_Arcs : constant := 43;
   Arcs     : constant
   array (0..Num_Arcs) of Arc_Type:=
      (
      1 =>(R1,(Xc_Yardw, Yc_Yardw),Pion2, Pion2-Tang1, R1*Tang1), -- turnout 1, siding yard W
      35=>(R1,(Xc_Yardw, Yc_Yardw),Pion2-Tang1, 0.0, R1*(Pion2-Tang1)),   -- siding yard W after turnout 1

      22=>(R4,(Xcs, Yc_Tn2),-Theta_Siding, -Tang4, Len_Siding_Arc),   -- pre turnout 2 in yard
      2 =>(R4,(Xcs, Yc_Tn2),-Tang4, 0.0, R4*Tang4),             -- turnout 2 in yard
      23=>(R4,(Xcs, Yc_Tn3),-Theta_Siding, -Tang4, Len_Siding_Arc),   -- pre turnout 3 in yard
      3 =>(R4,(Xcs, Yc_Tn3),-Tang4, 0.0, R4*Tang4),             -- turnout 3 in yard
      24=>(R4,(Xcs, Yc_Tn4),-Theta_Siding, -Tang4, Len_Siding_Arc),   -- pre turnout 4 in yard
      4 =>(R4,(Xcs, Yc_Tn4),-Tang4, 0.0, R4*Tang4),             -- turnout 4 in yard

      25=>(R2,E_Centre,-Pion2, -Tang2, R2*(Pion2-Tang2)),       -- S outer E semicircle
      5 =>(R2,E_Centre,-Tang2, 0.0, R2*Tang2),                  -- turnout 5 on outer right 1/2 circle
      26=>(R2,E_Centre,0.0, Pion2, R2*Pion2),                   -- N outer E quarter circle after turnout

      6 =>(R6,(Xc_Crosse, Yc_Cross_Ndn),Pion2, Pion2+Theta_Crossover, R6*Theta_Crossover),  -- turnout 6
      7 =>(R6,(Xc_Crossw, Yc_Cross_Ndn),Pion2-Theta_Crossover, Pion2, R6*Theta_Crossover),  -- turnout 7

      -- 8 outer "straight" (5/02/08):
      8 =>(R5,(Xcw, Margin + R5),Pion2, Pion2+Tang8out, R5*Tang8out),   -- turnout 8 outer "straight" branch NW
      30=>(R5,(Xcw, Margin + R5),Pion2+Tang8out, Pi-(R5-R2)/R5,
                                       R5*(Pion2-(R5-R2)/R5-Tang8out)), -- continues turnout 8 outer
      31=>(R3,W_Centre,Pi, 1.5*Pi, R3*Pion2),                           -- arc 8 contd, SW quarter circle R3

      -- 8 inner "turned":
      20=>(R2,W_Centre,Pion2, Pion2+Tang8in, R2*Tang8in),         -- turnout 8(inner)
      33=>(R2,W_Centre,Pion2+Tang8in, 1.5*Pi, R2*(Pi-Tang8in)),   -- beyond turnout 8(inner)


      9 =>(R6,(Xc_Crossw, Yc_Cross_Sup),-Pion2, -Pion2+Theta_Crossover, R6*Theta_Crossover),  -- turnout 9
      10=>(R6,(Xc_Crosse, Yc_Cross_Sup),-Pion2-Theta_Crossover, -Pion2, R6*Theta_Crossover),  -- turnout 10 (CW)

      11=>(R6,(Xc_Bypass_Edn, Yc_Bypass_Edn),Pion2+Theta_Bypass, Pion2, R6*Theta_Bypass),  -- turnout 11 bypass
      34=>(R6,(Xc_Bypass_Eup, Yc_Bypass_Eup),-Pion2, -Pion2+Theta_Bypass, R6*Theta_Bypass),  -- curve out of turnout 11

      12=>(Rd,Mid_Centre_N,Pion2, Pion2+Tangd, Rd*Tangd),                -- 23/01/08 turnout 12 NE diag
      40=>(Rd,Mid_Centre_N,Pion2+Tangd, Pion2+Theta_Diag, Len_Diag_Arc), -- 23/01/08 arc NE diag

      13=>(R6,(Xc_Crosse, Yc_Cross_Nup),-Pion2, -Pion2-Theta_Crossover, R6*Theta_Crossover),  -- turnout 13 (CW)
      14=>(R6,(Xc_Crossw, Yc_Cross_Nup),-Pion2+Theta_Crossover, -Pion2, R6*Theta_Crossover),  -- turnout 14 (CW)

      43=>(Rd,W_Centre_N,Pion2-Theta_Diag, Pion2-Tangd, Len_Diag_Arc),       -- 6/02/08 arc NW diag
      15=>(Rd,W_Centre_N,Pion2-Tangd, Pion2, Rd*Tangd),                      -- 6/02/08 turnout 15 N inner W circle
      38=>(R1,W_Centre,Pion2, Pi, R1*Pion2),                               -- inner NW quarter circle
      39=>(R1,W_Centre,Pi, 1.5*Pi, R1*Pion2),                              -- inner SW quarter circle
      16=>(Rd,W_Centre_S,1.5*Pi, 1.5*Pi+Tangd, Rd*Tangd),                    -- 6/02/08 turnout 16 S inner W circle
      42=>(Rd,W_Centre_S,1.5*Pi+Theta_Diag, 1.5*Pi+Tangd, Len_Diag_Arc),     -- 6/02/08 arc SW diag (CW)

      17=>(R6,(Xc_Crossw, Yc_Cross_Sdn),Pion2, Pion2-Theta_Crossover, R6*Theta_Crossover),    -- turnout 17 upper S crossover W (CW)
      18=>(R6,(Xc_Crosse, Yc_Cross_Sdn),Pion2+Theta_Crossover, Pion2, R6*Theta_Crossover),    -- turnout 18 upper S crossover E

      41=>(Rd,Mid_Centre_S,-Pion2-Tangd, -Pion2-Theta_Diag, Len_Diag_Arc),   -- 23/01/08 arc SE diag (CW)
      19=>(Rd,Mid_Centre_S,-Pion2-Tangd, -Pion2, Rd*Tangd),                  -- 23/01/08 turnout 19 SE diag

      32=>(R1,(Xc_Yards, Yc_Yards),-Pi, 0.0, R1*Pi),        -- siding yard S 1/2 circle
      21=>(R1,(Xc_Tram, Yc_Tram),0.0, Pion2, R1*Pion2),        -- tram curve

      36=>(R1,E_Centre,-Pion2, 0.0, R1*Pion2),                 -- S inner E semicircle
      37=>(R1,E_Centre,0.0, Pion2, R1*Pion2),                  -- N inner E semicircle

      others=>(100.0,(1124.0,Ycent),0.0, 2.0*Pi, 200.0*Pi)  -- circle round crossing
      );

   -- NE end of straight parts of siding
   Xsiding  : constant Float := Xcs + R4 * Cos (Theta_Siding);
   Ysiding4 : constant Float := Yc_Tn4 + R4 * Sin(Theta_Siding);
   Ysiding3 : constant Float := Ysiding4 + Tlen4;    -- 5/02/08
   Ysiding2 : constant Float := Ysiding3 + Tlen4;    -- 5/02/08

   -- displacement of straight part of sidings (now varies 6/02/08)
   Dx_Siding4 : constant Float := Len_Siding_Str4 * Sin (Theta_Siding);
   Dy_Siding4 : constant Float := Len_Siding_Str4 * Cos (Theta_Siding);
   Dx_Siding3 : constant Float := Len_Siding_Str3 * Sin (Theta_Siding);
   Dy_Siding3 : constant Float := Len_Siding_Str3 * Cos (Theta_Siding);
   Dx_Siding2 : constant Float := Len_Siding_Str2 * Sin (Theta_Siding);
   Dy_Siding2 : constant Float := Len_Siding_Str2 * Cos (Theta_Siding);

   -- centre of straight part of crossovers
   Y_Cross_N : constant := Margin + (R2 - R1) / 2.0;
   Y_Cross_S : constant := Margin + 2.0 * R2 - (R2 - R1) / 2.0;

   -- some unit normals:
   North : constant Point := (0.0,-1.0);
   South : constant Point := (0.0,1.0);
   East  : constant Point := (1.0,0.0);
   West  : constant Point := (-1.0,0.0);
   Cross_NE : constant Point := (Dy_Crossover/Len_Crossover, -Dx_Crossover/Len_Crossover);
   Cross_SE : constant Point := (Dy_Crossover/Len_Crossover,  Dx_Crossover/Len_Crossover);
   Cross_NW : constant Point := (-Dy_Crossover/Len_Crossover, -Dx_Crossover/Len_Crossover);
   Cross_SW : constant Point := (-Dy_Crossover/Len_Crossover,  Dx_Crossover/Len_Crossover);
   Diag_NE  : constant Point := (Dy_Diag/Len_Diag, -Dx_Diag/Len_Diag);
   Diag_NW  : constant Point := (-Dy_Diag/Len_Diag, -Dx_Diag/Len_Diag);
   Siding_SE : constant Point := (Dy_Siding2/Len_Siding_Str2,  Dx_Siding2/Len_Siding_Str2);

   -- straight lines are generally numbered by turnout number, contiguous lines are
   -- grouped together
   Num_Lines      : constant := 46;

--   type Point_Pair is array (1..2) of Point;  -- first,last
--   type Straight_Line_Type is record
--      Points : Point_Pair;
--      Length : Float;
--      Normal : Point; -- outward unit normal
--   end record;
--
   Straight_Lines : constant array(1..Num_Lines) of Straight_Line_Type :=
      (
      -- upper north straight E to W:
      27=>(((Xce,Margin),     (Xne_Cut,Margin)), Xce-Xne_Cut, North),
      28=>(((Xne_Cut,Margin), (Xc_Crosse,Margin)), Xne_Cut-Xc_Crosse, North),
      6 =>(((Xc_Crosse,Margin), (Xcsvr,Margin)), Xc_Crosse-Xcsvr, North),  -- turnout 6
      7 =>(((Xcsvr,Margin), (Xc_Crossw,Margin)), Xcsvr-Xc_Crossw, North),  -- turnout 7
      29=>(((Xc_Crossw,Margin), (Xcw,Margin)), Xc_Crossw-Xcw, North),

      -- lower north straight E to W:  (5/02/08 all Tlen6)
      35=>(((Xce,Ycent-R1),(Xcm,Ycent-R1)), Xce-Xcm, North),
      12=>(((Xcm,Ycent-R1),(Xcm-Tlen6,Ycent-R1)), Tlen6, North),              -- turnout 12
      36=>(((Xcm-Tlen6,Ycent-R1),(Xc_Crosse,Ycent-R1)), Xcm-Tlen6-Xc_Crosse, North),
      13=>(((Xc_Crosse,Ycent-R1),(Xcsvr,Ycent-R1)), Xc_Crosse-Xcsvr, North),  -- turnout 13
      14=>(((Xcsvr,Ycent-R1),(Xc_Crossw,Ycent-R1)), Xcsvr-Xc_Crossw, North),  -- turnout 14
      37=>(((Xc_Crossw,Ycent-R1),(Xcw+Tlen6,Ycent-R1)), Xc_Crossw-Xcw-Tlen6, North),
      15=>(((Xcw+Tlen6,Ycent-R1),(Xcw,Ycent-R1)), Tlen6, North),    -- turnout 15

      -- upper south straight W to E:  (5/02/08 all Tlen6)
      16=>(((Xcw,Ycent+R1),(Xcw+Tlen6,Ycent+R1)), Tlen6, South),    -- turnout 16
      38=>(((Xcw+Tlen6,Ycent+R1),(Xc_Crossw,Ycent+R1)), Xc_Crossw-Xcw-Tlen6, South),
      17=>(((Xc_Crossw,Ycent+R1),(Xcsvr,Ycent+R1)), Xcsvr-Xc_Crossw, South),  -- turnout 17
      18=>(((Xcsvr,Ycent+R1),(Xc_Crosse,Ycent+R1)), Xc_Crosse-Xcsvr, South),  -- turnout 18
      39=>(((Xc_Crosse,Ycent+R1),(Xcm-Tlen6,Ycent+R1)), Xcm-Tlen6-Xc_Crosse, South),
      19=>(((Xcm-Tlen6,Ycent+R1),(Xcm,Ycent+R1)), Tlen6, South),    -- turnout 19
      40=>(((Xcm,Ycent+R1),(Xce,Ycent+R1)), Xce-Xcm, South),

      -- lower south straight W to E:  (5/02/08 all Tlen6 except T1)
      8 =>(((Xcw,Ycent+R2),(Xc_Crossw,Ycent+R2)), Xc_Crossw-Xcw, South),
      9 =>(((Xc_Crossw,Ycent+R2),(Xcsvr,Ycent+R2)), Xcsvr-Xc_Crossw, South),       -- turnout 9
      10=>(((Xcsvr,Ycent+R2),(Xc_Crosse,Ycent+R2)), Xc_Crosse-Xcsvr, South),       -- turnout 10
      30=>(((Xc_Crosse,Ycent+R2),(Xjoin_Bypass,Ycent+R2)), Xjoin_Bypass-Xc_Crosse, South),
      11=>(((Xjoin_Bypass,Ycent+R2),(Xc_Bypass_Edn,Ycent+R2)), Xc_Bypass_Edn-Xjoin_Bypass, South),  -- turnout 11
      31=>(((Xc_Bypass_Edn,Ycent+R2),(Xc_Yardw,Ycent+R2)), Xc_Yardw-Xc_Bypass_Edn, South),
      1 =>(((Xc_Yardw,Ycent+R2),(Xc_Yardw+Tlen1,Ycent+R2)), Tlen1, South),         -- turnout 1
      32=>(((Xc_Yardw+Tlen1,Ycent+R2),(Xce,Ycent+R2)), Xce-(Xc_Yardw+Tlen1), South),

      -- tram:
      20=>(((Xtram_Cross,Yc_Tram+Ltram_Vert),(Xtram_Cross,Yc_Tram)), Ltram_Vert, East),   -- tram vert up
      21=>(((Xc_Tram,Yc_Tram-R1),(Xc_Tram-Ltram_Horiz,Yc_Tram-R1)), Ltram_Horiz, North),   -- tram horiz

      -- yard W vert down:
      34=>(((Xc_Yards-R1,Yc_Yardw),(Xc_Yards-R1,Yc_Yards)), Yc_Yards-Yc_Yardw, West),

      -- yard E vert up:
      25=>(((Xce+R2,Yc_Yards),(Xce+R2,Yc_Tn2+Tlen4)), Yc_Yards-Yc_Tn2-Tlen4, East),
      2 =>(((Xce+R2,Yc_Tn2+Tlen4),(Xce+R2,Yc_Tn2)), Tlen4, East),     -- st part of turnout 2
      3 =>(((Xce+R2,Yc_Tn3+Tlen4),(Xce+R2,Yc_Tn3)), Tlen4, East),     -- st part of turnout 3
      4 =>(((Xce+R2,Yc_Tn4+Tlen4),(Xce+R2,Yc_Tn4)), Tlen4, East),             -- st part of turnout 4
      -- 26=>(((Xce+R2,Yc_Tn4),(Xce+R2,Ycent+Tlen4)), Yc_Tn4-Ycent-Tlen4, East), -- small vert
      5 =>(((Xce+R2,Ycent+Tlen4),(Xce+R2,Ycent)), Tlen4, East),             -- st part of turnout 5

      -- straight part of sidings:
      24=>(((Xsiding-Dx_Siding4, Ysiding4+Dy_Siding4), (Xsiding, Ysiding4)), Len_Siding_Str4, Siding_SE), -- siding 4 SW to NE
      23=>(((Xsiding-Dx_Siding3, Ysiding3+Dy_Siding3), (Xsiding, Ysiding3)), Len_Siding_Str3, Siding_SE), -- siding 3 straight SW to NE
      22=>(((Xsiding-Dx_Siding2, Ysiding2+Dy_Siding2), (Xsiding, Ysiding2)), Len_Siding_Str2, Siding_SE), -- siding 2 straight

      -- long diagonals:
      41=>(((Xcent+Dx_Diag,Ycent-Dy_Diag), (Xcent-Dx_Diag,Ycent+Dy_Diag)), 2.0*Len_Diag, Diag_NW),   -- diagonal NE to SW
      42=>(((Xcent+Dx_Diag,Ycent+Dy_Diag), (Xcent-Dx_Diag,Ycent-Dy_Diag)), 2.0*Len_Diag, Diag_NE),   -- SE to NW

      -- southern by-pass:
      33=>(((Xcw,Ycent+R3),(Xc_Bypass_Eup,Ycent+R3)), Xc_Bypass_Eup-Xcw, South), -- bypass horiz W to E

      -- short diagonal lines making up centres of crossovers:
      43=>(((Xcsvr+Dx_Crossover,Y_Cross_N-Dy_Crossover),
         (Xcsvr-Dx_Crossover,Y_Cross_N+Dy_Crossover)), 2.0*Len_Crossover, Cross_NW), -- NE to SW, part B15
      44=>(((Xcsvr+Dx_Crossover,Y_Cross_N+Dy_Crossover),
         (Xcsvr-Dx_Crossover,Y_Cross_N-Dy_Crossover)), 2.0*Len_Crossover, Cross_NE), -- SE to NW, part B14

      45=>(((Xcsvr-Dx_Crossover,Y_Cross_S+Dy_Crossover),
         (Xcsvr+Dx_Crossover,Y_Cross_S-Dy_Crossover)), 2.0*Len_Crossover, Cross_SE), -- SW to NE, part B18
      46=>(((Xcsvr-Dx_Crossover,Y_Cross_S-Dy_Crossover),
         (Xcsvr+Dx_Crossover,Y_Cross_S+Dy_Crossover)), 2.0*Len_Crossover, Cross_SW), -- NW to SE, part B17

      others=>((( 1.0,1.0),(50.0,50.0)), 70.7, (0.7,-0.7))
      );

   -- the following data structure hooks all the arcs and lines together into
   -- the layout.  No pointers because they would need init code.

--   type Seg_Index is range 0 .. Max_Segments;

   Num_Segments : Seg_Index := Num_Arcs + Num_Lines + Num_Turnouts;
   A : constant Seg_Index := 0;
   L : constant Seg_Index := Num_Arcs;
   T : constant Seg_Index := Num_Arcs + Num_Lines;

--   type Next_Array is array (Polarity_Type) of Seg_Index;  -- next,prev

--   type Segment_Kind is
--         (Aline,
--          Anarc,
--          Aturnout);

--   type Segment (Kind : Segment_Kind := Aline) is
--   record
--      Id : Integer;
--      Blok : Block_Id;
--      case Kind is
--         when Aline|Anarc=>
--            Next : Next_Array;
--            Tnid : Turnout_Idx := No_Turnout;   -- if non-zero indicates to what turnout this segment belongs
--         when Aturnout=>
--            Converging : Boolean;
--            Seg_St,
--            Seg_Tu     : Seg_Index;
--      end case;
--   end record;
   Dummy_Segment : constant Segment := (Aline, 20, 1, (0, 0), 0);

   Block_Starts : constant
   array (Block_Id range 1..Num_Blocks) of Seg_Index :=
      (
      L+20, -- B1: L20,A21,L21
      L+22, -- B2: L22, A22 before A2
      L+23, -- B3: L23, A23 before A3
      L+24, -- B4: L24, A24 before A4
      T+2,  -- B5: (L2;A2),(L3;A3),(L4;A4),[L26,](L5;A5),A26,L27
      L+28, -- B6: L28,(L6;A6)
      T+7,  -- B7: (L7;A7),L29,(A8,A20)  -- st, tu
      A+30, -- B8: A30,A31
      A+33, -- B9: A33,L8,(L9;A9)
      T+10, -- B10: (L10;A10),L30
      T+11, -- B11: (L11;A11),L31,(L1;A1)
      A+37, -- B12: A37 from E,L35,(L12;A12)
      L+36, -- B13: L36 beyond tn12,(L13;A13,L44)
      L+43, -- B14: L43-,(L14;-A14),L37
      T+15, -- B15: (L15;A15),A38
      A+39, -- B16: A39 from W,(L16;A16)
      L+38, -- B17: L38,(L17;A17,L46)
      L+45, -- B18: L45,(L18;A18),L39
      T+19, -- B19: (L19;A19),L40
      L+32, -- B20: L32,A25 to tn5
      A+35, -- B21: A35 (from tn1),L34,A32,L25 to tn2
      A+40, -- B22: A40 from tn12,L41,A42 to tn16
      A+41, -- B23: A41 from tn19,L42,A43 to tn15
      L+33  -- B24: L33,A34
      );

--   type Train_Position is
--      record
--         Segno : Seg_Index;  -- index into Segments array.
--         Mm    : Float;      -- no of mm into block (0 .. segment length)
--           -- in defined Fwd direction
--         To_Front  : Polarity_Type;
--      end record;

   Segments : constant
   array (Seg_Index range 0..Num_Segments) of Segment :=
      (
      -- dummy zeroth segment
      Dummy_Segment,

      -- arcs part of turnouts: (index 1..43)
      (Anarc,  1, 11, (A+35, L+31), 1),  -- diverging with normal polr
      (Anarc,  2,  5, (L+ 3, A+22), 2),
      (Anarc,  3,  5, (L+ 4, A+23), 3),
      (Anarc,  4,  5, (L+ 5, A+24), 4),
      (Anarc,  5,  5, (A+26, A+25), 5),
      (Anarc,  6,  6, (L+43, L+28), 6),  -- diverging into converging T14
      (Anarc,  7,  7, (L+29, L+44), 7),
      (Anarc,  8,  7, (A+30, L+29), 8),  -- diverging, double arc
      (Anarc,  9,  9, (L+45, L+ 8), 9),  -- diverging
      (Anarc, 10, 10, (L+30, L+46), 10),
      (Anarc, 11, 11, (L+31, A+34), 11),
      (Anarc, 12, 12, (A+40, L+35), 12),  -- diverging
      (Anarc, 13, 13, (L+44, L+36), 13),  -- diverging
      (Anarc, 14, 14, (L+37, L+43), 14),
      (Anarc, 15, 15, (A+38, A+43), 15),
      (Anarc, 16, 16, (A+42, A+39), 16),  -- diverging
      (Anarc, 17, 17, (L+46, L+38), 17),  -- diverging
      (Anarc, 18, 18, (L+39, L+45), 18),
      (Anarc, 19, 19, (L+40, A+41), 19),

      -- other arcs:
      (Anarc, 20,  7, (A+33, L+29), 8),  -- turnout 8 inner
      (Anarc, 21,  1, (L+21, L+20), 0),  -- tram curve

      (Anarc, 22,  2, (A+2,  L+22), 0),  -- curved parts of sidings
      (Anarc, 23,  3, (A+3,  L+23), 0),
      (Anarc, 24,  4, (A+4,  L+24), 0),
      -- B20: L32,A25 to tn5
      (Anarc, 25, 20, (A+5,  L+32), 0),
      (Anarc, 26,  5, (L+27, T+5), 0),
      Dummy_Segment,      -- A27 missing
      Dummy_Segment,      -- A28 missing
      Dummy_Segment,      -- A29 missing
      -- B8: A30,A31
      (Anarc, 30, 8, (A+31, A+ 8), 0),  -- continues T8 outer
      (Anarc, 31, 8, (L+33, A+30), 0),
      (Anarc, 32, 21, (L+25,L+34), 0),  -- S yard semicircle
      -- B9: A33,L8,(L9;A9)
      (Anarc, 33,  9, (L+8, A+20), 0),  -- continues T8 inner
      (Anarc, 34, 24, (A+11,L+33), 0),  -- (v2.4) bug fix, was ,A+31
      -- B21: A35 (from tn1),L34,A20,L25 to tn2
      (Anarc, 35, 21, (L+34, A+1), 0),
      (Anarc, 36, 19, (A+37,L+40), 0),
      -- B12: A37 from E,L35,(L12;A12)
      (Anarc, 37, 12, (L+35,A+36), 0),
      -- B15: (L15;A15),A38
      (Anarc, 38, 15, (A+39,T+15), 0),
      -- B16: A39 from W,(L16;A16)
      (Anarc, 39, 16, (T+16,A+38), 0),
      -- B22: A40 from tn12,L41,A42 to tn16
      (Anarc, 40, 22, (L+41,A+12), 0),
      -- B23: A41 from tn19,L42,A43 to tn15
      (Anarc, 41, 23, (L+42,A+19), 0),
      (Anarc, 42, 22, (A+16,L+41), 0),
      (Anarc, 43, 23, (A+15,L+42), 0),

      -- lines part of turnouts: (index=44..89)
      (Aline,  1, 11, (L+32, L+31), 1),  -- diverging
      (Aline,  2,  5, (L+ 3, L+25), 2),
      (Aline,  3,  5, (L+ 4, T+ 2), 3),
      (Aline,  4,  5, (L+ 5, T+ 3), 4),  -- L26 was here
      (Aline,  5,  5, (A+26, T+ 4), 5),  -- L26 was here
      (Aline,  6,  6, (L+ 7, L+28), 6),  -- diverging
      (Aline,  7,  7, (L+29, L+ 6), 7),
      (Aline,  8,  9, (T+ 9, A+33), 0),  -- actually a straight_line, not turnout
      (Aline,  9,  9, (L+10, L+ 8), 9),
      (Aline, 10, 10, (L+30, L+ 9), 10),
      (Aline, 11, 11, (L+31, L+30), 11),
      (Aline, 12, 12, (L+36, L+35), 12),
      (Aline, 13, 13, (L+14, L+36), 13),
      (Aline, 14, 14, (L+37, L+13), 14),
      (Aline, 15, 15, (A+38, L+37), 15),
      (Aline, 16, 16, (L+38, A+39), 16),
      (Aline, 17, 17, (L+18, L+38), 17),
      (Aline, 18, 18, (L+39, L+17), 18),
      (Aline, 19, 19, (L+40, L+39), 19),
      -- B1: L20,A21,L21
      (Aline, 20,  1, (A+21,   0), 0),  -- no pred
      (Aline, 21,  1, (   0,A+21), 0),  -- no succ
      -- B2: L22, A22 before A2
      (Aline, 22,  2, (A+22,   0), 0),  -- no pred
      -- B3: L23, A23 before A3
      (Aline, 23,  3, (A+23,   0), 0),  -- no pred
      -- B4: L24, A24 before A4
      (Aline, 24,  4, (A+24,   0), 0),  -- no pred
      -- B6: L28,(L6;A6)
      (Aline, 25, 21, (L+ 2,A+32), 0),  -- (v2.4) bug fix, was ,A+20
      (Aline, 26,  5, (L+ 5,T+ 4), 0),  -- fossil 6/02/08
      (Aline, 27,  5, (L+28,A+26), 0),
      (Aline, 28,  6, (T+ 6,L+27), 0),
      (Aline, 29,  7, (T+ 8,T+ 7), 0),
      (Aline, 30, 10, (L+11,T+10), 0),
      (Aline, 31, 11, (T+ 1,T+11), 0),
      -- B20: L32,A25 to tn5
      (Aline, 32, 20, (A+25,L+ 1), 0),
      -- B24: L33,A34
      (Aline, 33, 24, (A+34,A+31), 0),
      (Aline, 34, 21, (A+32,A+35), 0),
      (Aline, 35, 12, (T+12,A+37), 0),
      -- B13: L36 beyond tn12,(L13;A13)
      (Aline, 36, 13, (T+13,L+12), 0),
      (Aline, 37, 14, (L+15,T+14), 0),
      -- B17: L38,(L17;A17,L46)
      (Aline, 38, 17, (T+17,L+16), 0),
      (Aline, 39, 18, (L+19,T+18), 0),
      (Aline, 40, 19, (A+36,T+19), 0),
      (Aline, 41, 22, (A+42,A+40), 0),  -- diag to SW
      (Aline, 42, 23, (A+43,A+41), 0),  -- diag to NW
      (Aline, 43, 14, (A+14,A+ 6), 0),  -- short N crossover to SW
      (Aline, 44, 13, (A+7, A+13), 0),  -- short N crossover to NW
      (Aline, 45, 18, (A+18,A+ 9), 0),  -- short S crossover to NE
      (Aline, 46, 17, (A+10,A+17), 0),  -- short S crossover to SE

      -- turnouts: (index=90..108)
      (Aturnout,  1, 11, False, L+1, A+1),  -- diverging with normal polr
      -- B5: (L2;A2),(A3;L3),(A4;L4),L26,(A5;L5),A26,L27
      (Aturnout,  2,  5, True, L+2, A+2),
      (Aturnout,  3,  5, True, L+3, A+3),
      (Aturnout,  4,  5, True, L+4, A+4),
      (Aturnout,  5,  5, True, L+5, A+5),
      (Aturnout,  6,  6, False, L+6, A+6),   -- diverging
      -- B7: (L7;A7),L29,(A8;A20)
      (Aturnout,  7,  7, True, L+7, A+7),
      (Aturnout,  8,  7, False, A+8, A+20),  -- diverging -- NB outer is defined as straight
      (Aturnout,  9,  9, False, L+9, A+9),    -- diverging
      -- B10: (L10;A10),L30
      (Aturnout, 10, 10, True, L+10, A+10),
      -- B11: (L11;A11),L31,(L1;A1)
      (Aturnout, 11, 11, True, L+11, A+11),
      (Aturnout, 12, 12, False, L+12, A+12),  -- diverging
      (Aturnout, 13, 13, False, L+13, A+13),  -- diverging
      -- B14: (L14;A14),L37
      (Aturnout, 14, 14, True, L+14, A+14),
      -- B15: (L15;A15),A38
      (Aturnout, 15, 15, True, L+15, A+15),
      (Aturnout, 16, 16, False, L+16, A+16),  -- diverging
      (Aturnout, 17, 17, False, L+17, A+17),  -- diverging
      -- B18: (L18;A18),L39
      (Aturnout, 18, 18, True, L+18, A+18),
      -- B19: (L19;A19),L40
      (Aturnout, 19, 19, True, L+19, A+19)
      );

   -- segments involved in crossings or crossovers, in pairs:
   --
   Crossing_Segments : constant array (1..2*Num_Crossings) of Seg_Index :=
      (
      L+41,  -- id no of main diagonal straight line from NE
      L+42,  -- id no of main diagonal straight line from SE
      L+43,  -- id no of upper crossover straight line from NE
      L+44,  -- id no of upper crossover straight line from SE
      L+45,  -- id no of lower crossover straight line from SW
      L+46,  -- id no of lower crossover straight line from NW
      L+40,  -- id no of upper south straight
      L+20,  -- id no of tramway vertical
      L+32,  -- id no of lower south straight
      L+20   -- id no of tramway vertical
      );
   Crossing_Points : constant array (1..2*Num_Crossings) of Float :=
      (
      1|2=> Len_Diag,
      7=> Xtram_Cross-Xcm, -- upper south straight where tramway intersects
      9=> Xtram_Cross-(Xc_Yardw+Tlen1), -- lower south straight where tramway intersects
         -- 28/05/08: fixed next 2 lines
      8=> Yc_Tram+Ltram_Vert - (Ycent+R1), -- tramway vertical where upper south straight intersects
      10=> Yc_Tram+Ltram_Vert - (Ycent+R2), -- tramway vertical where lower south straight intersects
      others=>0.0 );  -- ignored for short crossovers

   -- Physical constants for sensor placement within segments:
   --
   --  sensor_segment_numbers contains the segment a sensor is in,
   --  sensor_segment_mm contains counts from the start of the segment,
   --  nc indicates not used
   --
   Nc : constant Seg_Index := 0;

   Sensor_Segment_Numbers : constant
   array(Sensor_Id range Sensor_Id'first..Num_Sensors) of Seg_Index :=
      (
      A+35, A+32, L+22, A+22,   L+23, A+23, L+24, A+24,  --  1  2  3  4   5  6  7  8
      nc,   L+25, nc,   L+3,    nc,   L+4,  L+40, L+ 5,  --  9 10 11 12  13 14 15 [16
      A+36, L+32, A+36, L+32,   A+37, A+25, A+37, A+25,  -- [17 18 19 20  21 [22 23 24
      L+20, A+26, L+20, L+27,   A+21, L+28, L+21, L+28,  -- 25 26 27 28  29 30 31 32
      L+21, L+28, L+35, L+29,   L+36, L+29, L+37, A+33,  -- 33 34 35 36  37 [38 39 40   [23/02/08]
      A+38, L+8,  A+39, L+30,   A+39, L+30, L+38, A+42,  -- 41 42 43 44  45 [46 47 [48  [23/01/08]
      L+39, L+41, L+40, nc,     A+41, nc,   L+42, L+41,  -- 49 50 51 52  [53 54 55 56
      Nc,   A+40, Nc,   A+30,   L+42, L+33, A+43, A+34 );-- 57 [58 59 60  61 62 [63 64
   -- note sensors on boundaries are at 0.0 of the next segment.

   Std    : constant := 125.0;  -- standard stopping margin mm
   Sidm   : constant := Len_Siding_Arc - 30.0;
   Stramu : constant := Xc_Tram + R1 - Xcm;                -- where tram crosses seg 40
   Straml : constant := Xtram_Cross - (Xc_Yardw + Tlen1);  -- where tram crosses seg 32
   Stramn : constant := Yc_Tram + Ltram_Vert - Ycent - R1; -- where on seg 20 tram crosses seg 40
   Strams : constant := Yc_Tram + Ltram_Vert - Ycent - R2; -- where on seg 20 tram crosses seg 32
   Stramw : constant := Ltram_Horiz - Std;   -- sensor near W of tram horizontal
   Len27  : constant := (Xce - Xne_Cut);       -- for sensor E of cut on NE horizontal(L27)
   Len8   : constant := (Xc_Crossw - Xcw);     -- for sensor 42 on horizontal(L8)
   Len28  : constant := (Xne_Cut-Xc_Crosse);   -- for sensor E of crossovers on horizontal(L28)
   Len29  : constant := (Xc_Crossw - Xcw);     -- for sensor E of double turnout on L29

   Sensor_Segment_Mm : constant
   array (Sensor_Id range Sensor_Id'first..Num_Sensors) of Float :=
      (
      1=> Std,     2=> 292.0,        3=> Std,         4=> Sidm,
      5=> Std,     6=> Sidm,         7=> Std,         8=> Sidm,
      9=> 0.0,    10=> Std,         11=> 0.0,        12=> 0.0,
      13=> 0.0,   14=> 0.0,         15=> Stramu-Std, 16=> 0.0,   -- s16 should be 25.0 (28/05/08)
      17=>31.0,   18=>Std,          19=>R1*Pion2-Std, 20=>Straml-Std,
      21=>0.0,    22=>11.0,         23=>Std,          24=>R2*(Pion2-Tang2)-Std,
      25=>Std,    26=>Std,          27=>Strams-Std,   28=>Len27-165.0,
      29=>Stramn+Std-Ltram_Vert, 30=>0.0,    31=>0.0,     32=>140.0,
      33=>Stramw, 34=>Len28-50.0,   35=>Xce-Xcm-Std,  36=>25.0,
      37=>Std/2.0, 38=>Len29-40.0,  39=>15.0,         40=>58.0,
      41=>53.0,    42=>Len8-34.0,    43=>0.0,          44=>Std,
      45=>R1*Pion2-74.0, 46=>Xjoin_Bypass-Xc_Crosse - 75.0,   -- 23/01/08 check!!
                                    47=>0.0,   -- s47 should be 25.0 (28/05/08)
                                                      48=>Len_Diag_Arc-Std,
      49=>Std/2.0,    50=>Len_Diag+Std, 51=>Std,          52=>0.0, -- 52,54,57,59 not installed yet
      53=>Std,        54=>0.0,          55=>Len_Diag-Std, 56=>Len_Diag-Std,
      57=>0.0,        58=>Std,          59=>0.0,          60=>75.0,
      61=>Len_Diag+Std,  62=>0.0,       63=>Len_Diag_Arc-Std, 64=>R6*Theta_Bypass-Std );
   -- note sensors on boundaries are at 0.0 of the next segment.
   -- No mm should be <0, eg we assume Std < Len_Diag_Arc, nor > length of its segment


   -------------------------------------------------------------------------------------------------
   --
   --                     Train Dimensions (in mm NOT SCALED)
   --
   Wheel_Base         : constant := 110.0;
   --Train_Length       : constant := 146.0;  -- unused
   Wheel_To_Reflector : constant := 8.0;   -- renamed 19/05/02
   Carriage_Length    : constant := 100.0; -- nominal 17/04/01


end Simtrack2;