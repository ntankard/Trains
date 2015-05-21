-- Simrail2-Step  version 2.1
-- One step in the life history of a single train T.
--
-- Author: Rob Allen, Swinburne Univ Tech.
-- Version 2.0.0  9-Jul-2007 rka  converted from Simrail v1.9.9
-- version 2.0.2 18 Sep 2007: crash tram when hits ends
-- version 2.0.3 25 Feb 2008: simdefs2 replaces Simconst2
-- version 2.1.2: 28-May-2008 fix bugs in crossing and turnout occupancy
-- version 2.1.3: 30-May-2008 fix bug in Fix_Up_Crossing
-- version 2.1.4: 18-Jun-2008 fix bug in Going_Forward calculation which affected
--                            turnout/crossing occupancy when train backs out
-- version 2.1.5  19-Jun-2008 use const Simdefs2.Turnout_Tolerance, fix derailment
--                            check in Check_Turnout_Entry_And_Exit & Advance
-- version 2.1.6  30-Jun-2008 fix trains changing length when reversed over an
--                            inverting join (long-standing bug)
-- version 2.1.7   4-Aug-2008 allow turnout changeover when train at diverging
--                            end not over moving part (kludge!)
--                            (Check_Turnout_Consistent, Check_Turnout_Entry_And_Exit)
-- version 2.1.8  30-Mar-2011 reduce warnings
-- version 2.1.9   9-Mar-2012 for raildefs 2.5
-- version 2.2.2   3-Aug-2013 fix bug on entering crossover in reverse with
--                            front overshoot on moving part; decrease coasting
--                            when has carriages by about 33%
--
separate (Simrail2)
procedure Step (
      T            : in out Train_Type;
      Elapsed_Time : in     Duration    ) is
   --
   use Ada.Float_Text_Io;
   use Simtrack2;

   --Old_T : Train_Type := T; -- debug, maybe reengineer.

   Old_Front_Pos,
   Old_Front_Wheel,
   Old_Mid_Pos,
   Old_Back_Wheel,
   Old_Back_Pos  : Train_Position;
   Front_Block   : Block_Id;
   Rear_Block    : Block_Id;
   --A_Block : Block_Idx;
   Front_Voltage,
   Rear_Voltage  : Float := 0.0;
--   Step,
   Front_Step    : Float := 0.0;
--   Debug,
--   Debug_Inv,
   Going_Forward,
   Bad           : Boolean := false;
   Over_Crossing,
   Turnout_Mutex_Error : Boolean := False;
   The_Crossing : Crossing_Idx:= No_Crossing;

   -- function Speed_For
   -- returns the steady-state signed speed corresponding
   -- to the voltage.
   -- This version (7/05/00) has a sharp step at Volt_Zero_Speed and is
   -- zero below that, linear above.
   --  That is:  0.0 .. 4.1 -> 0,  8.6 -> 154 mm/s see Simdefs2.
   --
   function Speed_For (
         Voltage : in     Float )
     return Float is
      --
      Speed : Float := (
      abs(Voltage) - Volt_Zero_Speed)
         / (Max_Cab_Voltage - Volt_Zero_Speed) * Max_Train_Speed;
   begin
      if abs Voltage < Volt_Zero_Speed then
         return 0.0;
      elsif Voltage < 0.0 then
         return -Speed;
      else
         return Speed;
      end if;
   end Speed_For;

   -- Calculate_Step
   -- finds the distance in mm that a train will move
   -- in the elapsed_time, will give negative step if voltage negative.
   -- Also adjusts current speed.
   -- Inertial effects in speed up, coast and drive reversal are included.
   -- Assumes elapsed_time always positive.
   -- Note: Speed is relative to the locomotive, negative if the loco is going
   -- backward.  Drive_Speed is relative to the track section (under the front
   -- wheels); Polr_To_Front indicates how to convert, ie negate if Rev.
   -- Step_Mm is set relative to the track section and will need translating
   -- to the coordinates of each other relevant track section (eg rear of train).
   --
   procedure Calculate_Step (
         Speed       : in out Float;
         Polr_To_Front : in Polarity_Type;
         Drive_Speed : in     Float;
         -- steady state speed as per current voltage
         Elapsed_Time : in     Duration;
         Step_Mm      :    out Float     ) is
      --
      -- debugging:
      --use Ada.Float_Text_Io;

      Old_Speed   : Float := Speed;
      Drive       : Float := Drive_Speed;
      Delta_Time  : Float := Float (Elapsed_Time);
      Delta_Speed : Float;
      Debug : Boolean := False;
   begin
      if Polr_To_Front = Reverse_Pol then
         Drive := -Drive;
      end if;
      if Speed*Drive < 0.0 then
         -- sudden speed reversal as in emergency stop

--         Put("Speed:");  Put(Old_Speed, Exp=>0, Aft=>1);
--         Put_Line("mm/s");  -- debug
--         Put_Line("Polr_to_front: " & Polr_To_front'img);
--         Put("Reverse Drive_Speed:");  Put(Drive_Speed, Exp=>0, Aft=>1);
--         Put_Line("mm/s");  -- debug
--         Put("Elapsed_Time:");  Put(Delta_Time, Exp=>0, Aft=>3);
--         Put_Line("sec");  --
--         debug := True;

         -- the following is a guess at the physics: fixed high negative accel,
         -- definitely wrong if the reverse drive is small
         Delta_Speed := Max_Acceleration*Delta_Time;
         if Speed > 0.0 then
            Speed := Speed - Delta_Speed;
            -- Drive is negative, maybe we've changed to be too negative...
            if Speed < Drive then Speed := Drive;  end if;
         else
            Speed := Speed + Delta_Speed;
            -- Drive is positive, maybe we've changed to be too positive...
            if Speed > Drive then Speed := Drive;  end if;
         end if;
      elsif Drive = 0.0 then
         -- coasting
         Delta_Speed := Drag_Deceleration*Delta_Time;
         if T.Has_Carriages then
            Delta_Speed := Delta_Speed * 1.5;  -- carriages have more drag
         end if;
         if Old_Speed > Delta_Speed then
            --Put(Delta_Time, Exp=>0, Aft=>3);
            --Put(Speed, Exp=>0, Aft=>1);  Put_Line("mm/s slowing");  -- debug
            Speed := Old_Speed - Delta_Speed;
         elsif Old_Speed < -Delta_Speed then
            --Put(Delta_Time, Exp=>0, Aft=>3);
            --Put(Speed, Exp=>0, Aft=>1);  Put_Line("mm/s slowing");  -- debug
            Speed := Old_Speed + Delta_Speed;
         else
            Speed := 0.0;
         end if;
      else
         -- constant or ramping up or down
         Delta_Speed := Max_Acceleration*Delta_Time;
         if abs(Old_Speed - Drive) <= Delta_Speed then
            Speed := Drive;
         elsif Old_Speed < Drive then
            -- ramping up (if Speed>0) or down (but -ve)
            --Put(Delta_Time, Exp=>0, Aft=>3);
            --Put(Speed, Exp=>0, Aft=>1);  Put_Line("mm/s rising");  -- debug
            Speed := Old_Speed + Delta_Speed;
         else
            -- ramping down (+ve) or up (-ve)
            --Put(Delta_Time, Exp=>0, Aft=>3);
            --Put(Speed, Exp=>0, Aft=>1);  Put_Line("mm/s falling");  -- debug
            Speed := Old_Speed - Delta_Speed;
         end if;
      end if;
      Step_Mm := 0.5*(Old_Speed + Speed)*Delta_Time;
      if Polr_To_Front = Reverse_Pol then
         Step_Mm := -Step_Mm;
      end if;
      if Debug then Put("Step_Mm:");  Put(Step_Mm, Exp=>0, Aft=>1);  New_Line; end if;
   exception
   when others =>
      Put_Line("sim.Train" & T.Id'Img & " Step-Calculate_Step: died");
      raise;
   end Calculate_Step;


   -- Check_For_Voltage_Mismatch
   -- the front wheel block  and position is (Bf,Fpos) and
   -- the rear wheel position is (Br,Rpos) (maybe across block boundaries Bf/=Br)
   -- and vf and vr are the two voltages, front and rear, which should be compatible.
   -- May adjust either Vf or Vr to compensate for a polarity relay being in transition.
   -- Sets Bad true if there is a mismatch.
   --
   procedure Check_For_Voltage_Mismatch (
         Bf,
         Br : Block_Id;
         Fpos,
         Rpos : in Train_Position;
         Vf,
         Vr : in out Float;  -- 26/05/05 Vr also in out
         Bad : out Boolean ) is
      --
      Inverting_Join,
      Mismatch : Boolean := False;
      Corrected_Vr : Float;
   begin
      if Bf = Br or else (Vf = 0.0 and Vr = 0.0) then
         -- nothing to do, same block or train is stopped or both relays open circuit
         Bad := False;
         return;
      end if;
      Inverting_Join := Fpos.To_Front /= Rpos.To_Front;
      if Vr = 0.0 then
         -- relay in transition or CAB wrong
         if Is_Open_Circuit(Blockdrivers(Br)) then
            if Inverting_Join then
               Vr := -Vf;
            else
               Vr := Vf;
            end if;
         else
            Mismatch := True;  -- a wrong CAB
            Put_Line("WRONG CAB, ONE ZERO");
         end if;
      elsif Vf = 0.0 then
         -- relay in transition or CAB wrong
         if Is_Open_Circuit(Blockdrivers(Bf)) then
            if Inverting_Join then
               Vf := -Vr;
            else
               Vf := Vr;
            end if;
         else
            Mismatch := True;  -- a wrong CAB
         end if;
      else
         if Inverting_Join then
            Corrected_Vr := -Vr;
         else
            Corrected_Vr := Vr;
         end if;
         --if Corrected_Vr /= Vf then
         if abs(Corrected_Vr - Vf) > 0.002 then
            -- they have different CAB or wrong polarity, so set error
            Mismatch := True;
            Put("NEITHER ZERO, Inverting_Join=" & Inverting_Join'img);  Put(Corrected_Vr);  Put(Vf);
            New_Line;
         end if;
      end if;
      Bad := Mismatch;
   exception
     when others =>
      Put_Line("sim.train" & T.Id'Img & " Step-Check_For_Voltage_Mismatch: died");
      raise;
   end Check_For_Voltage_Mismatch;

   function Length_Of( Seg : Segment ) return Float is
      -- since 2.2.10
   begin
      if Seg.Kind = Aline then
         return Straight_Lines(Seg.Id).Length;
      elsif Seg.Kind = Anarc then
         return Arcs(Seg.Id).Length;
      end if;
      return 1000.0; -- shouln't happen
   end Length_Of;

   -- Advance a point on the train by Step (mm,signed) relative to
   -- Ref_Pos and adjusted to the currect section if an inverting join
   -- is already known to exist between Old_Pos and Ref_Pos.
   -- New_Pos may be on a different segment with New_Pos.Mm adjusted
   -- otherwise abs(Old_PosMm - New_Pos.Mn) = Step.
   -- (param Ref_Pos added v2.1.6 30/06/08)
   -- Pre: current segment Kind /= Turnout
   --
   procedure Advance (
         Old_Pos : in     Train_Position;
         New_Pos : in out Train_Position;
         Step    : in     Float;
         Ref_Pos : in     Train_Position  ) is
      --
      Mms, Mm_Left, Len : Float;
      Cur_Seg, New_Seg, Tn_Seg : Segment;
      Tpos : Turnout_Pos;
      Inverting : Boolean;
   begin
      Cur_Seg := Segments(Old_Pos.Segno);
      New_Pos := Old_Pos;
      if Step = 0.0 then
         return;
      elsif Old_Pos.To_Front = Ref_Pos.To_Front then
         Mms := Step;
      else
         Mms := -Step;
      end if;

      New_Pos.Mm := Old_Pos.Mm + Mms;
      if (New_Pos.Mm < 0.0) then
         -- gone off bottom
         if Cur_Seg.Next(Reverse_Pol) = 0 then
            New_Pos.Mm := 0.0;
            T.Crashed := True;
            return;
         end if;
         Mm_Left := New_Pos.Mm;  -- < 0.0
         New_Pos.Segno := Cur_Seg.Next(Reverse_Pol);
         New_Seg := Segments(New_Pos.Segno);
      else
         Len := Length_Of(Cur_Seg);

         if New_Pos.Mm > Len then
            -- gone off top
            if Cur_Seg.Next(Normal_Pol) = 0 then
               New_Pos.Mm := Len;
               T.Crashed := True;
               return;
            end if;
            Mm_Left := New_Pos.Mm - Len;
            New_Pos.Segno := Cur_Seg.Next(Normal_Pol);
            New_Seg := Segments(New_Pos.Segno);
         else
            return;  -- still in same segment
         end if;
      end if;
      -- We are changing segments, but some more to do:
      -- New_Pos.Mm may need adjusting, New_Pos.To_Front too
      -- We might need to choose a turnout branch.
      -- (1) is there a polarity flip (at only two places on layout)?
      --
      Inverting := False; -- normal case
      if Cur_Seg.Kind = Anarc then
         --Put(" arc" & Cur_Seg.Id'img);
         --   WARNING: MAGIC NUMBERS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         if Mms >= 0.0 then
            Inverting := Old_Pos.Segno = A+16 or Old_Pos.Segno = A+42;
         else
            Inverting := Old_Pos.Segno = A+19 or Old_Pos.Segno = A+41;
         end if;
         --Put_Line(" inverting=" & Inverting'img);
      end if;
      if Inverting then
         -- Debug_Inv := True; -- global
         New_Pos.To_Front := Opposite(Old_Pos.To_Front);
         Mm_Left := -Mm_Left;  --v2.1.6 confirmed
      end if;
      -- note: New_Pos.Mm is still not correct

      -- (2) is it a diverging turnout?
      --
      if New_Seg.Kind = Aturnout then
         --Put(" aturnout " & New_Seg.Id'img);
         -- attribute Converging applies to Normal_Pol polarity ie +ve Mms
         if New_Seg.Converging = (Mm_Left >= 0.0) then
            Put_Line(" converging: ERROR should have come in via branch");
            return;
         else
            --Put(" diverging ");
            -- diverging onto one branch or other:
            Tpos := Position_Of(Turnouts(Turnout_Id(New_Seg.Id)));
            if Tpos = Turned then
               New_Pos.Segno := New_Seg.Seg_Tu;
            else
               New_Pos.Segno := New_Seg.Seg_St;  -- assume not Middle
            end if;
            New_Seg := Segments(New_Pos.Segno);
         end if;
         --Put_line(" new segno=" & New_Pos.Segno'img);
      elsif New_Seg.Tnid /= No_Turnout then
         --
         -- (3) check for converging turnout position being correct
         --
         Tn_Seg := Segments(Simtrack2.T + Seg_Index(New_Seg.Tnid));
         if Tn_Seg.Converging = (Mm_Left >= 0.0) then
            Tpos := Position_Of(Turnouts(New_Seg.Tnid));
            if Tpos = Middle
            or else (Tpos = Turned and then
               Tn_Seg.Seg_Tu /= New_Pos.Segno)
            or else (Tpos = Straight and then
               Tn_Seg.Seg_St /= New_Pos.Segno)
            then
               Put_Line("simrail2-step.advance train" & T.Id'img &
               " converging on turnout" & New_Seg.Tnid'img &
               " may soon derail:" & Tpos'img); --(v2.1.5)
               --T.Crashed := True;
            end if;
         end if;
      end if;

      --
      -- (4) adjust Mm
      --
      if Mm_Left >= 0.0 then
         New_Pos.Mm := Mm_Left;
      else
         New_Pos.Mm := Mm_Left + Length_Of(New_Seg);
      end if;

   exception
   when others =>
      Put_Line("sim.Train" & T.Id'Img & " Step-Advance: died");
      raise;
   end Advance;

   -- Check_Turnout_Consistent
   --
   -- pre: current front, Pos, is entering moving part of
   -- turnout Tnid
   -- post:  if current turnout position is wrong then
   --    if just at the diverging start then Pos.Segno and Wheel_Pos.Segno
   --     are changed to the other branch   --(v2.1.7)
   --    else T.Crashed is true
   --
   procedure Check_Turnout_Consistent(   --(v2.1.5)
      Tnid       : in Turnout_Id;
      To_Front   : in Polarity_Type;      --(v2.1.7)
      Pos,
      Wheel_Pos  : in out Train_Position  --(v2.1.7)
   ) is
   --
      Tn_Seg : Segment;
      Tpos : Turnout_Pos;
      Err : Boolean := False;
      Mm_From_End : Float;
   begin
      Tpos := Position_Of(Turnouts(Tnid));
      Tn_Seg := Segments(Simtrack2.T + Seg_Index(Tnid));
      if Tpos = Middle then
         Err := True;
      elsif (Tpos = Turned and then
         Tn_Seg.Seg_Tu /= Pos.Segno) then
         -- (v2.1.7) if diverging then we've just arrived at the moving part...
         if Tn_Seg.Converging /= (To_Front = Normal_Pol) then
               -- swap over to Seg_Tu
            Pos.Segno := Tn_Seg.Seg_Tu;

            if To_Front = Reverse_Pol then   -- v2.1.10
               -- make mm from end the same:
               Mm_From_End := Length_Of(Segments(Tn_Seg.Seg_St)) - Pos.Mm;
               Pos.Mm := Length_Of(Segments(Tn_Seg.Seg_Tu)) - Mm_From_End;
            end if;

            Seg_Occupant(Tn_Seg.Seg_Tu) := T.Id;
            Seg_Occupant(Tn_Seg.Seg_St) := No_Train;
            -- do same to wheel but only if it's on turnout
            if Wheel_Pos.Segno = Tn_Seg.Seg_St then
               Wheel_Pos.Segno := Tn_Seg.Seg_Tu;

               if To_Front = Reverse_Pol then   -- v2.1.10
                  -- make mm from end the same:
                  Mm_From_End := Length_Of(Segments(Tn_Seg.Seg_St)) - Wheel_Pos.Mm;
                  Wheel_Pos.Mm := Length_Of(Segments(Tn_Seg.Seg_Tu)) - Mm_From_End;
               end if;

            end if;
            Put_Line("simrail2-step.chk_tnt_entry.. train" & T.Id'img &
            " swapped branches on turnout" & Tnid'img);
         else
            Err := True;
         end if;
      elsif (Tpos = Straight and then
            Tn_Seg.Seg_St /= Pos.Segno) then
         -- if diverging then we've just arrived at the moving part...
         if Tn_Seg.Converging /= (To_Front = Normal_Pol) then
               -- swap over to Seg_St
            Pos.Segno := Tn_Seg.Seg_St;

            if To_Front = Reverse_Pol then   -- v2.1.10
               -- make mm from end the same:
               Mm_From_End := Length_Of(Segments(Tn_Seg.Seg_Tu)) - Pos.Mm;
               Pos.Mm := Length_Of(Segments(Tn_Seg.Seg_St)) - Mm_From_End;
            end if;

            Seg_Occupant(Tn_Seg.Seg_St) := T.Id;
            Seg_Occupant(Tn_Seg.Seg_Tu) := No_Train;
            -- do same to wheel but only if it's on turnout
            if Wheel_Pos.Segno = Tn_Seg.Seg_Tu then
               Wheel_Pos.Segno := Tn_Seg.Seg_St;

               if To_Front = Reverse_Pol then   -- v2.1.10
                  -- make mm from end the same:
                  Mm_From_End := Length_Of(Segments(Tn_Seg.Seg_Tu)) - Wheel_Pos.Mm;
                  Wheel_Pos.Mm := Length_Of(Segments(Tn_Seg.Seg_St)) - Mm_From_End;
               end if;

            end if;
            Put_Line("simrail2-step.chk_tnt_entry.. train" & T.Id'img &
            " swapped branches on turnout" & Tnid'img);
         else
            Err := True;
         end if;
      end if;

      if Err then
         Put_Line("simrail2-step.chk_tnt_entry.. train" & T.Id'img &
         " on turnout" & Tnid'img &
         " derailed:" & Tpos'img);
         T.Crashed := True;
      end if;
   end Check_Turnout_Consistent;

   -- Check_Turnout_Entry_And_Exit
   -- check if train covers the moving part of turnouts in front and rear
   -- segments
   -- post: may modify Cover(*), may set T.Crashed, Turnout_Mutex_Error
   --
   procedure Check_Turnout_Entry_And_Exit is
      Tnid : Turnout_Idx;
      -- Seg : Seg_Index;
      Cen : Float;
      Radius : Float;
      Overlaps : Boolean;
      Entering_Turnout,
      Leaving_Turnout : Turnout_Idx := No_Turnout;
   begin
      Tnid := Segments(T.Front_Pos.Segno).Tnid;
      if Tnid /= No_Turnout then
         -- we assume a turnout gets "sensitive" about an inch
         -- (Turnout_Tolerance) in from each end
         Cen := Length_Of(Segment_Features(T.Front_Pos.Segno))/2.0;
         Radius := Cen - Turnout_Tolerance;   --(v2.1.5) 25.0;
         Check_Central_Occupancy (T.Id, T.Front_Pos, T.Back_Pos,
            Segno=>T.Front_Pos.Segno,
            Intersect=>Cen, Radius=>Radius, Present=>Overlaps );
         if Overlaps and T.Going_Forward then
            if Train_Covering (Turnouts(Tnid)) /= T.Id then
               Entering_Turnout := Tnid;
               Set_Cover(Turnouts(Tnid), T.Id, Turnout_Mutex_Error);
               Check_Turnout_Consistent(Tnid, T.Front_Pos.To_Front,
                  T.Front_Pos, T.Front_Wheel_Pos);  -- may cause crash
            end if;
         elsif not Overlaps and not T.Going_Forward then
            -- we're clear of it
            if Train_Covering (Turnouts(Tnid)) = T.Id then
               Leaving_Turnout := Tnid;
               Set_Cover(Turnouts(Tnid), No_Train, Turnout_Mutex_Error);
            end if;
         end if;
      end if;
      if Turnout_Mutex_Error then
         Put_Line("simrail2.step Train" & T.Id'Img &
            " crash: other train in turnout" & Tnid'Img);
         T.Crashed := True;
         return;
      end if;
      -- similar for other end (todo: cut duplicate code here)
      Tnid := Segments(T.Back_Pos.Segno).Tnid;
      if Tnid /= No_Turnout then
         -- we assume a turnout gets "sensitive" an inch (Turnout_Tolerance)
         -- in from each end
         Cen := Length_Of(Segment_Features(T.Back_Pos.Segno))/2.0;
         Radius := Cen - Turnout_Tolerance;   --(v2.1.5) 25.0;
         Check_Central_Occupancy (T.Id, T.Front_Pos, T.Back_Pos,
            Segno=>T.Back_Pos.Segno,
            Intersect=>Cen, Radius=>Radius, Present=>Overlaps );
         if Overlaps and not T.Going_Forward then
            if Train_Covering (Turnouts(Tnid)) /= T.Id then
               Entering_Turnout := Tnid;
               Set_Cover(Turnouts(Tnid), T.Id, Turnout_Mutex_Error);
               Check_Turnout_Consistent(Tnid, Opposite(T.Back_Pos.To_Front),
                  T.Back_Pos, T.Back_Wheel_Pos);  -- may cause crash
            end if;
         elsif not Overlaps and T.Going_Forward then
            -- we're clear of it
            if Train_Covering (Turnouts(Tnid)) = T.Id then
               Leaving_Turnout := Tnid;
               Set_Cover(Turnouts(Tnid), No_Train, Turnout_Mutex_Error);
            end if;
         end if;
      end if;
      if Turnout_Mutex_Error then
         Put_Line("simrail2.step Train" & T.Id'Img &
            " crash: other train in turnout" & Tnid'Img);
         T.Crashed := True;
         return;
      end if;
      if Entering_Turnout /= No_Turnout then
         --      if Turnout_Occupant(Entering_Turnout) /= No_Train and then
         --         Turnout_Occupant(Entering_Turnout) /= T.Id then
         T.Last_Turnout_Entered := Entering_Turnout;
         --         Turnout_Occupant(Entering_Turnout) := T.Id;
         if Position_Of(Turnouts(Entering_Turnout)) = Middle then
            Put_Line("simrail2.step Train" & T.Id'Img & " derailment: on turnout" &
               Entering_Turnout'Img);
            T.Crashed := True;
            --Crashed := True;
         end if;
      end if;
   end Check_Turnout_Entry_And_Exit;


   procedure Show_Occupied_Error(Seg : Seg_Index) is
   begin
      Put_Line("simrail2.step train" & T.Id'Img &
         " crash: other train" & Seg_Occupant(Seg)'img & " in segment"
         & Seg'img & ", block" & Segments(Seg).Blok'img);
   end Show_Occupied_Error;

   -- Fix_Up_Crossing
   -- pre: Check_For_Crossing has established the parameters
   -- post: array elements Crossing_Occupant(x) for x= The_Crossing, Crossing2
   -- have been adjusted.
   -- If the crossing is already occupied by a different train then T.Crash is set
   -- else T.Over_Crossing is set to The_Crossing
   --
   procedure Fix_Up_Crossing(Over_Crossing : Boolean; The_Crossing, Crossing2 : Crossing_Idx) is
   begin
      if Over_Crossing then
         if Crossing_Occupant(The_Crossing) /= No_Train
             and then Crossing_Occupant(The_Crossing) /= T.Id then
            --Crossing_Mutex_Error := True;
            Put_Line("simrail2.step Train" & T.Id'Img &
               " crash: other train in crossing" & The_Crossing'img);
            T.Crashed := True;
            --Crashed := True;
         else
            Crossing_Occupant(The_Crossing) := T.Id;
            T.Over_Crossing := The_Crossing;
         end if;
         -- special code for tram:  (we avoid storing another id but always deal in pairs)
         if Crossing2 /= No_Crossing then   -- (30/05/08) was 'and then'
            if Crossing_Occupant(Crossing2) /= No_Train
               and then Crossing_Occupant(Crossing2) /= T.Id then
               --Crossing_Mutex_Error := True;
               Put_Line("simrail2.step Train" & T.Id'Img &
                  " crash: other train in crossing" & Crossing2'img);
               T.Crashed := True;
               --Crashed := True;
            else
               Crossing_Occupant(Crossing2) := T.Id;
               --T.Over_Crossing := The_Crossing;
            end if;
         end if;
      elsif T.Over_Crossing /= No_Crossing then
         Crossing_Occupant(T.Over_Crossing) := No_Train;
         T.Over_Crossing := No_Crossing;
         -- special code for tram:
         if Crossing2 /= No_Crossing then
            Crossing_Occupant(Crossing2) := No_Train;
         end if;
      end if;
   end Fix_Up_Crossing;

begin -- Step
   --      Put_Line("train" & T.Id'img & " start of step...");
   if T.Crashed then
      return;
   end if;
   Front_Block := Segments(T.Front_Wheel_Pos.Segno).Blok;
   Rear_Block := Segments(T.Back_Wheel_Pos.Segno).Blok;
   --Put_Line("simrail2-step.Step(Train" & T.Id'Img & " Front_WBlock:" & Front_Wheel_Block'img);

   -- Get Front and rear voltages
   Front_Voltage := Get_Signed_Voltage(Blockdrivers(Front_Block));
   Rear_Voltage := Get_Signed_Voltage(Blockdrivers(Rear_Block));

   --      Put_Line("train" & T.Id'img & " check polarities...");
   -- Check train is not on two different polarities
   -- or jammed on a turnout ...
   Check_For_Voltage_Mismatch(Front_Block, Rear_Block, T.Front_Wheel_Pos, T.Back_Wheel_Pos,
        Front_Voltage, Rear_Voltage, Bad);
   if Bad then
      Put_Line("*** error: Train" & T.Id'Img &
         " has different voltages at front, rear wheels B" &
         Front_Block'Img & " B" & Rear_Block'Img &
         " ("& T.Front_Wheel_Pos.Segno'Img & T.Back_Wheel_Pos.Segno'Img & " )");
      Put(Front_Voltage, 3, 3, 0);
      Put(Rear_Voltage, 3, 3, 0);
      New_Line;
      T.Crashed := True;
      Dump;
      --Crashed := True;
      return;
   end if;
   -- assert Front_Voltage and Rear_Voltage are the same or complements, if one is zero then
   -- both are.
   --
   Calculate_Step(
      Speed         => T.Front_Speed,
      Polr_To_Front => T.Front_Wheel_Pos.To_Front,
      Drive_Speed   => Speed_For (Front_Voltage),
      Elapsed_Time  => Elapsed_Time,     --or To_Duration(Elapsed_Time),
      Step_Mm       => Front_Step);
   --
   --  Debug_Inv := False; -- global

   -- note: this step applies to the wheel positions -- the train ends
   -- could be the other side of inverting joins!

   -- Move front of train.  (Note it might be going backward -- a clumsy
   -- way to code a control system.  Beware!)

   if abs Front_Step > 0.01 then   -- v2.1.4 (18-Jun-08) new calculation
      Going_Forward := (T.Front_Wheel_Pos.To_Front = Normal_Pol) = (Front_Step > 0.0);
      if Going_Forward /= T.Going_Forward then
         -- Ada.Text_Io.Put_Line("train" & T.Id'img & " turned around");
         T.Going_Forward := Going_Forward;
      end if;
   end if;

   --Put("train" & T.Id'img & " advance front..."); Put(Step, 4,1,0);
   Old_Front_Pos := T.Front_Pos;
   Advance(Old_Front_Pos, T.Front_Pos, Front_Step, T.Front_Wheel_Pos);
   if Old_Front_Pos.Segno /= T.Front_Pos.Segno then
      -- we've crossed a segment boundary, is it a new segment?
      if Seg_Occupant(T.Front_Pos.Segno) = No_Train then
                     --T.Going_Forward := True;  --removed 18/06/08
         Seg_Occupant(T.Front_Pos.Segno) := T.Id;
      elsif Seg_Occupant(T.Front_Pos.Segno) = T.Id then
                     --T.Going_Forward := False;  --removed 18/06/08
         Seg_Occupant(Old_Front_Pos.Segno) := No_Train;
      else
         -- entering a segment occupied by another train!
         Show_Occupied_Error(T.Front_Pos.Segno);
      end if;
   end if;

   -- move front wheel position:
   Old_Front_Wheel := T.Front_Wheel_Pos;
   Advance(Old_Front_Wheel, T.Front_Wheel_Pos, Front_Step, Old_Front_Wheel);

   if T.Has_Carriages then
      -- move middle of train (rear of engine)
      Old_Mid_Pos := T.Middle_Pos;
      --Put("train" & T.Id'img & " advance midl..."); Put(Mid_Step, 4,1,0);
      Advance(Old_Mid_Pos, T.Middle_Pos, Front_Step, Old_Front_Wheel);
      if Old_Mid_Pos.Segno /= T.Middle_Pos.Segno and then
         -- we've crossed a segment boundary, is it a new segment?
       Seg_Occupant(T.Middle_Pos.Segno) /= T.Id then
         Put_Line("*** error: simrail2.step Train" & T.Id'Img &
         " has unowned segment between front and rear" &
         T.Middle_Pos.Segno'Img & Segments(T.Middle_Pos.Segno).Blok'Img);
         T.Crashed := True;
         Crashed := True;
         return;
      end if;
      --New_Line;
   end if;

   -- move rear wheel position:
   Old_Back_Wheel:= T.Back_Wheel_Pos;
   Advance(Old_Back_Wheel, T.Back_Wheel_Pos, Front_Step, Old_Front_Wheel);

   -- Move rear of train
   Old_Back_Pos := T.Back_Pos;
   --Put("train" & T.Id'img & " advance back..."); Put(Step, 4,1,0);
   Advance(Old_Back_Pos, T.Back_Pos, Front_Step, Old_Front_Wheel);

   if Old_Back_Pos.Segno /= T.Back_Pos.Segno then
      -- we've crossed a segment boundary, is it a new segment?
      if Seg_Occupant(T.Back_Pos.Segno) = No_Train then
                        --T.Going_Forward := False;  --removed 18/06/08
         Seg_Occupant(T.Back_Pos.Segno) := T.Id;
      elsif Seg_Occupant(T.Back_Pos.Segno) = T.Id then
                        --T.Going_Forward := True;  --removed 18/06/08
         Seg_Occupant(Old_Back_Pos.Segno) := No_Train;
      else
         -- entering a segment occupied by another train!
         Show_Occupied_Error(T.Back_Pos.Segno);
      end if;

      --v2.1.6 debugging train length changes
      --if T.Front_Pos.Segno = T.Back_Pos.Segno then
      --      Put_Line("*** carriage length check caboose entering" & T.Back_Pos.Segno'img);
      --   if abs (T.Orig_Length - abs(T.Front_Pos.Mm - T.Back_Pos.Mm)) > 1.0 then
      --      Put_Line("*************** train length changed!");
      --      Print_Train(T);
      --      Put("Step=");  Put(Front_Step,2,1,0); New_Line;
      --      Crashed := True;
      --   end if;
      --elsif Old_Front_Pos.Segno = Old_Back_Pos.Segno then
      --      Put_Line("*** train length check caboose leaving" & Old_Back_Pos.Segno'img);
      --   if abs (T.Orig_Length - abs(Old_Front_Pos.Mm - Old_Back_Pos.Mm)) > 1.0 then
      --      Put_Line("*************** train length changed!");
      --      Print_Train(T);
      --      Put("Step=");  Put(Front_Step,2,1,0); New_Line;
      --      Crashed := True;
      --   end if;
      --end if;

   end if;
   --         Put_Line("train" & T.Id'img & " check sensors...");

   -- Check if front reflector over sensor...
   Check_Sensor(Segment_Features(T.Front_Pos.Segno), T.Front_Pos);

   -- simrail2/2007: no middle reflectors
   -- Check_Sensor(Segment_Features(T.Middle_Pos.Segno), T.Middle_Pos);

   -- check if rear reflector over sensor...
   Check_Sensor(Segment_Features(T.Back_Pos.Segno), T.Back_Pos);

   -- Check for moving over a turnout and are we the only train there...
   Check_Turnout_Entry_And_Exit;

   -- check main crossing and minor crossovers
                  --Put_Line("train" & T.Id'img & " check crossings...");
   Check_For_Crossing(T.Id, T.Front_Pos, T.Back_Pos, Over_Crossing, The_Crossing);

   if Front_Block = 1 then
      -- it's the tram, entering or leaving a double crossing <<< beware magic nos!
      Fix_Up_Crossing(Over_Crossing, 4, 5);
   else
      Fix_Up_Crossing(Over_Crossing, The_Crossing, No_Crossing);
   end if;


exception
   when E : others =>
      Put_Line("***simrail2.step Train" & T.Id'Img & " Step: died");
      Put_Line(Ada.Exceptions.Exception_Information(E));  --v2.1.6
      T.Crashed := True;
      Crashed := True;
end Step; -- for Simrail2
