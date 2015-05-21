-- Simrail2-Turnout_Pkg.adb  version 2.2.0
--
-- Author: Rob Allen, Swinburne Univ Tech.
-- Version 1.5.2  10-Jul-01 (turnout_pkg separated)
-- Version 1.6.5  17-May-02 (reduce debugging output)
-- Version 2.0.0   9-Jul-07 (version 2)
-- Version 2.0.1  20-Jul-07 (Cover reinstated, some messages)
-- Version 2.0.2  11-Aug-07 (init straight)
-- Version 2.0.3  17-Sep-07 (pos_bit)
-- Version 2.2.0   4-Feb-13 (Init Tflip added, eliminate random failures)
--
-- Note: originally this modelled capacitor-discharge turnout motors.  It was
-- changed to model Tortoise motors by fixing T.charge to 1.0
-- and Tflip to mean 3.5 second.
--
separate (Simrail2)
package body Turnout_Pkg is
   use Raildefs, Dio192defs;

      -- ensure Tflip is appropriate
   procedure Init_Timing (Tick_Interval : in Positive) is
   begin
      if Tick_Interval = 15 or Tick_Interval = 16 then
         Tflip := Tortoise_Time*10 / 156;
      else
         Tflip := Tortoise_Time / Tick_Interval;
      end if;
   end Init_Timing;

   -- allow some random reduction in time for turnouts to move.
   -- Returns initial value for field Turnout.Time that is between
   -- 0 and Uncertainty*Tflip where Tflip is the total required.
      function Random_Start(Max_Fraction : Float) return Integer is
         use Ada.Numerics.Float_Random;
      begin
         return Integer(Float(Tflip) * Max_Fraction * Random(Rng));
      end Random_Start;


      -- Initialise turnout to moving toward Straight
      -- (anywhere from 0% to 100% of the way there)
      procedure Init(T : in out Turnout; Id : in Turnout_Id) is
      begin
         T.Id := Id;
         T.Pos_Bit := Busy;
         --if (Ada.Numerics.Float_Random.Random(Rng) > 0.5) then
         T.State := Go_Straight;
         --else
         --   T.State := Go_Turn;
         --end if;
         T.Time := Random_Start(1.0);

         --T.Charge := 1.0;
      end Init;

--        function Enough(Charge : Float) return Boolean is
--           use Ada.Numerics.Float_Random;
--        begin
--           return (Charge - Uncertainty * Random(Rng)**2  > Threshold);
--        end Enough;

      procedure Tell(
         T : in out Turnout;
         Command : in Dio192defs.Turnout_Drive_Bit;
         Changed : in out Boolean) is
         --
         -- Possibly start turnout moving or recharging.
         -- If at start of movement there is insufficient charge in the
         -- simulated capacitor (0 .. 1.0) then the state will be Is_Stuck.
         -- Similarly for a premature end to the command or a silly reversal
         -- or the error command Pull_Both.
         -- T.Time is set to zero at the start of a movement that will succeed
         -- (Go_Turn, Go_Straight)
         -- Changed returns true iff Position_Of() will return Middle, viz state
         -- has changed from Is_Straight or Is_Turn to something else.
      begin
         --Put_Line("sim.turnout.tell" & T.Id'Img & " Command:" & Command'img);
         Changed := False;
         case T.State is
            when Is_Straight =>
               if Command = Pull_Tu then
                  if T.Cover > 0 then
                     Put_Line("Error: trying to move turnout" & T.Id'Img & " with a train on it!");
                     Trains(T.Cover).Crashed := True;
                     Crashed := True;
                  end if;
                  T.State := Go_Turn;
                  T.Pos_Bit := Busy; -- note change from old railroad
--                    if Enough(T.Charge) then
--                       T.State := Go_Turn;
--                    else
--                       Put_Line("turnout" & T.Id'Img & " random error or not ready");
--                          --& T.Charge'Img);
--                       T.State := Is_Stuck;
--                    end if;
                  Changed := True;
                  T.Time := Random_Start(Uncertainty);  -- might be eg 10% on way
               else -- Pull_St or Pull_None or Pull_Both (error)
                  T.Time := 0;  -- something has started
               end if;

            when Is_Turn =>
               if Command = Pull_St then
                  if T.Cover > 0 then
                     Put_Line("Error: trying to move turnout" & T.Id'Img & " with a train on it!");
                     Trains(T.Cover).Crashed := True;
                     Crashed := True;
                  end if;
                  T.State := Go_Straight;
                  T.Pos_Bit := Busy; -- note change from old railroad
--                    if Enough(T.Charge) then ...
                  Changed := True;
                  T.Time := Random_Start(Uncertainty);
               else -- Pull_Tu (redundant) or Pull_None or Pull_Both (error)
                  T.Time := 0;  -- something has started
               end if;

            when Is_Stuck =>
               if Command = Pull_St or Command = Pull_Tu then
                  if T.Cover > 0 then
                     Put_Line("Error: trying to move turnout" & T.Id'Img & 
                              " with a train on it!");
                     Trains(T.Cover).Crashed := True;
                     Crashed := True;
                  end if;
--                    if Enough(T.Charge) then
--                       T.State := Go_Straight;
--                    else
--                       Put_Line("turnout" & T.Id'Img & " random error" 
--                                  --" or insufficient charge"
--                          & T.Charge'Img & " stays stuck");
--                    end if;
                  T.Time := 0;
                  if Command = Pull_Tu then
                  --    if Enough(T.Charge) then ...
                     T.State := Go_Turn;
                  else
                     T.State := Go_Straight;
                  end if;
               else -- Pull_None or Pull_Both (error, obsolete) stay stuck
                  T.Time := 0;
               end if;

            when Go_Turn =>
               if Command = Pull_St then
                  -- (obsolete) T.State := Is_Stuck;  -- still discharging
                  -- unlike capacitance discharge, tortoise drive can instantly reverse
                  T.State := Go_Straight;
                  T.Time := 0;  -- start from beginning, (not random)
               elsif Command = Pull_Tu then
                  null; -- ignore
--                  T.Charge := 0.0;  -- still discharging
--               elsif Command = Pull_None then
--                  -- premature!!
--                  T.State := Is_Stuck;
--                  T.Time := 0;  -- can start timing recharge
--               else -- Pull_Both (error)
--                  T.State := Is_Stuck;
--                  T.Charge := 0.0;  -- still discharging
               end if;

            when Go_Straight =>
               if Command = Pull_Tu then
                  -- T.State := Is_Stuck;
                  T.State := Go_Turn;
                  T.Time := 0;  -- start from beginning, (not random)
               elsif Command = Pull_St then
                  null; -- ignore
                  --  T.Charge := 0.0;  -- still discharging ...
               end if;
         end case;
--         if Command /= Pull_None then
--            T.Charge := 0.0;  -- assume all lost at once
--            -- or still discharging
--         end if;
         T.Current_Command := Command;
         --if T.Id=1 then Put_Line("... turnout" & T.Id'img & " state=" & T.State'img);
         --end if;
      end Tell;

      procedure Tick(T : in out Turnout;  Changed : in out Boolean) is
         --
         -- advance T.time by one tick (usually 15.6ms)
         -- Must be called at least once BEFORE Tell().
         -- If in motion (Go_Turn, Go_Straight) it might arrive.
         -- (obsolete)If not might recharge capacitor a bit.
         --
      begin
         --if T.Id=1 then Put_Line("Tick turnout" & T.Id'img & " state=" & T.State'img); end if;
--         if T.Current_Command = Pull_None then
--            T.Charge := T.Charge + (1.0 - T.Charge)/ Tau;  -- exponential rise
--         end if;

         Changed := False;
         case T.State is
            when Is_Straight | Is_Turn | Is_Stuck =>
               null;
            when Go_Turn =>
               T.Time := T.Time + 1;
               if T.Time >= Tflip then
                  T.State := Is_Turn;
                  T.Pos_Bit := In_Position;  -- at last, the bit changes
                  Changed := True;
                  T.Time := 0;
               end if;
            when Go_Straight =>
               T.Time := T.Time + 1;
               if T.Time >= Tflip then
                  T.State := Is_Straight;
                  T.Pos_Bit := In_Position;
                  Changed := True;
                  T.Time := 0;
               end if;
         end case;
         --if T.Id=1 then Put_Line("... turnout" & T.Id'img & " state=" & T.State'img); end if;
      end Tick;

      function Position_Bit(T : Turnout) return Turnout_Status_Bit is
      begin
         return T.Pos_Bit;
      end Position_Bit;

      function Position_Of(T : Turnout) return Turnout_Pos is
      begin
         case T.State is
            when Is_Straight => return Raildefs.Straight;
            when Is_Turn =>     return Raildefs.Turned;
            when others =>      return Raildefs.Middle;
         end case;
      end Position_Of;

      function Train_Covering(T : Turnout) return Train_Idx is
      begin
         return T.Cover;
      end Train_Covering;

      procedure Set_Cover(
          T : in out Turnout;
          Train : Train_Idx;
          Error : out Boolean) is
      begin
         if T.Cover /= No_Train and then Train /= No_Train and then T.Cover /= Train then
            Error := True;
         else
            T.Cover := Train;  -- possibly No_Train, meaning not covered
            Error := False;
         end if;
      end Set_Cover;

end Turnout_Pkg;
