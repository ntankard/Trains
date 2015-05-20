-- Simrail2-Segment_Feature_Pkg.adb  version 2.2.1
--
-- Author: Rob Allen, Swinburne Univ Tech.
-- Version 2.2.1  6-Feb-2013 (pkg separated)
--
separate(Simrail2)
package body Segment_Feature_Pkg is
      use Simtrack2;

      procedure Add_Sensor (
            Sb : in out Segment_Feature;
            Id : in     Sensor_Id;
            Mm : in     Float            -- forced onto end if too long.
            ) is -- called at setup time
      begin
         Sb.Num_Sensors := Sb.Num_Sensors + 1;
         if Sb.Num_Sensors > Max_Segment_Sensors then
            -- trying to add too many
            Put_Line("sim.Segment_Feature: too many sensors added to segment");
            raise Constraint_Error;
         else
            Sb.Sensor_Ident(Sb.Num_Sensors) := Id;
            Sb.Mm_Into_Block(Sb.Num_Sensors) := Mm;
         end if;
      end Add_Sensor;

      -- train calls Check_Sensor after a step (will only need to check
      -- max of 5 sensors)
      -- Modifies Temp_Sensor_States  -- on switches on
      procedure Check_Sensor (
            Sb  : in     Segment_Feature;
            Pos : in     Train_Position   ) is
      --
         Half_Width : Float := Reflector_Width/2.0;
      begin
         for N in 1 .. Sb.Num_Sensors loop -- check each sensor
            if (Pos.Mm < Sb.Mm_Into_Block(N) + Half_Width) and
               (Pos.Mm > Sb.Mm_Into_Block(N) - Half_Width) then
               Temp_Sensor_States(Sb.Sensor_Ident(N)) := On;
               --            elsif (Pos.Mm < Sb.Mm_Into_Block(N) + Reflector_Width) and
               --                  (Pos.Mm > Sb.Mm_Into_Block(N) - Reflector_Width) then
               --               Temp_Sensor_States(Sb.Sensor_Ident(N)) := Sensor_Off;
            end if;
         end loop;
      end Check_Sensor;


      procedure Check_Central_Occupancy (
            Tid     : in     Train_Id;
            Fpos,
            Bpos    : in     Train_Position;
            Segno     : in     Seg_Index;
            Intersect : in     Float;
            Radius    : in     Float;
            Present   :    out Boolean      ) is
         -- this proc is written for the main diagonals
         Lo : Float := Intersect - Radius;
         Hi : Float := Intersect + Radius;
      begin
         Present := False;
         if Seg_Occupant(Segno) = Tid then
            Present := True;
            -- but check a bit more...
            if Fpos.Segno = Segno then
               if Fpos.To_Front = Normal_Pol then
                  if Fpos.Mm < Lo  -- havent reached it yet
                        or else (Fpos.Mm > Hi and then
                        Bpos.Segno = Segno and then Bpos.Mm > Hi) -- both ends past
                        then
                     Present := False;
                  end if;
               else -- reverse polarity (moving downward in mm)
                  if Fpos.Mm > Hi  -- havent reached it yet
                        or else (Fpos.Mm < Lo and then
                        Bpos.Segno = Segno and then Bpos.Mm < Lo) -- both ends past
                        then
                     Present := False;
                  end if;
               end if;
            elsif Bpos.Segno = Segno then
               if Bpos.To_Front = Normal_Pol then
                  if Bpos.Mm > Hi then -- already past
                     Present := False;
                  end if;
               else -- reverse polarity (moving downward in mm)
                  if Bpos.Mm < Lo then  -- already past
                     Present := False;
                  end if;
               end if;
            end if;
         end if;
      end Check_Central_Occupancy;

      procedure Check_For_Crossing (
            Tid     : in     Train_Id;
            Fpos,
            Bpos    : in     Train_Position;
            Present :    out Boolean;
            Which   :    out Crossing_Idx    ) is
         --
         -- algorithm for main crossing (number 1):
         -- if both ends of the train are on a diagonal block then
         -- the train is over the crossing UNLESS both ends are before
         -- the crossing or after the crossing.
         -- Assume Fpos is the current front (not necessarily the loco).
         --
         -- For crossovers (2,3) consider occupied if the train is over
         -- any part of a diagonal segment.
         --
         -- 28/05/08: added code for tram crossings at client, generalised code
         -- into Check_Central_Occupancy

         Critical_Radius : constant := 25.0; -- <<<<<< need to measure

      begin
         Present := False;
         Which := No_Crossing;
         for C in 1..2*Num_Crossings loop
            if Crossing_Points(C) > 0.0 then
               Check_Central_Occupancy(Tid, Fpos, Bpos,
                  Crossing_Segments(C), Crossing_Points(C), Critical_Radius, Present);
            elsif Seg_Occupant(Crossing_Segments(C)) = Tid then
               Present := True;
            end if;
            if Present then
               Which := Crossing_Idx((C+1)/2);
               return;
            end if;
         end loop;
      end Check_For_Crossing;


      -- Simrail2: no longer need this - turnout occupancy is done by tracking
      -- entering and leaving.
      --      procedure Check_For_Turnout (
      --            Fpos,
      --            Bpos    : in     Train_Position;
      --            Present :    out Boolean;
      --            Which   :    out Turnout_Idx     ) is
      --      begin
      --         Present := False;
      --         Which := 0;
      --         --      -- assert the relevant segments are simple (not Kind=Aturnout)
      --
      --      end Check_For_Turnout;

      function Length_Of (
            Sb : in     Segment_Feature )
        return Float is
      begin
         return Sb.Length;
      end Length_Of;

      procedure Set_Length (
            Sb : in out Segment_Feature;
            L  : in     Float            ) is
      begin
         Sb.Length := L;
      end Set_Length;

end Segment_Feature_Pkg;
