-- revised for test1602: time simtrack operations
-- 11-Mar-02 (orig 22-Apr-00)
separate(Simrail2)
   procedure Test is
      use Ada.Float_Text_Io;
      Old_Frontpos1, Old_Backpos1 : Train_Position;
      Start, Now : Time;
      Dur : Time_Span;  -- was Duration;
      Elapsed : Float;
   begin
      --null;
      abort Worker;
      Start := Clock;
      for i in 1..1000 loop
         -- redraw train       
         Old_Frontpos1 := Trains(1).Front_Pos;
         Old_Backpos1  := Trains(1).Back_Pos;
         Simtrack2.Display.Draw_Train(False, Old_Backpos1, Old_Backpos1,
             Old_Frontpos1, Simtrack2.Display.Track_Color);
         Simtrack2.Display.Draw_Train(False, Trains(1).Back_Pos, Trains(1).Back_Pos,
             Trains(1).Front_Pos, Simtrack2.Display.Train_Color(1));
      end loop;
      Now := Clock;
      Dur := Now - Start;
      Elapsed := Float(To_Duration(Dur));
      Put(Elapsed, Exp=>0, Aft=>2);  Put_Line("s for 1000 train redraws");
      Start := Clock;
      for i in 1..1000 loop
         -- one CAB
         Block_Changed(3) := True;
         Update_Cab_Dac_Display;
      end loop;
      Now := Clock;
      Dur := Now - Start;
      Elapsed := Float(To_Duration(Dur));
      Put(Elapsed, Exp=>0, Aft=>2);  Put_Line("s for 1000 CAB redraws");
      Start := Clock;
      for i in 1..1000 loop
         -- one CAB
         Dac_Changed(1) := True;
         Update_Cab_Dac_Display;
      end loop;
      Now := Clock;
      Dur := Now - Start;
      Elapsed := Float(To_Duration(Dur));
      Put(Elapsed, Exp=>0, Aft=>2);  Put_Line("s for 1000 DAC redraws");
   end Test;