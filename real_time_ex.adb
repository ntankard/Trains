with Ada.Text_Io, Ada.Float_Text_Io, Ada.Real_Time;
use  Ada.Text_Io, Ada.Float_Text_Io, Ada.Real_Time;
procedure Real_Time_Ex is 
   To,  
   T  : Time;  
   Dt,  
   Ts : Time_Span;  
   So,  
   S  : Seconds_Count;  
   -- integer-like
   Micro_Sec : Time_Span                   := Microseconds (1);  
   D         : Duration;  
   Mics      : Integer range 0 .. 1000_000;  
   Tf        : Float;  
begin
   To := Clock;
   Split(To, So, Ts);
   delay 2.0;
   T := Clock;
   --   Dt := T – T0;  etc or
   Split(T, S, Ts);
   S := S - So;  --    >=0
   Mics := Ts/Micro_Sec; -- 0..999999
   Put_Line(S'Img & "s " &
      Mics'Img & "us");
   -- eg
   --   167s  46001us
   -- for
   --  167.046001
   --or
   Tf := Float(S) + Float(Mics)*0.000_001;
   Put(Tf, 5,4,Exp=>0);
   New_Line;
   --  167.0460
end Real_Time_Ex;