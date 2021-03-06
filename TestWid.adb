-- Testwid : testprogram for Widget (example Sporadic)
-- R K Allen, Swinburne Univ Tech.  13-May-03
with Projdefs, Widget;
use  Projdefs;
procedure TestWid is

begin
   for J in 0 .. 9 loop
      for I in 1 .. 10 loop
         Widget.Start (Request_Type'Val (I+(10*J)));
 	 delay 0.7;
      end loop;
      delay 5.0;
   end loop;
end TestWid;
--example output:
-- Req=STOP Over_Run=FALSE
-- Req=RUN Over_Run=FALSE
-- Req=STOP Over_Run=FALSE
-- Req=RUN Over_Run=FALSE
-- Req=STOP Over_Run=FALSE
-- Req=STOP Over_Run=TRUE
-- Req=RUN Over_Run=FALSE
-- Req=STOP Over_Run=FALSE
-- Req=RUN Over_Run=FALSE
