-- Testwdg2 : testprogram for Widgets2 (example multi-Sporadic with two
-- start methods)
-- R K Allen, Swinburne Univ Tech.  13-May-03
with Projdefs, Widgets2;  
use Projdefs;
procedure TestWdg2 is

begin
   for I in 1..15 loop
      Widgets2.Start_Run(Widget_Id(i mod 3 + 1), Speed_Type(I mod 7));
      delay 0.25;
      Widgets2.Start_Stop(Widget_Id(i mod 3 + 1));
      delay 0.25;
   end loop;
end TestWdg2;
--
--example output:
-- 2 Req=RUN Speed= 1 Over_Run=FALSE
-- 3 Req=RUN Speed= 2 Over_Run=FALSE
-- 2 Req=STOP Stopping Over_Run=FALSE
-- 1 Req=RUN Speed= 3 Over_Run=FALSE
-- 3 Req=STOP Stopping Over_Run=FALSE
-- 2 Req=RUN Speed= 4 Over_Run=FALSE
-- 1 Req=STOP Stopping Over_Run=FALSE
-- 3 Req=RUN Speed= 5 Over_Run=FALSE
-- 2 Req=STOP Stopping Over_Run=FALSE
-- 1 Req=RUN Stopping Speed= 6 Over_Run=FALSE
-- 3 Req=STOP Stopping Over_Run=FALSE
-- 2 Req=STOP Stopping Over_Run=TRUE
-- 1 Req=STOP Stopping Over_Run=FALSE
-- 3 Req=STOP Stopping Over_Run=TRUE
-- 1 Req=STOP Stopping Over_Run=TRUE
-- 2 Req=RUN Speed= 3 Over_Run=FALSE
-- 3 Req=RUN Speed= 4 Over_Run=FALSE
-- 2 Req=STOP Stopping Over_Run=FALSE
-- 1 Req=RUN Speed= 5 Over_Run=FALSE
-- 3 Req=STOP Stopping Over_Run=FALSE
-- 2 Req=RUN Speed= 6 Over_Run=FALSE
-- 1 Req=STOP Stopping Over_Run=FALSE
-- 3 Req=RUN Speed= 0 Over_Run=FALSE
-- 2 Req=STOP Stopping Over_Run=FALSE
-- 1 Req=RUN Speed= 1 Over_Run=FALSE
-- 3 Req=STOP Stopping Over_Run=FALSE
-- 1 Req=STOP Stopping Over_Run=FALSE 