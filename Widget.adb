-- Widget : an example Sporadic for RTP lecture
-- It has single-item buffer and a worker that delays 1 second.
-- R K Allen, Swinburne Univ Tech.  orig 25-Mar-01 revised 13-May-03
with Projdefs, Ada.Text_Io, Exec_Load, Ada.Calendar;
use Projdefs;
package body Widget is

   type Request_rec is
   record
      Requesttype_ID : Request_type;
      used: Boolean;
      end record;

   N : constant := 4;
   type Index_Type is mod N;
   type Request_Array is array(Index_Type) of Request_rec;

   protected Buffer is
      procedure Start(
         Request: in Request_Type);

      entry Wait_Start(
         Request: out Request_Type;
         Over_Run : out Boolean );
   private
	Items : Request_Array;
   Count :Integer := 0;
   In_point, out_point : Index_Type := 0;
   end Buffer;
   ---
   task Worker_Thread;

   Start_time : Ada.Calendar.Time:= Ada.Calendar.Clock;
    procedure Sporadic_Op(Request: in Request_Type) is
      use Ada.Calendar; -- for  '-'
   begin
      Ada.Text_IO.Put(Duration'Image(Ada.Calendar.Clock - Start_Time));
      Ada.Text_Io.Put_Line("Spor Req=" & Request'Img);
      Exec_load.Eat(1.0);
      Ada.Text_IO.Put_Line(Duration'Image(Ada.Calendar.Clock - Start_Time)& " Done");
     end Sporadic_Op;
   ------------

   procedure Start(
         Request: in Request_Type) is
   begin
      Buffer.Start(Request);
   end Start;

   ---------
   protected body Buffer is
      procedure Start(
                      Request: in Request_Type) is
      req : Request_rec;
      begin
         Ada.Text_Io.Put_Line("IN " & Count'img);
	req.Request_ID := Request;
      Items(In_point) := req;
      In_point:= In_point+1;
      Count := Count +1;
      end Start;

      entry Wait_Start(
            Request: out Request_Type;
            Over_Run : out Boolean )
            when Count > 0 is
      begin
          Ada.Text_Io.Put_Line("Out" & Count'img);
         Request := Items(Out_point).Request_ID;
      Out_point := Out_point +1;
         Count := count -1;
         Over_Run:=False;
      end Wait_Start;
   end Buffer;

   -------
   task body Worker_Thread is
      Req : Request_Type ;
      Oops : Boolean;
   begin
      loop
         Buffer.Wait_Start(Request=>Req, Over_Run=>Oops);

         --if(Oops=true) -- alwasy the last of a burst?
         --then
            Ada.Text_Io.Put_Line(" Req=" & Req'Img
                                    & " Over_Run=" & Oops'Img);
         --end if;
      --Sporadic_Op(Req);
      delay 1.0;
         -- NB the above delay statement is for test/demo ONLY.
         -- Normal sporadics do NOT have delays in their bodies.
         -- The relative time intervals between here and the test harness
         -- are simply to demonstrate one or more over-run occurrences.
      end loop;
   end Worker_Thread;



end Widget;
