-- Widgets2 : an example multi-Sporadic where there are two Start methods
-- with different parameter profiles.  
-- This body uses a variant ("discriminated") record for Item_type because the 
-- two different kinds of request need different parameters.
-- Single-item buffer.
-- R K Allen, Swinburne Univ Tech.  13-May-03, revised 1-Apr-07
with Projdefs, Ada.Text_Io;
use Projdefs;
package body Widgets2 is

   type Op_Type is (Run, Stop);
   -- 
   type Item_Type (Req: Op_Type := Run) is
   record
      case Req is
      when Run =>
         Speed : Speed_Type;
      when Stop =>
         Dummy : Character := ' ';
      end case;
   end record;

   protected type Buffer_Type is
      procedure Start(
         Event : in Item_Type);

      entry Wait_Start(
         Event : out Item_Type;
         Over_Run : out Boolean );
   private
      Item : Item_Type;
      Item_Available,
         Too_Fast : Boolean := False;
   end Buffer_Type;
   ---
   task type Thread_Type (Me : Widget_Id);

   Worker1 : Thread_Type(1);
   Worker2 : Thread_Type(2);
   Worker3 : Thread_Type(3);
   ------------  
   Buffers : array (Widget_Id) of Buffer_Type;

   -------

   procedure Start_Run(
         Id : in Widget_Id;
         Speed : in Speed_Type) is
   begin
      Buffers(Id).Start(Item_Type'(Run, Speed));
   end Start_Run;

   procedure Start_Stop(
         Id : in Widget_Id ) is
   begin
      Buffers(Id).Start(Item_Type'(Stop, ' '));  -- compiler gets confused
   end Start_Stop;                              -- without the 2nd field ' '

   --------- 
   protected body Buffer_Type is
      procedure Start(
            Event : in Item_Type) is
      begin
         if Item_Available then
            Too_Fast := True;
         else
            Item_Available := True;
         end if;
         Item := Event ;  -- ignore old
      end Start;

      entry Wait_Start(
            Event : out Item_Type;
            Over_Run : out Boolean )
            when Item_Available is
      begin
         Event := Item;
         Over_Run := Too_Fast;
         Item_Available := False;
         Too_Fast := False;
      end Wait_Start;
   end Buffer_Type;

   -------
   task body Thread_Type is
      -- state data here maybe
      
      procedure Actual_Run(Speed : in Speed_Type) is
      begin
            Ada.Text_Io.Put(" Speed=" & Speed'Img);  -- debug
      end Actual_Run;
    
      procedure Actual_Stop is
      begin
            Ada.Text_Io.Put(" Stopping");  -- debug
      end Actual_Stop;

      Item : Item_Type;
      Oops : Boolean;
   begin
      loop
         Buffers(Me).Wait_Start(Item, Over_Run=>Oops);
         Ada.Text_Io.Put(Me'img & " Req=" & Item.Req'Img);  --debug
         if Item.Req = Run then -- or case statement
            Actual_Run(Item.Speed);
         else
            Actual_Stop;
         end if;
         Ada.Text_Io.Put_Line(" Over_Run=" & Oops'Img);
         delay 1.0;
         -- NB the above delay statement is for test/demo ONLY.
         -- Normal sporadics do NOT have delays in their bodies.
      end loop;
   end Thread_Type;

end Widgets2;