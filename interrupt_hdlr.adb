-- interrupt package for skeleton test program skel2 for trains --
-- requires Swindows, Simrail2 (vsn 3.0+), Raildefs
-- and Halls2 & Io_Ports for Simrail2
-- Only displays one line for each interrupt.
--
-- Version: 1.0 extracted from skel2 for GNAT2009, 19-Feb-09
-- Version: 1.1 hide protected, 16-Mar-10
-- Version: 1.3 improve display, eg ...5.7.., 31-Jan-2011
-- Version: 1.4 add W_Height to eliminate a magic no, reformat 22-Jun-2011
-- Version: 2.2 write 'I' events to Slogger 19-Mar-2013
-- Version  3.0 25-Feb-2015 procedure no longer protected
--
-- Copyright: Dr R. K Allen, Faculty of Sc Eng Tech, Swinburne Uni Tech
--
with Ada.Integer_Text_Io;
with Swindows;  use Swindows;
with Unsigned_Types;  use Unsigned_Types;
with
   Raildefs,
   Int32defs,
   Halls2;
use Raildefs;
with Slogger;  --v2.2

with Train;



package body Interrupt_Hdlr is

   W_Interrupts : Swindows.Window;
   -- assumed to be full width, ie 78 columns inside, and W_Height lines inside
   W_Height : constant Natural := 14;  -- beware: must conform to main program!

   package Iio renames Ada.Integer_Text_Io;

   subtype String8 is String(1..8);  -- cant be declared in a protected.

   ----protected Interrupt is

   procedure Analyze (
         Offset : in     Raildefs.Sensor_Idx;     -- 0 or 32
         Value : in     Raildefs.Four_Registers );-- vsn 2.0.5
   -- this procedure conforms to Raildefs.Proc4_Access

   ----private

   Number : Natural := 1;  -- a serial number
   Line_Number : Natural := 1;  -- within the Swindows window

   Fg : Swindows.Color := Swindows.White;
   Sensor_Line : String(1..78) :=
      (1..4|13|22|31|40|49|58|67|76..78 => ' ', others => '.');

   function To_String (
         Val        : Unsigned_8;
         LSB_Sensor : Raildefs.Sensor_Idx)
         return String8 ;

   ----end Interrupt;

   procedure Init (
         Win : in     Swindows.Window) is
   begin
      W_Interrupts := Win;
   end Init;

   procedure Install is -- added v1.1
   begin
      Halls2.Install_Int_Handling(Analyze'access);  --was Interrupt.Analyze
   end Install;

   type Sensor_Array is array (Integer range 0..3) of Int32defs.Sensor_Register;
   Raildef_sensor_Array: Sensor_Array;

   -- Analyze (Offset, Value)
   --
   -- Receive 40-bit data captured from one of the two INT32 cards
   -- (by Halls2, or simulated in Simrail2) and pass sensor changes on to
   -- train control code.  Also display as one line in W_Interrupts.
   --
   -- This version only displays; it takes too much screen space.
   -- Todo: add variable(s) to remember Value from previous call;
   -- add code to check for sensor bit changes and for any change call
   -- an async (ASER) operation in train controller.
   --
   procedure Analyze (
         Offset : in     Raildefs.Sensor_Idx;  -- 0 or 32
         Value  : in     Raildefs.Four_Registers ) is
      --
      use Int32defs;

      procedure Increment_Line is -- v1.4
         -- provides rolling colour change (much more efficient than
         -- scrolling)
      begin
         Line_Number := (Line_Number mod W_Height) + 1;
         if Line_Number = 1 then
            -- change colour
            if Fg = White then
               Fg := Yellow;
            else
               Fg := White;
            end if;
         end if;
      end Increment_Line;

      Value_sensor     : Int32defs.Sensor_Register;
      sensor_index: Raildefs.Sensor_Idx;


   begin
      -- if logging is enabled (it is by default) write an 'I' event.
      -- This could be suppressed entirely, as Simrail2 v2.2.1 and HW Halls2 v2.6
      -- send 'i' events.  'i' to 'I' measures interrupt queue latency, but
      -- this is meaningless for simrail2.
      if Slogger.On then
         Slogger.Send_Event('I', Integer(Offset), Value);
      end if;

      -- assemble 78-char string:
      Iio.Put(Sensor_Line(1..3), Number);
      -- display sensor bytes and turnout nibble in binary ('.'=1)
      if Offset = 0 then -- sensors 1..32
         Sensor_Line( 5..12) := To_String(Value(0), 1);
         Sensor_Line(14..21) := To_String(Value(1), 9);
         Sensor_Line(23..30) := To_String(Value(2), 17);
         Sensor_Line(32..39) := To_String(Value(3), 25);
      else  -- sensors 33..64
         Sensor_Line(41..48) := To_String(Value(0), 33);
         Sensor_Line(50..57) := To_String(Value(1), 41);
         Sensor_Line(59..66) := To_String(Value(2), 49);
         Sensor_Line(68..75) := To_String(Value(3), 57);
      end if;
      -- Note: 35 chars retained from previous call.

      -- last byte is for debugging simrail or Halls2:
      if Value'Last = 4 then  -- (a bit of future-proofing)
         Iio.Put(Sensor_Line(76..78), Integer(Value(4)));  -- added v.1.5.2
         -- 0..255 a pity not hex but no room.
      end if;
      Put_Line(W_Interrupts, Sensor_Line, Line_Number, Foreground=>Fg);
      Number := (Number + 1) mod 1000;  -- serial number
      Increment_Line;


      --addition
      for I in Integer range 0..3 loop
         Value_sensor  :=Unsigned_8_To_Sensor_Register(Value(I));

         for J in Sensor_Idx range 0..7 loop
            if Value_sensor(J)/= Raildef_sensor_Array(I)(J) then
               --change
               sensor_index:= Sensor_ID(1 + 8*I);
               if Offset /=0 then
               	sensor_index:=sensor_index+32;
               end if;
               Train.Recive_sensor_event(J + sensor_index,Value_sensor(J)); --inform

               Raildef_sensor_Array(I)(J):=Value_sensor(J);--change memory
            end if;
         end loop;

      end loop;


      --end addition



   exception
      when Constraint_Error =>
         Put_Line(W_Interrupts, "int constraint", Line_Number);
         Increment_Line;  -- v1.4
      when Tasking_Error =>
         Put_Line(W_Interrupts, "int tasking", Line_Number);
         Increment_Line;  -- v1.4
      when others =>
         Put_Line(W_Interrupts, "int others", Line_Number);
         Increment_Line;  -- v1.4
   end Analyze;

   function To_String (
         Val        : Unsigned_8;
         LSB_Sensor : Raildefs.Sensor_Idx)
         return String8 is
   -- Example:
   -- for only sensor18 on (Val=>2,LSB_Sensor=>17) returns ".8......"
      use Int32defs;
      V      : Sensor_Register := Unsigned_8_To_Sensor_Register (Val);
      Result : String8         := "........";
   begin
      for I in Sensor_Idx range 0..7 loop  -- note display LSB on left
         if V(I) = Raildefs.On then
            Result(Integer(I)+1) :=
               Character'Val(48 + Integer(I + LSB_Sensor) mod 10);
         end if;
      end loop;
      return Result;
   end To_String;



   --end Interrupt;

end Interrupt_Hdlr;
