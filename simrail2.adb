-- Simrail2.adb  version 2.2
--
-- Simulate all hardware associated with the railroad including the two
-- interface cards.  This package is meant to be called by student-written
-- control software via the Simrail2 version of Io_Ports.
-- Simulated interrupts from the simulated interrupt card are by call-back
-- to a procedure that takes an array of bytes parameter.  This call back nust
-- be registered using procedure Install_Int_Handling.
--
-- Architecture:
-- *********************************************************
-- *    WARNING:  THIS CODE IS NOTHING LIKE HRT-HOOD STYLE.*
-- *              DO NOT USE IT AS A MODEL!!!!             *
-- *********************************************************
-- Commands to the various IO cards are queued into a protected
-- object and the queue is emptied every cycle (normally 10ms) by a task
-- Worker.  There are no other tasks or protected objects; all railroad
-- objects are realised as ADT objects in global arrays.  Internal packages
-- define the ADTs.  Those objects with time behaviour have procedure
-- Tick which is called every cycle (trains have procedure Step).
-- The display is supported by package Simtrack2 which uses AdaGraph.
-- The overall algorithm within a cycle is
-- (1) tick all turnouts, trains and blockdrivers (in that order)
-- (2) empty the command buffer
-- (3) update the display
-- (4) perform interrupt call-back if a sensor has changed.
--
-- Author: Rob Allen, Swinburne University of Technology
--
-- Revision history:
--
-- Based on simrail.adb v 1.9.9 11 Mar 2007 (orig 1999)
-- version 2.0    8-Jul-2007: First version simrail2
-- version 2.0.1 16 Aug 2007: use restructured simtrack2
-- version 2.0.2 18 Sep 2007: fix bug in code generation, proc Switch,
--                            improve Dump
-- version 2.0.3 25 Feb 2008: raildefs and simdefs2 replace Simdefs2;
--                            Dio192defs, Int32defs, Dda06defs replace
--                            Sim* equivalents; improve dump format.
-- version 2.0.4  2 Mar 2008: int32 simulation, as two 32-bit cards
-- version 2.0.5  7 Mar 2008: for revised Dio192defs.Block_Register and
--                            introduced _Index subtypes for register arrays
-- version 2.0.6  8-Mar-08 use Sensor_Bit instead of Boolean
-- version 2.0.7 13-Mar-08 simtrack2-display color fixed
-- version 2.0.8  3-Apr-08 changed an array index type in Dda06defs, fixed
--                         bug in DDA06 code, removed redundant crossing
--                         code and turnout occupancy
-- version 2.0.9 28-Apr-08 removed debug print in Write_Dda06; simtrack2 v2.4
-- version 2.1.0 13-May-08 redraw trains if a sensor fires
-- version 2.1.1: 21-May-2008 generalised Reset for up to 4 trains
-- version 2.1.2: 28-May-2008 fix bugs in crossing and generalised for
--                            turnout occupancy
-- version 2.1.3: 30-May-2008 fix bug in Step.Fix_Up_Crossing
-- version 2.1.4: 18-Jun-2008 refix bug in Step for turnout/crossing
--                            occupancy when trn backs out
-- version 2.1.5  19-Jun-2008 use constant Simdefs2.Turnout_Tolerance in Step
--                            turnout occupancy and derailment checks
-- version 2.1.6   3-Jul-2008 fix trains changing length when reversed over an
--                            inverting join (long-standing bug);
--                            display sensor numbers
-- version 2.1.7   4-Aug-2008 allow turnout changeover when train at diverging
--                            end not over moving part Step.
-- version 2.1.8  31-Mar-2011 display whistle, bell changes (train ignored);
--                            reduce warnings;
--                 5-Apr-2011 improve interrupt dump display, more command
--                            history via param
--               7,8-Apr-2011 fix das08 base address (310), invert sound logic
-- version 2.1.9  25-Jan-2013 change simulated interrupt debug byte to match --                            Halls2 v2.4
-- version 2.2.0 1,4-Feb-2013 fix turnout timing, eliminate random failures
--                            but have ~10% random variation in move time.
-- version 2.2.1   6-Feb-2013 fix comments; separate Segment_Feature_pkg;
--                 7-Feb-2013 fix bug in interrupt history
-- version 2.2.2   3-Aug-2013 fix bug on entering crossover in reverse with
--                            front overshoot on moving part; decrease coasting
--                            when carriages (all in simrail-step.adb)
-- version 2.3.1   9-Apr-2015 improve sound display, voltages 0.00-10.0
--
-- To do:
--   1. Das08 simulation (sound part done).
--   2. Individual crash messages
--   3. Tune dynamics to 2008 hardware
--
with Raildefs;  use Raildefs;
with Dio192defs;
with Int32defs;
with Dda06defs;

with Simdefs2;  use Simdefs2;
with Simtrack2;
with Simtrack2.Display;

with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Float_Text_Io;  -- for Dump
with Ada.Integer_Text_Io;  -- for Dump
with Ada.Numerics.Float_Random;
with Ada.Real_Time;  -- was Calendar; 15-Nov-99
use  Ada.Real_Time;
with Ada.Exceptions;  -- v2.1.6
with Ada.Unchecked_Conversion;  -- v2.1.9

package body Simrail2 is
   use Unsigned_Types;   -- v1.8+

   Version_String : constant String := "2.3.1";

   function Version return String is
   begin
      return Version_String;
   end Version;

   package Dur_Io is new Ada.Text_Io.Fixed_Io(Duration);

   Start_Time,
   Now        : Ada.Real_Time.Time := Clock; -- 15-Nov-99
   procedure Timestamp is
   begin
      Dur_Io.Put(To_Duration(Now - Start_Time), Exp=>0, Fore=>4, Aft=>3);
   end Timestamp;

   -- Ms : constant Ada.Real_Time.Time_Span := Milliseconds (1);
   -- was Duration := 0.001;
   type Ticker_Type is mod 100_000_000;   -- v1.7 was 10_000
   Current_Tick : Ticker_Type := 0;
   -- used for timestamps in Buffer (for Dump)
   -- also (version 1.5.2) in "interrupt" callback

   subtype String8 is String(1..8); -- common type for dumps

   -------------------------------------------------------------------
   --        Interface Variables
   -------------------------------------------------------------------
   -- These are the registers on the cards used by the simulation
   -- Io_Ports will read these registers via the relevant functions
   -- The "old" vars are for deciding whether to raise interrupts
   --
   -- INT32:
   subtype Sensor_Register_Index is Sensor_Idx range 0 .. (Num_Sensors / 8 - 1);  -- 0..7
   type Sensor_Register_Array is array (Sensor_Register_Index) of Int32defs.Sensor_Register;
   Sensor_Regs,
   Old_Sensor_Regs : Sensor_Register_Array := (others => Int32defs.All_Off);

   -- Turnout status registers of CIO-DIO192 card:
   subtype Turnout_Register_Index is Turnout_Idx range 0 .. 2;
   type Turnout_Register_Array is array (Turnout_Register_Index) of Dio192defs.Turnout_Status_Register;
   Turnout_Status_Regs : Turnout_Register_Array := (others => Dio192defs.All_In_Position);

   -- Turnout drive registers of CIO-DIO192 card:
   type Turnout_Drive_Array is array (Turnout_Register_Index) of Dio192defs.Turnout_Drive_Register;
   Turnout_Drive_Regs : Turnout_Drive_Array := (others => Dio192defs.Turnout_Drive_Init);

   -- Block control registers of CIO-DIO192 card:
   subtype Block_Register_Index is Block_Idx range 0 .. 11;
   type Block_Register_Array is array (Block_Register_Index) of Dio192defs.Block_Register;
   Block_Regs : Block_Register_Array := (others => Dio192defs.Zero_Normal);

   -- DAC registers of CIO-DDA06/Jr card:
   type Dac_Register_Array is array (Dac_Id) of Unsigned_8;
   Dac_Low_Regs  : Dac_Register_Array := (others => 0);
   Dac_High_Regs : Dac_Register_Array := (others => 0);

   -- Card config/control registers:
   Cio192_Pctl1 : Unsigned_8 := 255;
   Cio192_Qctl1 : Unsigned_8 := 255;
   Cio192_Pctl2 : Unsigned_8 := 255;
   Cio192_Qctl2 : Unsigned_8 := 255;
   Cio192_Pctl3 : Unsigned_8 := 255;
   Cio192_Qctl3 : Unsigned_8 := 255;

   -- Sound control:
   Sound_Address : constant Unsigned_16 := 16#313#;  -- (DAS08.PA)
   -- the following is a kludge: students should do better!
   Sound_Reg     : Int32defs.Sensor_Register := (others => Raildefs.On);
   Sound_On : constant Raildefs.Sensor_Bit := Raildefs.On;  -- no inversion 8-Apr-11

   ----Interrupt vars:
   --
   -- controls whether a fake interrupt will be sent
   Interrupts_Enabled : Boolean := False;
   Sensors_Changed : Boolean := False;

   Crashed            : Boolean := False;
   -- set on crash or derailment, clear by Reset.
   --The_Handler        : Proc_Access;  -- removed v2.0
   The_Analyzer : Proc4_Access;
   -- added in vsn 1.5
   -- these will be modified by the following Write procedures
   --
   Cio192_Changed : Boolean;
   -- to optimise calls to Simtrack2
   ------------------------------------------------------------------------
   -- buffer of Write calls.
   -- Worker empties this buffer on each clock tick.
   type Array_8 is array (Integer range <>) of Unsigned_8;
   type Array_16 is array (Integer range <>) of Unsigned_16;
   type Tick_Array is array (Integer range <>) of Ticker_Type;
   Buffer_Size : constant := 50;

   protected  Command_Buffer is

      entry Add (
            Address : in     Unsigned_16;
            Value   : in     Unsigned_8   );
      entry Remove (
            Address :    out Unsigned_16;
            Value   :    out Unsigned_8   );
      function Is_Empty return Boolean;
      procedure Dump (
            N : Positive := 20 );
      procedure Reset;

   private
      -- also the local vars of the task Buffer:
      A8 : Array_8 (1 .. Buffer_Size);
      A16 : Array_16 (1 .. Buffer_Size) := (others => 0);
      A_Tick : Tick_Array (1 .. Buffer_Size); -- for debugging
      I : Integer := 1;
      J : Integer := 1;
      Count : Integer := 0;
   end Command_Buffer;


   function Mnemonic (   -- called by Command_Buffer.Dump
         Addr : Unsigned_16 )
     return String is
      Result : String (1 .. 9);
   begin
      if Addr >= Dio192defs.Base_Address1 and Addr <= Dio192defs.Base_Address1 + 7 then
         case Addr - Dio192defs.Base_Address1 is
            when 0=>
               Result := " dio-PA1 ";
            when 1=>
               Result := " dio-PB1 ";
            when 2=>
               Result := " dio-PC1 ";
            when 3=>
               Result := "dio-Pctl1";
            when 4=>
               Result := " dio-QA1 ";
            when 5=>
               Result := " dio-QB1 ";
            when 6=>
               Result := " dio-QC1 ";
            when 7=>
               Result := "dio-Qctl1";
            when others=>
               null;
         end case;
      elsif Addr >= Dio192defs.Base_Address2 and Addr <= Dio192defs.Base_Address2 + 7 then
         case Addr - Dio192defs.Base_Address2 is
            when 0=>
               Result := " dio-PA2 ";
            when 1=>
               Result := " dio-PB2 ";
            when 2=>
               Result := " dio-PC2 ";
            when 3=>
               Result := "dio-Pctl2";
            when 4=>
               Result := " dio-QA2 ";
            when 5=>
               Result := " dio-QB2 ";
            when 6=>
               Result := " dio-QC2 ";
            when 7=>
               Result := "dio-Qctl2";
            when others=>
               null;
         end case;
      elsif Addr >= Dio192defs.Base_Address3 and Addr <= Dio192defs.Base_Address3 + 7 then
         case Addr - Dio192defs.Base_Address3 is
            when 0=>
               Result := " dio-PA3 ";
            when 1=>
               Result := " dio-PB3 ";
            when 2=>
               Result := " dio-PC3 ";
            when 3=>
               Result := "dio-Pctl3";
            when 4=>
               Result := " dio-QA3 ";
            when 5=>
               Result := " dio-QB3 ";
            when 6=>
               Result := " dio-QC3 ";
            when 7=>
               Result := "dio-Qctl3";
            when others=>
               null;
         end case;
      elsif Addr >= Dda06defs.Base_Address and Addr <= Dda06defs.Base_Address + 7 then
         case Addr - Dda06defs.Base_Address is
            when 0=>
               Result := "dda06-1lo";
            when 1=>
               Result := "dda06-1hi";
            when 2=>
               Result := "dda06-2lo";
            when 3=>
               Result := "dda06-2hi";
            when 4=>
               Result := "dda06-3lo";
            when 5=>
               Result := "dda06-3hi";
            when 6=>
               Result := "dda06-4lo";
            when 7=>
               Result := "dda06-4hi";
            when others =>
               Result := "dda06+" & Addr'Img;
         end case;
      elsif Addr = Sound_Address then
               Result := "das08-PA ";
      else
         Ada.Integer_Text_Io.Put(Result, Integer(Addr));
      end if;
      return Result;
   end Mnemonic;

   protected  body Command_Buffer is

      entry Add (
            Address : in     Unsigned_16;
            Value   : in     Unsigned_8   ) when Count < Buffer_Size is
      begin
         A16(I) := Address;
         A8(I) := Value;
         A_Tick(I) := Current_Tick;
         I := I mod Buffer_Size + 1;
         Count := Count + 1;
      end Add;

      entry Remove (
            Address :    out Unsigned_16;
            Value   :    out Unsigned_8   ) when Count > 0 is
      begin
         Address := A16(J);
         Value := A8(J);
         J := J mod Buffer_Size + 1;
         Count := Count - 1;
      end Remove;

      function Is_Empty return Boolean is
      begin
         return Count = 0;
      end Is_Empty;

      procedure Dump (
            N : Positive := 20 ) is
         -- print last N commands plus unprocessed commands if any.
         -- Effectively A(J - N .. J + (Count-1) ) with cyclic indexing
         use Ada.Integer_Text_Io;
         Dump_Size : Integer := N + Count;
         Idump     : Integer;

         function To_String_Binary(Val : Unsigned_8) return String8 is
            use Int32defs;
            V : Integer := Integer(Val);
            Result : String8 := "........";
         begin
            for I in reverse 1..8 loop  -- note display LSB on Right
               Result(I) := Character'Val(48 + (V mod 2));
               V := V / 2;
            end loop;
            return Result;
         end To_String_Binary;

      begin
         Put_Line("previous commands (index  tick: address value):");
         if Dump_Size > Buffer_Size then   -- fix 22/08/2007 (!)
            -- will display the lot
            Dump_Size := Buffer_Size;      -- fix 22/08/2007 (!)
            Idump  := I;
         else   -- start N before J (cyclic)
            Idump  := (J - N + Buffer_Size - 1) mod Buffer_Size + 1;  -- fix 22/08/2007 (!)
         end if;
         for It in 1 .. Dump_Size loop
            if A16(Idump ) /= 0 then  -- ignore never used elements
               Put(Idump, 2);
               Put(Integer(A_Tick(Idump)), 9);
               Put(": " & Mnemonic(A16(Idump)) );
               Put("    " & To_String_Binary(A8(Idump)) );
               New_Line;
               --else array element never used
            end if;
            Idump  := Idump mod Buffer_Size + 1;
            if Idump = J then
               Put_Line("----" & Count'Img &
                  " commands still in queue ----");
            end if;
         end loop;
         Put_Line("end of command queue");
      end Dump;

      procedure Reset is
      begin
         I := 1;
         J := 1;
         Count := 0;
      end Reset;

   end Command_Buffer;

   ------------------------------------------------------------------------
   -- 9 element version of Raildefs.Four_Registers
   type Eight_Registers is array (0..8) of Unsigned_Types.Unsigned_8;
     -- last element is for debugging (valid simrail only)
     -- cf Sensor_Register_Array which is 0..7

   -- circular array to store interrupt data after Analyze calls.
   -- for debugging.
   type Registers_Array is array (Integer range <>) of Eight_Registers;

   package Sensor_History is  -- pre v2.1.8 was named Output_History
      -- (Unlike Command_Buffer this doesnt need mutex protection so is
      -- an ordinary ASM package.)

      procedure Add (
         Data      : in   Sensor_Register_Array;
         Time_Byte : in Unsigned_8 );
      --function Is_Empty return Boolean;

      procedure Dump (
            N : Positive := 20 );
      procedure Reset;

   end Sensor_History ;

   package body Sensor_History is

      subtype Buffer_Range is Integer range 1 .. Buffer_Size;
      Saved_Regs : Registers_Array (Buffer_Range);
      --Time_Bytes : array (1 .. Buffer_Size) of Unsigned_8;
      A_Tick : Tick_Array (Buffer_Range);           -- for debugging
      I      : Integer  := 1;
      Count  : Integer range 0..Buffer_Size := 0;

      procedure Add (
         Data      : in Sensor_Register_Array;
         Time_Byte : in Unsigned_8 ) is
      begin
         for J in Data'range loop
            Saved_Regs(I)(Integer(J)) := Int32defs.Unsigned(Data(J));
         end loop;
         Saved_Regs(I)(8) := Time_Byte;
         A_Tick(I) := Current_Tick;
         I := I mod Buffer_Size + 1;
         if Count < Buffer_Size then
            Count := Count + 1;
         end if;
      end Add;

--      function Is_Empty return Boolean is
--      begin
--         return Count = 0;
--      end Is_Empty;

      procedure Dump (
            N : Positive := 20 ) is
         -- print last N interrupts.
         -- Effectively A(J - (N-1) .. J) with cyclic indexing
         use Ada.Integer_Text_Io;
         Dump_Size : Integer := N;
         Idump     : Integer;

         function To_String(
            Val : Unsigned_8;
            LSB_Sensor : Raildefs.Sensor_Idx )
          return String8 is
         --
            use Int32defs;
            V : Sensor_Register := Unsigned_8_To_Sensor_Register(Val);
            Result : String8 := "........";
         begin
            for I in Sensor_Register'range loop  -- note display LSB on left
               if V(I) = On then
                  Result(Integer(I)+1) :=  -- 'o';
                   Character'val(48 + Integer(I + LSB_Sensor) mod 10);  -- v2.1.8
               end if;
            end loop;
            return Result;
         end To_String;

      begin
         Put_Line("previous interrupts (index  tick: s1..s64 subtick):");
         if Dump_Size > Count then  -- asking more than stored
            Dump_Size := Count;
         end if;
         if Dump_Size > Buffer_Size then  -- asking more than possible
            Dump_Size := Buffer_Size;    -- redundant iff Count <= Buffer_Size
         end if;
         -- currently I is one past the last interrupt stored
         Idump  := (I - 1 - Dump_Size + Buffer_Size) mod Buffer_Size + 1; -- fix 7-Feb-2013!
         for It in 1 .. Dump_Size loop
            Put(Idump, 2);
            Put(Integer(A_Tick(Idump)), 9);
            Put_Line(": ");
            for R in 0..7 loop -- Eight_Registers'range loop
               Put(To_String(Saved_Regs(Idump)(R), Sensor_Idx(R*8 + 1)));  Put(' ');
            end loop;
            Put(Integer(Saved_Regs(Idump)(8)), Width=>5); --, Base=>10);
            New_Line;
            Idump  := Idump mod Buffer_Size + 1;
         end loop;
         Put_Line("end of interrupt history");
      end Dump;

      procedure Reset is
      begin
         I := 1;
         Count := 0;
      end Reset;

   end Sensor_History;

   ---------------------------------------------------------------------------
   --               *** Subprogram bodies for public ops *** ---------------------------------------------------------------------------

   -- Install interrupt analyzer (vsn 2.0.4)
   procedure Install_Int_Handling (
         Analyzer : Raildefs.Proc4_Access ) is
   begin
      The_Analyzer := Analyzer;
      Interrupts_Enabled := True;
   end Install_Int_Handling;

   -- The following read procedure is called (via io_ports.read_io_port)
   -- to read simulated io ports.
   -- Registers that are meant to be output ONLY return 0.

   function Read_Reg (
         Addr : Unsigned_16 )
     return Unsigned_8 is
--      use Int32defs;
      --Sreg : Sensor_Register;
   begin
      if Crashed then
         raise Train_Crash;
      end if;
      case Addr is
         when Int32defs.Pa1_Addr=>
            return Int32defs.Unsigned(Sensor_Regs(0));
         when Int32defs.Pb1_Addr=>
            return Int32defs.Unsigned(Sensor_Regs(1));
         when Int32defs.Qa1_Addr=>
            return Int32defs.Unsigned(Sensor_Regs(2));
         when Int32defs.Qb1_Addr=>
            return Int32defs.Unsigned(Sensor_Regs(3));
         when Int32defs.Pa2_Addr=>
            return Int32defs.Unsigned(Sensor_Regs(4));
         when Int32defs.Pb2_Addr=>
            return Int32defs.Unsigned(Sensor_Regs(5));
         when Int32defs.Qa2_Addr=>
            return Int32defs.Unsigned(Sensor_Regs(6));
         when Int32defs.Qb2_Addr=>
            return Int32defs.Unsigned(Sensor_Regs(7));
         --
         when Dio192defs.Qb3_Addr=>
            return Dio192defs.Unsigned(Turnout_Status_Regs(0));
         when Dio192defs.Pa3_Addr=>
            return Dio192defs.Unsigned(Turnout_Status_Regs(1));
         when Dio192defs.Pc3_Addr=>
            return Dio192defs.Unsigned(Turnout_Status_Regs(2));
         when others=>
            return 0;
      end case;
   end Read_Reg;

   procedure Write_Reg (
         Address : in     Unsigned_16;
         Value   : in     Unsigned_8   ) is
   begin
      if Crashed then
         raise Train_Crash;
      end if;
      Command_Buffer.Add(Address, Value);
   end Write_Reg;

   -- Dump and Reset later.

   ---------------------------------------------------------------------------
   -- Register changes that will come via Command_Buffer:

   procedure Write_Block_Reg (
         I     : in     Block_Register_Index;
         Value : in     Unsigned_8 );  -- later
   procedure Write_Turnout_Reg (
         I     : in     Turnout_Register_Index;
         Value : in     Unsigned_8 );  -- later

--   procedure Write_Other_Reg (
--         I     : in     Integer;
--         Value : in     Unsigned_8 ) is
--   begin
--      Put_Line("simrail2 Error: write" & Value'Img & " to addr" & I'Img);
--   end;

   ---------------------------------------------------------------------------
   -- Packages hidden in this package body (alphabetic order after Relay_Pkg)

   ----------------------------------------------------------
   --  Relay_Pkg (ADT)
   ----------------------------------------------------------
   package Relay_Pkg is
      Relay_Changed : Boolean; -- optimisation for Simtrack
      type Relay_Type is private;

      Tflip : Integer := 2; -- ticks
      procedure Tell (
            R           : in out Relay_Type;
            Dir_Command : in     Polarity_Type;
            Changed     : in out Boolean        );
      procedure Tick (
            R       : in out Relay_Type;
            Changed : in out Boolean     );
      function Is_Open_Circuit (
            R : Relay_Type )
        return Boolean;
      function Polarity (
            R : Relay_Type )
        return Float;  -- -1.0, 0, +1.0
   private
      type Relay_State is
            (Norm,
             Rev,
             Go_Norm,
             Go_Rev);
      type Relay_Type is
         record
            State : Relay_State := Norm;
            Time  : Natural     := 0;
         end record;
   end Relay_Pkg;
   use Relay_Pkg;

   ----------------------------------------------------------
   --  Blockdriver_Pkg (ADT)
   ----------------------------------------------------------
   package Blockdriver_Pkg is
      type Blockdriver is private;
      subtype String2 is String(1..2);

      procedure Init (
            B   : in out Blockdriver;
            Num : in     Block_Id     );
      procedure Tell (
            B   : in out Blockdriver;
            Cab : in     Cab_Type     );
      procedure Change_Polarity (
            B       : in out Blockdriver;
            Dir     : in     Polarity_Type;
            Changed : in out Boolean        );
      procedure Tick (
            B       : in out Blockdriver;
            Changed : in out Boolean      );
      function Is_Open_Circuit (
            B : Blockdriver )
        return Boolean;
      function Get_Cab (
            B : Blockdriver )
        return Cab_Type; -- returns which DAC selected
      function Get_Signed_Cab (
            B : Blockdriver )
        return String2; -- returns eg "-2", "+0"  (v2.0.1)
      function Get_Signed_Voltage (
            B : Blockdriver )
        return Float; -- returns voltage (0 - 10 V)
   private
      type Blockdriver is
         record
            Cab   : Cab_Type   := 0; -- was temporarily 1 19/07/07-11/08/07
            Id    : Block_Id;
            Relay : Relay_Type;
         end record;
   end Blockdriver_Pkg;
   use Blockdriver_Pkg;

   ----------------------------------------------------------
   --  Segment_Feature_Pkg (ADT)
   --  geometry: sensor positions within segments,
   --  raw data in Simtrack2
   ----------------------------------------------------------
   -- Simrail2: reorganised this to use segments (was Block oriented):
   package Segment_Feature_Pkg is
      type Segment_Feature is private;
      -- add sensor called at setup time
      procedure Add_Sensor (
            Sb : in out Segment_Feature;
            Id : in     Sensor_Id;
            Mm : in     Float            );
      -- train calls procedure after moving a step, only needs to check
      -- max of 5 sensors per call.
      procedure Check_Sensor (
            Sb  : in     Segment_Feature;
            Pos : in     Train_Position   );

      -- helper ops:
      procedure Check_For_Crossing (
            Tid     : in     Train_Id;
            Fpos,
            Bpos    : in     Train_Position;
            Present :    out Boolean;
            Which   :    out Crossing_Idx    );
      -- procedure Check_For_Turnout (      -- removed v2.0.8
      --      Fpos,
      --      Bpos    : in     Train_Position;
      --      Present :    out Boolean;
      --      Which   :    out Turnout_Idx     );

      procedure Check_Central_Occupancy (  -- added 28/05/08
         Tid     : in     Train_Id;
         Fpos,
         Bpos    : in     Train_Position;
         Segno     : in     Seg_Index;
         Intersect : in     Float;
         Radius    : in     Float;
         Present   :    out Boolean      );

      function Length_Of (
            Sb : in     Segment_Feature )
        return Float;
      procedure Set_Length (
            Sb : in out Segment_Feature;
            L  : in     Float            );
   private
      type Sensor_Id_Array is array (1 .. Max_Segment_Sensors) of Sensor_Id;
      type Sensor_Mm_Array is array (1 .. Max_Segment_Sensors) of Float;
      type Segment_Feature is
         record
            Length        : Float                                  := 0.0;
            Mm_Into_Block : Sensor_Mm_Array;                               -- mm into the block
            Sensor_Ident  : Sensor_Id_Array;                               --sensor IDs
            Num_Sensors   : Integer range 0 .. Max_Segment_Sensors := 0;
         end record;
   end Segment_Feature_Pkg;
   use Segment_Feature_Pkg;



   ----------------------------------------------------------
   --  Turnout_Pkg (ADT)
   ----------------------------------------------------------
   package Turnout_Pkg is
      type Turnout is private;

      Tortoise_Time : constant Positive := 3500;  -- typical change time (ms)
      Tflip : Integer := Tortoise_Time*10 / 156; -- ticks for 3.5 sec (simrail2)
                                      -- assuming 15.6 ms is actual simrail2 period
                                      --
      -- Obsolete constants for capacitor-discharge solenoids
      --   Time_Constant : Integer := 50;
      --   -- ticks (0.5s, public for setup)
      --   Tau           : Float   := Float (Time_Constant);

      -- type Turnout_Pos is (Straight, Turned, Middle);  -- see Raildefs

      -- ensure Tflip is appropriate
      procedure Init_Timing(Tick_Interval : in Positive);

      -- Initialise turnout to moving toward Straight
      -- (anywhere from 0% to 100% of the way there)
      procedure Init (
            T  : in out Turnout;
            Id : in     Turnout_Id );

      procedure Tell (
            T       : in out Turnout;
            Command : in     Dio192defs.Turnout_Drive_Bit;
            Changed : in out Boolean );

      procedure Tick (
            T       : in out Turnout;
            Changed : in out Boolean  );

      function Position_Bit (
            T : Turnout )
        return Dio192defs.Turnout_Status_Bit;

      function Position_Of (
            T : Turnout )
        return Turnout_Pos;

      function Train_Covering (
            -- removed 30/07/07, reinstated 6/08/07
            T : Turnout )
        return Train_Idx;

      procedure Set_Cover (
            T     : in out Turnout;
            Train :        Train_Idx;
            Error :    out Boolean    );
   private
      --Threshold   : constant Float := 0.9;
      Uncertainty : constant Float := 0.11;
      -- (originally) 1% failure at full charge, 25% at 95% charge,
      -- random**2 * uncertainty was subtracted from actual charge)
      -- (v2.2.0:) Uncertainly is max reduction in move time, uniform
      -- random distribution, eg 3.115 .. 3.5 sec  ( 3.115=3.5*(1-0.11) )
      type Turnout_State is
            (Is_Straight,
             Is_Stuck,
             Is_Turn,
             Go_Turn,
             Go_Straight);

      type Turnout is
         record
            Id              : Turnout_Id;
            Pos_Bit         : Dio192defs.Turnout_Status_Bit := Dio192defs.In_Position;
            State           : Turnout_State                := Is_Straight;
            Current_Command : Dio192defs.Turnout_Drive_Bit  := Dio192defs.Pull_St;
            -- Charge       : Float                        := 0.0;  -- obsolete
            Time            : Integer                      := 0;
            Cover           : Train_Idx                    := No_Train;
            -- id of train over it, if any
         end record;
   end Turnout_Pkg;
   use Turnout_Pkg;

   --------------------------------------------------------------------------------
   -- These are models of the physical objects of the train system
   --
   task Worker is
      -- periodic task to do everything for each time step then update screen
      entry Start (
            Tick_Ms  : Positive := 10;
            Sleep_Ms : Positive := 10;
            N_Trains : Positive := Max_Trains );
   end Worker;

   type Train_Type is
      record
         Id              : Train_Id;       -- Train Number
         Front_Pos       : Train_Position; -- current train front reflector position
         Front_Wheel_Pos : Train_Position; -- a little backward
         Middle_Pos      : Train_Position; -- ignore if no carriages
         Back_Wheel_Pos  : Train_Position; -- a little forward
         Back_Pos        : Train_Position; -- back of loco reflector
         --
         Last_Step_Time : Time := Clock;
         -- used to calculate time dependant step
         Front_Speed   : Float   := 0.0;  -- signed
         Going_Forward : Boolean := True;
         --Forward_Direction : Boolean; -- maintained in Step but otherwise unused
         Has_Carriages : Boolean;
         Crashed       : Boolean;
         --Over_Turnout      : Boolean;
         Last_Turnout_Entered : Turnout_Idx  := No_Turnout;
         Over_Crossing        : Crossing_Idx := No_Crossing; -- no train is long enough
      -- occupy two crossings.
         Orig_Length : Float;  --v2.1.6
      end record;

   Dacs : array (Dac_Id) of Unsigned_8 := (others => 0);
   -- one for each Train plus 2 dummies
   Dac_Changed : array (Dac_Id) of Boolean := (others => True);

   Turnouts : array (Turnout_Id) of Turnout;

   Blockdrivers  : array (Block_Id) of Blockdriver;                     -- one for each block
   Block_Changed : array (Block_Id) of Boolean     := (others => True);

   Trains     : array (Train_Id) of Train_Type;
   Num_Trains : Train_ID := Max_Trains; -- can be 1 for testing (needed here for Dump)

   Crossing_Occupant : array (Crossing_Id) of Train_Idx := (others => No_Train);

   Seg_Occupant : array (Seg_Index range 1 .. Simtrack2.Num_Segments - Num_Turnouts) of Train_Idx := (others => No_Train);

   --Turnout_Occupant : array(Turnout_Idx) of Train_Idx  -- duplicates function Covered
   -- := (others => No_Train);  -- of Turnout_Pkg
   -- todo: maybe change to linked lists:
   Segment_Features        : array (Seg_Index range 1 .. Simtrack2.Num_Segments - Num_Turnouts) of Segment_Feature;          -- essentially constant
   Segment_Features_Inited : Boolean                                                                               := False;

   -- Segment_Feature_Pkg.Check_Sensor will change Temp_Sensor_States when checking if train's
   -- reflector is over sensor.  Later switch(ID,boolean) calls will modify
   -- Sensor_States, the Int32 registers and update the Simtrack2 screen display
   --
   Sensor_States : Sensor_State_Array;
   -- the real state, imaged in Dio192 registers and screen
   Temp_Sensor_States : Sensor_State_Array; -- working version

   Rng : Ada.Numerics.Float_Random.Generator;

   ---------------------------------------------------------------------
   --   Misc procedures:
   ---------------------------------------------------------------------
   procedure Init_Sensors; -- called during setup: sets all to SENSOR_OFF state

   -- change Sensor_States, tell Simtrack.Display, update Sensor_Regs
   procedure Switch (
         Id    : Sensor_Id;
         State : Sensor_Bit );

   function Get_Voltage (
         Val : Unsigned_8 )
     return Float;

   procedure Setup (
         Num_Trains    : Positive;
         Init_Data     : Init_Array;
         Tick_Interval : Positive := 15          );

   procedure Update_Cab_Dac_Display (
         Force : in     Boolean := False );

   ----------------------------------------------------------------------
   -- Bodies of subprograms relating to I/O cards:
   ----------------------------------------------------------------------
   procedure Write_Turnout_Reg (
         I : in     Turnout_Register_Index;
         -- assume 0..2
         Value : in     Unsigned_8 ) is
      use      Dio192defs,
      Turnout_Pkg;
      Reg         : Dio192defs.Turnout_Drive_Register := Unsigned_8_To_Turnout_Drive_Register (Value);
      Id          : Turnout_Idx;
      Changed     : Boolean := False;
   begin
      --   Put_Line("Write_Turnout_Reg...");
      if Turnout_Drive_Regs(I) /= Reg then
         -- now possibly call a method to drive a turnout
         for T in Turnout_Idx range 0..7 loop
            if Reg(T) /= Turnout_Drive_Regs(I)(T) then
               Id := 8*Turnout_Idx(I) + T + 1;
               if Id <= Num_Turnouts then
                  Tell(Turnouts(Id), Reg(T), Changed);
                  if Changed then
                     Simtrack2.Display.Change_Turnout_Status(Id, Middle);
                     Turnout_Status_Regs(I)(T) := Position_Bit(Turnouts(Id));
                  end if;
               end if;
               -- no else: we ignore turnout 20+
            end if;
         end loop;
         Turnout_Drive_Regs(I) := Reg;   -- remember change
      end if;
   end Write_Turnout_Reg;

   procedure Write_Block_Reg (
         I     : in     Block_Register_Index;   -- 0..11
         Value : in     Unsigned_8 ) is
      use      Dio192defs,
      Blockdriver_Pkg,
      Relay_Pkg;
      Reg : Block_Register := Unsigned_8_To_Block_Register (Value);
      -- declared in Dio192defs:
      --      Blk0_Cab : Cab_Type;  -- for blocks 1,3,5,...
      --      Blk0_Pol : Polarity_Type;
      --      Blk1_Cab : Cab_Type;  -- for blocks 2,4,6,...
      --      Blk1_Pol : Polarity_Type;
      Id : Block_Idx;
   begin

      if Block_Regs(I) /= Reg then
         -- Now call setCab/Pol method of appropriate block 1 to 24
         Id := 2*Block_Idx(I) + 1;
         for Nibble in reverse Block_Idx range 0..1 loop
            if Reg(Nibble).Blk_Cab /= Block_Regs(I)(Nibble).Blk_Cab then
               Tell(Blockdrivers(Id), Reg(Nibble).Blk_Cab);
               Block_Changed(Id) := True;
            end if;
            if Reg(Nibble).Blk_Pol /= Block_Regs(I)(Nibble).Blk_Pol then
               Change_Polarity(Blockdrivers(Id), Reg(Nibble).Blk_Pol, Block_Changed(Id));
            end if;
            if Id < Num_Blocks then Id := Id + 1; end if;
         end loop;
         Block_Regs(I) := Reg;
      end if;
      Cio192_Changed := True;  -- since this is for optimisation, it doesnt
      -- matter if in fact nothing changed here.
   end Write_Block_Reg;


   -- protocol for DDA06 is write lower 8-bits then upper 4 bits, ie 12-bits
   -- but 2048 is zero volts.  Retaining 8 bits the assumed format is:
   --   hi: 0001xxx    lo: xxxxx000
   --
   procedure Write_Dda06 (
         Addr  : in     Unsigned_16;
         Value : in     Unsigned_8   ) is
      use Dda06defs;
   begin
      for Dac in Dac_Id loop
         if Addr = Dda06defs.Dahi_Addr(Dac) then
            Dac_High_Regs(Dac) := Value;
            Dacs(Dac) := Unsigned_8((Value-8)*32 + Dac_Low_Regs(Dac)/8);  -- 0..255
            Dac_Changed(Dac) := True;
         elsif Addr = Dda06defs.Dalo_Addr(Dac) then  -- (v2.0.8 fixed bug)
            Dac_Low_Regs(Dac) := Value;
         end if;
      end loop;
   end Write_Dda06;



   -- Das08 PA register controls horn and bell on 4 Dalley sound generators
   -- Here we display the union of all horns & bells in Simtrack2's
   -- "hazard" field, and details in the cab table as 2 chars, HB for both.
   -- The register is treated as an array of 8 sensor bits (a kludge!).

   procedure Write_Sound_Register( Value : in     Unsigned_8 ) is
      use Int32defs;
      New_Reg : Sensor_Register := Unsigned_8_To_Sensor_Register(Value);
      Whoo_Ding : String(1..9) := (others=>' ');
      Hb4 : String(1..8) := (others=>' ');
      Non_Blank : Boolean := False;
   begin
      if New_Reg = Sound_Reg then return; end if;
      for I in Sound_Reg'range loop
--         if New_Reg(I) /= Sound_Reg(I) then
         if New_Reg(I) = Sound_On then
            if I mod 2 = 0 then
               Whoo_Ding(1..4) := "Whoo";  -- a horn bit
               Hb4(Integer(I)+1) := 'H';
            else
               Whoo_Ding(6..9) := "Ding";  -- a bell bit
               Hb4(Integer(I)+1) := 'B';
            end if;
            Non_Blank := True;
         end if;
--         end if;
      end loop;
      Simtrack2.Display.Show_Horns_Bells(Hb4);
      Simtrack2.Display.Hazard(Non_Blank, Whoo_Ding);
      Sound_Reg := New_Reg;
   end Write_Sound_Register;

   --------------------------------------------------------------------------
   -- Types and vars for simulating Halls2-INT32 interrupt handling
   --
   -- An array of 8 bits packed into a byte.
   -- Used to store individual bits in a byte as Boolean, cf Sensor_Register.
   type Bit_Array is array (Sensor_Idx range 0..7) of Boolean;
   pragma pack (Bit_Array);
   for Bit_Array'Size use 8;
   All_False : constant Bit_Array := (others => False);

   -- Convert an array of bits to a byte
   function To_Unsigned is new Ada.Unchecked_Conversion
                                 (Source => Bit_Array,
                                  Target => Unsigned_8);

   type Halls2_Buffer_Index is mod 16;
   Halls2_Index : array (0..1) of Halls2_Buffer_Index := (0,0);
   -- These provide upper nibble of Halls2 debugging bytes, ie the
   -- last elements of Four_Registers.  They are fake versions of the
   -- current 'take' index within the two buffers of Halls2.

   -------------------------------------------------------------------
   --   Possibly raise a simulated interrupt
   -------------------------------------------------------------------

   procedure Check_Changes is
      --  globals: Sensors_Changed : Boolean set False before this call
      use Int32defs;
      Regs : Four_Registers;  -- was Eight_ in 2007
      Time_Byte : Unsigned_8;
      -- lower nibbles for Halls2 debugging bytes:
      Int32_Chipbits : array (0..1) of Bit_Array := (others=>All_False);
      -- control whether a fake interrupt will be sent, 32 bits / card
      Offset4 : Sensor_Idx;
   begin
      for I in Sensor_Regs'range loop  -- 0..7
         if Sensor_Regs(I) /= Old_Sensor_Regs(I) then
            Old_Sensor_Regs(I) := Sensor_Regs(I);
            if I < 4 then
               -- something changed in chip I of first Int32 card
               Int32_Chipbits(0)(I) := True;
            else
               -- something changed in chip I-4 of second Int32 card
               Int32_Chipbits(1)(I - 4) := True;
            end if;
            Sensors_Changed := True;
         end if;
      end loop;
      if not Sensors_Changed then
         return;
      end if;
      if Interrupts_Enabled then
         -- assert: The_Analyzer /= null
         -- should now raise interrupt and send relevant 4 registers
         -- as if from one or both of the Halls2 handler tasks:
         Time_Byte := Unsigned_8(Current_Tick mod 200);  -- v1.7 was 100

         for Card in Int32_Chipbits'range loop  -- 0..1
            Offset4 := Sensor_Idx(Card*4);  -- 0 or 4
            if Int32_Chipbits(Card) /= All_False then
               for I in Sensor_Register_Index range 0..3 loop
                  Regs(Integer(I)) := Unsigned(Sensor_Regs(I + Offset4));
               end loop;
               Regs(4) := To_Unsigned(Int32_Chipbits(Card)) + Unsigned_8(Halls2_Index(Card))*16;
               Halls2_Index(Card) := Halls2_Index(Card) + 1;
               The_Analyzer.All(8*Offset4, Regs);  -- first param 0 or 32
            end if;
         end loop;
         -- save all 64-bits (history is not oriented as 2 x 32 bits)
         Sensor_History.Add(Sensor_Regs, Time_Byte);
      end if;
   end Check_Changes;

   ---------------------------------------------------------------------
   --      Bodies of local packages (alphabetic order):
   ---------------------------------------------------------------------

   package body Blockdriver_Pkg is separate;

   ---------------------------------------------------------------------

   package body Relay_Pkg is separate;

   ---------------------------------------------------------------------

   package body Segment_Feature_Pkg is separate;

   ---------------------------------------------------------------------

   package body Turnout_Pkg is separate;

   ---------------------------------------------------------------------
   --   Bodies of misc procedures:
   ---------------------------------------------------------------------

   -- Get_Voltage returns Analogue equivalent of digital voltage
   --  This version:  0 -> 0V,  255 -> 8.6V see SimConst.
   function Get_Voltage (
         Val : Unsigned_8 )
     return Float is
      --answer : Float;
   begin
      return (Float(Val) / 255.0) * Max_Cab_Voltage;
   end Get_Voltage;

   ---------------------------------------------------------------------
   --   Procedures called by Worker:
   ---------------------------------------------------------------------

   procedure Tick_All_Turnouts is
      --
      -- Tell each turnout time has advanced by one step (10ms).
      -- If a turnout finishes its movement then set
      -- relevant bit of turnout register accordingly and
      -- tell track display.
      --
      --use Dio192defs;
      Changed  : Boolean     := False;
      Position : Turnout_Pos;
      Bit : Dio192defs.Turnout_Status_Bit;
--      -- vars: for debugging:
--      Bitno : Turnout_Idx;
--      Regno : Natural;
--      Val : Unsigned_8;
   begin
      for Tn in Turnout_Id loop
         Turnout_Pkg.Tick(Turnouts(Tn), Changed);
         if Changed then
            Position  := Position_Of(Turnouts(Tn));
            Bit := Position_Bit(Turnouts(Tn));

--            Regno := (Integer(Tn - 1)/8);
--            Bitno := ((Tn - 1) mod 8);
--            Put_Line("debug Tick_All_Turnouts:" & Tn'img & Regno'img & Bitno'img);
--            Val := Dio192defs.Unsigned(Turnout_Status_Regs(Integer(Tn - 1)/8));
--            Ada.Integer_Text_Io.Put(Integer(Val), base=>2);
--            New_Line;

            -- Note: gnat 3.15 does NOT generate wrong code for the following line when id is 8n+1
            Turnout_Status_Regs(Turnout_Idx(Tn - 1)/8)((Tn - 1) mod 8) := Bit;

--            Val := Dio192defs.Unsigned(Turnout_Status_Regs(Integer(Tn - 1)/8));
--            Ada.Integer_Text_Io.Put(Integer(Val), base=>2);
--            New_Line;

            Simtrack2.Display.Change_Turnout_Status(Tn, Position);
            Cio192_Changed := True;
         end if;
      end loop;
   end Tick_All_Turnouts;

   procedure Tick_All_Blockdrivers is
   begin
      for B in Block_Id loop
         Blockdriver_Pkg.Tick(Blockdrivers(B), Block_Changed(B));
      end loop;
   end Tick_All_Blockdrivers;

   procedure Process_All_Commands is
      --
      -- Empty the command buffer, viz all writes to simulated registers
      --
      Address : Unsigned_16;
      Value   : Unsigned_8;
   begin
      --      Put_Line("Process_All_Commands...");
      while not Command_Buffer.Is_Empty loop
         Command_Buffer.Remove(Address, Value);
         -- Put_Line("processing" & Address'Img & Value'Img);

         -- the following partially takes account of tristate buffering:
         -- register clearing on reset is missing

         -- CIO192 bytes for blocks:
         -- 1..12: Pa1_Addr, Pb1_Addr, Pc1_Addr, Qa1_Addr, Qb1_Addr, Qc1_Addr,
         -- 13..24: Pa2_Addr, Pb2_Addr, Pc2_Addr, Qa2_Addr, Qb2_Addr, Qc2_Addr
         case Address is
            when Dio192defs.Pa1_Addr =>
               Write_Block_Reg(0, Value);  -- blocks 1,2 (4 bits each)
            when Dio192defs.Pb1_Addr =>
               Write_Block_Reg(1, Value);  -- blocks 3,4
            when Dio192defs.Pc1_Addr =>
               Write_Block_Reg(2, Value);
            when Dio192defs.Qa1_Addr =>
               Write_Block_Reg(3, Value);
            when Dio192defs.Qb1_Addr =>
               Write_Block_Reg(4, Value);
            when Dio192defs.Qc1_Addr =>
               Write_Block_Reg(5, Value);
            when Dio192defs.Pa2_Addr =>
               Write_Block_Reg(6, Value);
            when Dio192defs.Pb2_Addr =>
               Write_Block_Reg(7, Value);
            when Dio192defs.Pc2_Addr =>
               Write_Block_Reg(8, Value);
            when Dio192defs.Qa2_Addr =>
               Write_Block_Reg(9, Value);
            when Dio192defs.Qb2_Addr =>
               Write_Block_Reg(10, Value);
            when Dio192defs.Qc2_Addr =>
               Write_Block_Reg(11, Value);  -- blocks 23,24

            when Dio192defs.Pctl1_Addr =>
               if Cio192_Pctl1 = 128 and Value = 0 then  -- v1.7
                  Put_Line("cio192_P1 enabled for output");
               end if;
               Cio192_Pctl1 := Value;
            when Dio192defs.Qctl1_Addr =>
               if Cio192_Qctl1 = 128 and Value = 0 then
                  Put_Line("cio192_Q1 enabled for output");
               end if;

            when Dio192defs.Pctl2_Addr =>
               if Cio192_Pctl2 = 128 and Value = 0 then
                  Put_Line("cio192_P2 enabled for output");
               end if;
               Cio192_Pctl2 := Value;
            when Dio192defs.Qctl2_Addr =>
               if Cio192_Qctl2 = 128 and Value = 0 then
                  Put_Line("cio192_Q2 enabled for output");
               end if;

            when Dio192defs.Pctl3_Addr =>
               if Cio192_Pctl3 = Dio192defs.Pctl3_Init1 and Value = Dio192defs.Pctl3_Init2 then
                  Put_Line("cio192_P3 enabled for I/O");
               end if;
               Cio192_Pctl3 := Value;
            when Dio192defs.Qctl3_Addr =>
               if Cio192_Qctl3 = Dio192defs.Qctl3_Init1 and Value = Dio192defs.Qctl3_Init2 then
                  Put_Line("cio192_Q3 enabled for I/O");
               end if;
            -- Dio192 bytes for Turnout_Drive:
            -- turnouts 1..8:Qa3_Addr, 9..16:Qc3_Addr, 17..24:Pb3_Addr
            --
            when Dio192defs.Qa3_Addr =>
               Write_Turnout_Reg(0, Value);  -- turnouts 1..8
            when Dio192defs.Qc3_Addr =>
               Write_Turnout_Reg(1, Value);  -- turnouts 9..16
            when Dio192defs.Pb3_Addr =>
               Write_Turnout_Reg(2, Value);  -- turnouts 17..24 but only installed up to 19

            -- and state:
            -- turnouts 1..8:Qb3_Addr, 9..16:Pa3_Addr, 7..24:Pc3_Addr

            when Int32defs.Pctl1_Addr | Int32defs.Qctl1_Addr |
                  Int32defs.Pctl2_Addr | Int32defs.Qctl2_Addr =>
               null;

            -- todo: Int32 equivalent for...
            --         when Simdio48.Enable_Chg_State_Int =>
            --            Interrupts_Enabled  := Value = 16#30#;
            --            Put_Line("Interrupts_Enabled " & Interrupts_Enabled'Img);
            --         when Simdio48.Clear_Chg_State_Int =>
            --            Changed := False;

            -- DACs on DDA06 card:
            when Dda06defs.Base_Address..Dda06defs.Base_Address + 7 =>
               Write_Dda06(Address, Value);

            -- PA on DAS08 card:
            when Sound_Address =>
               Write_Sound_Register(Value);

            when others =>
               Put("***error: unexpected write to address ");
               Ada.Integer_Text_Io.Put(Integer(Address), Base=>16);
               Ada.Text_Io.New_Line;
         end case;
      end loop;
   end Process_All_Commands;


   procedure Update_Cab_Dac_Display (
         Force : in     Boolean := False ) is
   begin
      for N in Block_Id loop
         if Force or Block_Changed(N) then
            --Ada.Text_Io.Put_Line("cab" & N'Img & ":" & Get_Signed_Cab(Blockdrivers(N)));
            Simtrack2.Display.Show_Cab(N, Get_Signed_Cab(Blockdrivers(N)));
            Block_Changed(N) := False;
         end if;
      end loop;
      for D in Dac_Id loop
         if Force or Dac_Changed(D) then
            Simtrack2.Display.Show_Dac(Train_Id(D), Get_Voltage(Dacs(D)));
            Dac_Changed(D) := False;
         end if;
      end loop;
   end Update_Cab_Dac_Display;


   -- change Sensor_States, tell Simtrack.Display, update Sensor_Regs
   procedure Switch (
         Id    : Sensor_Id;
         State : Sensor_Bit ) is
   --
      Bitno  : Natural;
      Bitval : Sensor_Bit;
      Index  : Sensor_Register_Index;
   begin
      if Sensor_States(Id) /= State then
         -- only write to register if value changes
         Sensor_States(Id) := State;
         -- sensors state changed so write to register
         -- (which may cause dio_interrupt)
         --Timestamp; Put_Line(" sim.Sensor_handler: sensor " & Id'Img & " = " & State'Img);
         Simtrack2.Display.Draw_Sensor(Id, State);

         -- Note: gnat 3.15 generates wrong code for the following line when id is 8n+1
         --    Sensor_Regs(Integer((Id - 1)/8))((Id - 1) mod 8) := State;
         -- replacement:
         Bitno := (Integer(Id)-1) mod 8;
         if State = On then
            Bitval := On;
         else
            Bitval := Off;
         end if;
         Index := Sensor_Register_Index((Id-1)/8);
         Sensor_Regs(Index)(Sensor_Idx(Bitno)) := Bitval;
      end if;
   exception
      when others=>
         Put_Line("exception in Switch, id=" & Id'Img & " bitno="
            & Bitno'Img & " index=" & Index'Img);
   end Switch;


   ---------------------------------------------------------------------
   --      Train procedures
   ---------------------------------------------------------------------

   --   type Train_Type is record
   --      Id : Train_Id; -- Trains unique ID Number
   --      Front_Pos : Train_Position; -- current train front position
   --      Front_Wheel_Pos : Train_Position; -- a little back
   --      Middle_Pos : Train_Position; -- ignore if no carriages
   --      Back_Wheel_Pos : Train_Position; -- a little forward
   --      Back_Pos : Train_Position; -- current train back position
   --      Last_Step_Time : Time := Clock; -- used to calculate time dependant step
   --      Front_Speed : Float := 0.0; -- signed
--         Going_Forward : Boolean := True;
--         Has_Carriages : Boolean;
--         Crashed       : Boolean;
--         Last_Turnout_Entered : Turnout_Idx  := No_Turnout;
--         Over_Crossing        : Crossing_Idx := No_Crossing; -- no train is long enough
--         Orig_Length : Float;  --v2.1.6
   --   end record;

   procedure Init_Train (
         T                : in out Train_Type;
         Train_Num        : in     Train_Id;
         Start_Back_Pos   : in     Train_Position;
         Start_Middle_Pos : in     Train_Position;
         Start_Front_Pos  : in     Train_Position;
         Carriages        : in     Boolean;
         Length           : in     Float            ) is    --v2.1.6
      -- initialises the train object

      procedure Set_Occupied (
            Segno : in     Seg_Index ) is
         Tnid  : Turnout_Idx;
         Dummy : Boolean;
      begin
         Seg_Occupant(Segno) := Train_Num;
         Tnid := Simtrack2.Segments(Segno).Tnid;
         if Tnid /= No_Turnout then
            --         Turnout_Occupant(Tnid) := Train_Num;
            Set_Cover(Turnouts(Tnid), Train_Num, Dummy);
         end if;
      end Set_Occupied;

   begin
      T.Id := Train_Num;
      T.Front_Pos := Start_Front_Pos;
      T.Front_Wheel_Pos := Start_Front_Pos;  -- assume same segment
      if Start_Front_Pos.To_Front = Normal_Pol then
         T.Front_Wheel_Pos.Mm := T.Front_Wheel_Pos.Mm - Simtrack2.Wheel_To_Reflector;
      else
         T.Front_Wheel_Pos.Mm := T.Front_Wheel_Pos.Mm + Simtrack2.Wheel_To_Reflector;
      end if;
      T.Has_Carriages := Carriages;
      T.Middle_Pos := Start_Middle_Pos;
      T.Back_Pos := Start_Back_Pos;
      T.Back_Wheel_Pos := Start_Back_Pos;  -- assume same segment
      if Start_Back_Pos.To_Front = Normal_Pol then
         T.Back_Wheel_Pos.Mm := T.Back_Wheel_Pos.Mm + Simtrack2.Wheel_To_Reflector;
      else
         T.Back_Wheel_Pos.Mm := T.Back_Wheel_Pos.Mm - Simtrack2.Wheel_To_Reflector;
      end if;
      T.Front_Speed := 0.0;
      T.Going_Forward := True;  -- new attribute (simrail2)

      Set_Occupied(T.Front_Pos.Segno);
      Set_Occupied(T.Middle_Pos.Segno);
      Set_Occupied(T.Back_Pos.Segno);
      -- NB we assume the above are all the relevant segs.
      -- todo: relax this

      T.Last_Step_Time := Clock;
      T.Crashed := False;
      --T.Over_Turnout := False;
      T.Over_Crossing := No_Crossing;
      T.Orig_Length := Length;  --v2.1.6
   end Init_Train;

   procedure Print_Train_Heading is
      -- prints headings for proc Print
   begin
      Put_Line("ID Front(B,S,mm,-mm)    Midl(B,S,mm,-mm)     Back(B,S,mm,-mm)     tnouts");
   end Print_Train_Heading;

   Pol : constant array (Polarity_Type) of Character := (
      Normal_Pol => ' ',
      Reverse_Pol => '_');
   -- Seg : Integer;

   procedure Put_Pos (
         Pos : in     Train_Position ) is
      use Ada.Text_Io;
      use Ada.Integer_Text_Io;
      use Ada.Float_Text_Io;
   begin
      Put(Integer(Simtrack2.Segments(Pos.Segno).Blok), 3);
      -- because this is
      if Pos.Segno <= Simtrack2.Num_Arcs then
         Put(" A");
         Put(Integer(Pos.Segno), 2);
     elsif Pos.Segno <= Simtrack2.Num_Arcs + Simtrack2.Num_Lines then
         Put(" L");
         Put(Integer(Pos.Segno - Simtrack2.Num_Arcs), 2);
      else
         Put(" T"); -- shouldnt see these
         Put(Integer(Pos.Segno - Simtrack2.Num_Arcs - Simtrack2.Num_Lines), 2);
      end if;
      Put(Pos.Mm, 4, 1, 0);
      Put(Length_Of(Segment_Features(Pos.Segno))-Pos.Mm, 4, 1, 0);
      Put(Pol(Pos.To_Front));
      Put('|');
   end Put_Pos;

   procedure Print_Train (
         T : in     Train_Type ) is
      -- prints the train object for debugging
      use Ada.Text_Io;
      use Ada.Integer_Text_Io;
      use Ada.Float_Text_Io;
      Tf : constant array (Boolean) of String(1..3) := (" F "," T ");
   begin
      Put(T.Id'Img);
      Put(" |");
      Put_Pos(T.Front_Pos);
      if T.Has_Carriages then
         Put_Pos(T.Middle_Pos);
      else
         Put("                    |");
      end if;
      Put_Pos(T.Back_Pos);
      New_Line;

      Put("whl|");
      Put_Pos(T.Front_Wheel_Pos);
      Put("fwd" & Tf(T.Going_Forward) & "crs" & T.Over_Crossing'Img & " crash" & Tf(T.Crashed) & "|");
      Put_Pos(T.Back_Wheel_Pos);

      for Tn in Turnout_Id loop
         --      if Turnout_Occupant(Tn) = T.Id then
         if Train_Covering(Turnouts(Tn)) = T.Id then
            Put(Tn'Img & " ");
         end if;
      end loop;
      New_Line;
      --   Put("Segs: ");
      --   for I in Seg_Occupant'range loop
      --      if Seg_Occupant(I) = T.Id then
      --         if I <= Simtrack2.L then
      --            Put('A');  Put(Integer(I),2);
      --         else
      --            Put('L');  Put(Integer(I)-Integer(Simtrack2.L),2);
      --         end if;
      --         Put(", ");
      --      end if;
      --   end loop;
      --   New_Line;
   end Print_Train;


   procedure Step (
         T            : in out Train_Type;
         Elapsed_Time : in     Duration    ) is
   separate;

   -----------------------------------------------------------------
   --                           Task Worker
   -----------------------------------------------------------------
   --
   --  This task periodically (usually every 10ms) updates train
   --  positions on the track.
   --
   task body Worker is

      Tick_Interval,
      Sleep_Time    :          Duration;
      Ms            : constant Duration       := 0.001;
      type Train_Positions is array (Train_Id) of Train_Position;
      Old_Frontpos,
      Old_Midpos,
      Old_Backpos : Train_Positions;
      -- Forward_Direction: Boolean;  -- unused
      -- Now,  --v1.7 moved global
      Back_Then  : Time;
      Dur        : Duration;
      --Count,
      Ticks      : Integer;
      First      : Boolean;
      Num_Trains : Train_ID := Max_Trains;
      --Temp_Sensor_States : Sensor_State_Array; -- working version
      Min_Tick_Int    :          Duration;
      N_Tick_Checks   : constant          := 15; -- loops for checking Tick_Interval
      Ticks_Threshold :          Integer  := 5;  -- multiplier for reporting abnormal delays
      Trace           :          Integer  := 0;
   begin
      Outer_Loop:
         loop
         accept Start (
               Tick_Ms  : Positive := 10;
               Sleep_Ms : Positive := 10;
               N_Trains : Positive := Max_Trains ) do
            Tick_Interval := Tick_Ms*Ms;
            Sleep_Time := Sleep_Ms*Ms;
            Num_Trains := Train_ID(N_Trains);
         end Start;
         Crashed := False;
         First   := True;
         Min_Tick_Int := 2*Tick_Interval;

         begin  -- exception handler block
            --Ada.Text_Io.Put_Line("simulation.Worker task start...");
            Old_Frontpos(1) := Trains(1).Front_Pos;
            Old_Backpos(1)  := Trains(1).Back_Pos;
            Old_Midpos(1)  := Trains(1).Middle_Pos;
            if Num_Trains > 1 then
               Old_Frontpos(2) := Trains(2).Front_Pos;
               Old_Backpos(2)  := Trains(2).Back_Pos;
               Old_Midpos(2)  := Trains(2).Middle_Pos;
            end if;
            if Num_Trains > 2 then
               Old_Frontpos(3) := Trains(3).Front_Pos;
               Old_Backpos(3)  := Trains(3).Back_Pos;
               Old_Midpos(3)  := Trains(3).Middle_Pos;
            end if;
            Ada.Text_Io.Put_Line("Sleep_Time=" & Sleep_Time'Img &
               " Tick_Interval(for simulation)=" & Tick_Interval'Img);
            Back_Then := Clock;

            Main_Loop:
               loop
               delay Sleep_Time; -- 55ms or 15ms or 10ms depending on BIOS
               exit when Crashed;
               Trace := 1;
               Now := Clock;
               Dur := To_Duration(Now - Back_Then);
               -- 50 or 60 or 10 or 11 ms (or more if cpu busy)
               Back_Then := Now;
               if Current_Tick < N_Tick_Checks then
                  Timestamp;
                  Ada.Text_Io.Put_Line(" Simulation step...actual" &
                     Integer(1000*Dur)'Img & "ms");
                  if Dur < Min_Tick_Int then
                     Min_Tick_Int := Dur;
                  end if;
               elsif Current_Tick = N_Tick_Checks then
                  Tick_Interval := Integer(1000*Min_Tick_Int)*Ms;
                  Ada.Text_Io.Put_Line("Revised Tick_Interval:" & Tick_Interval'Img);
               end if;

               Ticks := Integer(Float(Dur)/Float(Tick_Interval));
               -- rounded, perhaps to zero
               if First or else Ticks = 0 then  --2005-06-21 was "and then"
                  Ticks := 1;
               end if;
               if Ticks > Ticks_Threshold then  --2005-06-21 added report
                  if To_Duration(Now - Start_Time) > 60.0 then
                     -- some clients use too much cpu so
                     -- raise reporting threshold after 1 min
                     Ticks_Threshold := Ticks_Threshold*3;
                  end if;
   --               Timestamp;
   --               Ada.Text_Io.Put_Line(Current_Tick'Img & ": abnormally long sim sleep:"
   --                   & Ticks'Img & " ticks");
               end if;

               -- loop deleted 2005-06-21 because this makes the simulator go much
               -- faster than student's tcs code when the cpu has been taken by some
               -- Windows housekeeping task, eg virusscan update checking.  Originally
               -- the problem was slow screen updates, but with 2Ghz CPUs it is no
               -- longer a problem.
               --deleted:  for Tick in 1..Ticks loop  -- deleted 2005-06-21

               Current_Tick := Current_Tick + 1;
               Trace := 2;

               Tick_All_Turnouts;

               Temp_Sensor_States := (others => Off);

               for Tn in 1..Num_Trains loop
                  Step(Trains(Train_Id(Tn)), Tick_Interval);
               end loop;

               Trace := 3;

               -- if an installed sensor has changed update the display and change a
               -- bit in a simulated INT32 register
               for S in Sensor_Id loop
                  if Simtrack2.Sensor_Segment_Numbers(S) /= Simtrack2.Nc then
                     Switch(S, Temp_Sensor_States(S));
                  end if;
               end loop;

               Trace := 4;
               Relay_Pkg.Relay_Changed := False;  -- may change in next line...
               Tick_All_Blockdrivers; -- note: after trains have moved

               Trace := 5;
               Cio192_Changed := False;  -- may change in next line...
               Process_All_Commands;

               Trace := 6;
               Check_Changes;  -- may 'raise' a simulated interrupt
                  -- and set          Sensors_Changed

               if Crashed then
                  Simtrack2.Display.Hazard;
                  exit Main_Loop;
               end if;
               --deleted: end loop;  -- end 2005-06-21 change

               Trace := 7;
               -- now update the display:
               for T in Train_ID range 1..Num_Trains loop
                  if First or Sensors_Changed or
                     abs (Trains(T).Front_Pos.Mm - Old_Frontpos(T).Mm) >= 5.0 then
                     Trace := 8;
                     -- inefficient full draw, twice!
                     Simtrack2.Display.Draw_Train(Trains(T).Has_Carriages, Old_Backpos(T),
                        Old_Midpos(T), Old_Frontpos(T), Simtrack2.Display.Bg_Color);
                     Simtrack2.Display.Draw_Train(Trains(T).Has_Carriages, Trains(T).Back_Pos,
                        Trains(T).Middle_Pos, Trains(T).Front_Pos, Simtrack2.Display.Train_Color(T));
                     Old_Frontpos(T) := Trains(T).Front_Pos;
                     Old_Backpos(T)  := Trains(T).Back_Pos;
                     Old_Midpos(T) := Trains(T).Middle_Pos;
                  end if;
               end loop;

               Trace := 10;
               Update_Cab_Dac_Display(First);
               First := False;
               Sensors_Changed := False;

            end loop Main_Loop;

         exception
            when Constraint_Error =>
               Ada.Text_Io.Put_Line("simrail2.Train crashed, constraint" & Trace'Img);
               Crashed := True;
         end; -- exception block
      end loop Outer_Loop;
   exception
      when Constraint_Error =>
         Ada.Text_Io.Put_Line("simrail2.Worker crashed, constraint" & Trace'Img);
         Crashed := True;
      when others =>
         Ada.Text_Io.Put_Line("simrail2.Worker task died" & Trace'Img);
         Crashed := True;
   end Worker;


   ---------------------------------------------------------------------
   --   Initialisation procedures:
   ---------------------------------------------------------------------

   -- load sensor positions from data in Simtrack2
   --
   procedure Init_Segment_Features is
      use Simtrack2;

      Mm    : Float;
      Segno : Seg_Index;
      S     : Segment;

      function Length_Of (
            Segno : Seg_Index )
        return Float is
         -- assume not a turnout
      begin
         if Segments(Segno).Kind = Aline then
            return Straight_Lines(Segments(Segno).Id).Length;
         else
            return Arcs(Segments(Segno).Id).Length;
         end if;
      end;

   begin
      if Segment_Features_Inited then
         return;
      end if;
      for Sno in Segment_Features'range loop
         Set_Length(Segment_Features(Sno), Length_Of(Sno));
      end loop;

      for N in Sensor_Id loop
         Segno := Sensor_Segment_Numbers(N);
         if  Segno /= Nc then
            -- if sensor used then add it to
            -- section (Nc => not used)
            Mm := Sensor_Segment_Mm(N);
            Add_Sensor(Segment_Features(Segno), N, Mm);
            if Mm = 0.0 then
               -- add to end of previous segment:
               Segno := Segments(Segno).Next(Reverse_Pol);
               S := Segments(Segno);
               if S.Kind = Aturnout then
                  -- must be a turnout that converged exactly on this sensor
                  -- (part of block 5).  Need to add to end of both branches
                  --
                  Add_Sensor(Segment_Features(S.Seg_St), N, Length_Of(S.Seg_St));
                  Add_Sensor(Segment_Features(S.Seg_Tu), N, Length_Of(S.Seg_Tu));
               else
                  Add_Sensor(Segment_Features(Segno), N, Length_Of(Segno));
               end if;
            end if;
         end if;
      end loop;
      -- 2007: boundaries done automatically
      --   Add_Sensor(Segment_Features(1), 13, Blk1_Length);
      --   ...

      Segment_Features_Inited := True;
   end Init_Segment_Features;


   procedure Init_Sensors is
   begin
      for N in Sensor_Id loop
         Sensor_States(N) := Off;
      end loop;
      for N in Sensor_Regs'range loop
         Sensor_Regs(N) := Int32defs.All_Off;
         Old_Sensor_Regs(N) := Int32defs.All_Off;
      end loop;
   end Init_Sensors;


   procedure Init_Turnouts (Tick_Interval : Positive) is
      Bit   : Dio192defs.Turnout_Status_Bit;
      Bitno : Turnout_Idx;
      Regno : Natural;
   begin
      Turnout_Pkg.Init_Timing (Tick_Interval);
      Put_Line("Turnout_Pkg.TFlip" & Turnout_Pkg.TFlip'Img);

      -- set all turnout bits showing straight, test_bit=0
      Turnout_Status_Regs := (others=>Dio192defs.All_In_Position);
      Turnout_Drive_Regs := (others=>Dio192defs.Turnout_Drive_Init);

      --  but turnouts will start in random time state heading Straight
      for Tn in Turnout_Id loop
         Init(Turnouts(Tn), Tn);
         -- Position  := Position_Of(Turnouts(Tn));  -- Straight or Middle
         Bit := Position_Bit(Turnouts(Tn));

         Regno := (Integer(Tn - 1)/8);
         Bitno := ((Tn - 1) mod 8);
         Turnout_Status_Regs(Turnout_Idx(Tn - 1)/8)((Tn - 1) mod 8) := Bit;
      end loop;
   end Init_Turnouts;


   -- find the smallest non-zero delay rounded to the nearest 1 ms (2005-06-23)
   -- above 10ms.
   --
   function Sleep_Quantum_Ms return Positive is
      Start,
      T            : Time;
      Dt           : Duration;
      Min_Tick_Dur : Duration := 1.0; --2005-06-23 calc min not average
      Result       : Integer;
   begin
      Start := Clock;
      loop
         T := Clock;
         exit when T /= Start;
      end loop;
      delay 0.010;  -- v1.7 extra call for gnat3.13 on pentiums
      Start := T;
      for I in 1..20 loop --2005-06-21 added loop
         delay 0.010;
         T := Clock;
         Dt := To_Duration(T - Start);  -- using Real_Time
         Start := T;
         if Dt < Min_Tick_Dur then
            Min_Tick_Dur := Dt;
         end if;
      end loop;
      Result := Integer(Min_Tick_Dur / 0.001);  --2005-06-21 prev .005)*5
      return Result;
   end Sleep_Quantum_Ms;

   -------------------------------
   -- calculate position record on some segment after a sensor
   -- assumes no turnouts involved
   --
   function Position_After(Sens : Sensor_Id; Plus_Mm : Float)
       return Train_Position is
      use Simtrack2;
      Result : Train_Position;
      -- Blk : Block_Idx;
      Segno : Seg_Index := Sensor_Segment_Numbers(Sens);
      Seg2 : Seg_Index;
      Dmm : Float := Plus_Mm;
      Mm : Float := Sensor_Segment_Mm(Sens) + Dmm;
      Len : Float;
   begin
      Len := Segment_Feature_Pkg.Length_Of(Segment_Features(Segno));
      while Mm > Len loop
         -- actually on next segment
         Seg2 := Segno;
         Segno := Segments(Segno).Next(Normal_Pol);
         if Segno = 0 then
            Segno := Seg2;  -- prevent going off end of a siding
            Mm := Len - 6.0;
         else
            Mm := Mm - Len;
            Len := Segment_Feature_Pkg.Length_Of(Segment_Features(Segno));
         end if;
      end loop;
      Result.Segno := Segno;
      Result.Mm := Mm;
      Result.To_Front := Normal_Pol;
      Put_Line("Position_After(" & Sens'Img & "," & Integer(Plus_Mm)'Img & "):"
         & Result.Segno'img & Result.Mm'img & Result.To_Front'img);
      return Result;
   end Position_After;

   --------------------------------

   -- procedure Setup
   -- initialises track blocks,
   -- adds sensors to track segments
   -- initialises trains, turnouts, blockdrivers
   -- and Simtack2.Display
   --
   -- should be called by Reset before Worker starts
   --
   procedure Setup (
      Num_Trains    : Positive;
      Init_Data     : Init_Array;
      Tick_Interval : Positive := 15          ) is
   --
      Posfr,
      Posbk,
      Pos_Mid     : Train_Position;
      Loco_Length, Len : Float;
   begin
      --Ada.Text_Io.Put_Line("Simulation setup started");
      -- Turn off interrupts before initialising all register values
      Interrupts_Enabled := False;

      Init_Sensors; -- all off

      Init_Turnouts(Tick_Interval);  -- (v2.2)

      --Tick_All_Turnouts;  -- call removed v2.2 (NB simtrack2 not ready)

      -- assume relays start in normal direction
      for N in Block_Id loop
         Init(Blockdrivers(N), N);
      end loop;

      -- Add sensors to blocks (if not done)
      --Put_Line("About to init sensorblocks");
      Init_Segment_Features;

      -- open Adagraph window (if not done)
      if Simtrack2.Display.X_Char = 0 then
         --Put_Line("About to init graphics");
         Simtrack2.Display.Init_Graphics;
      end if;

      -- Put_Line("About to draw track");
      Simtrack2.Display.New_Track;

      for N in Turnout_Id loop
         Simtrack2.Display.Change_Turnout_Status(N, Position_Of(Turnouts(N)));
      end loop;

      --Put_Line("About to init trains");
      Print_Train_Heading;
      -- set up a minimal train, no carriages in a block between the sensors
      Loco_Length := Simtrack2.Wheel_Base + 2.0*Simtrack2.Wheel_To_Reflector;
      --Posbk.Mm := 100.0;  -- assume about 50 mm clear of sensor
      --Posfr.Mm := Posbk.Mm + Loco_Length;  -- front
      --Posfr.To_Front := Normal_Pol;
      --Posbk.To_Front := Normal_Pol;
      Simrail2.Num_Trains := Train_ID(Num_Trains);
      for I in 1..Num_Trains loop
         if Init_Data(I).Id /= No_train then
            Posbk := Position_After(Init_Data(I).After, Init_Data(I).Mm);
            Len := Loco_Length
               + Float(Init_Data(I).Num_Carriages)*Simtrack2.Carriage_Length;
            Posfr := Position_After(Init_Data(I).After, Init_Data(I).Mm + Len);
            if Init_Data(I).Num_Carriages > 0 then
               Pos_Mid := Position_After(Init_Data(I).After, Init_Data(I).Mm + Len - Loco_Length);
               Init_Train(Trains(Train_Id(I)), Init_Data(I).Id, Posbk, Pos_Mid, Posfr, True, Len);
            else
               Init_Train(Trains(Train_Id(I)), Init_Data(I).Id, Posbk, Posbk, Posfr, False, Len);
            end if;
            Print_Train(Trains(Train_Id(I)));
         end if;
      end loop;

      --Interrupts_Enabled := False;
      Sensors_Changed := False;
      Crashed := False;
      Command_Buffer.Reset;
      Sensor_History.Reset;

      Timestamp;
      Ada.Text_Io.Put_Line(" Simulation " & Version
         & " setup finished");
   end Setup;

   ------------ reset everything -----------

   -- General version for up to 4 trains:
   procedure Reset (
         N_Trains  : Positive;
         Init_Data : Init_Array ) is
   --
      Sleep_Interval : Positive;
   begin
      Ada.Numerics.Float_Random.Reset(Rng);
      Simtrack2.Display.Init_Graphics;
      Ada.Text_Io.Put_Line("Train 1 has" & Init_Data(1).Num_Carriages'Img &
         " carriages.");
      Sleep_Interval := Sleep_Quantum_Ms; -- typically 15
      Ada.Text_Io.Put_Line("Your PC seems to have a delay quantum of" &
         Sleep_Interval'Img & "ms approx.  Simrail compensates.");
      Setup(N_Trains, Init_Data, Sleep_Interval); --10); -- set up the track
      --   if Slowness*10 > Sleep_Interval then
      --      Sleep_Interval := Slowness*10;
      --   end if;
      delay 1.0;  -- debug
      Crashed := True;  -- in case Worker is already busy
      Worker.Start(
         Tick_Ms => Sleep_Interval,
         --2005-06-21 was 10,
         Sleep_Ms => 10,
         --2005-06-21 was Sleep_Interval,
         N_Trains => N_Trains);
   end Reset;

   -- Easy to drive version:
   procedure Reset (
         N_Trains            : Positive := 2;
         N_Carriages_Train_1 : Natural  := 0
         -- Slowness            : Positive := 1  -- controls sleep_time (for testing)
         -- removed 2.1.8
         ) is
      --
      First_Name : String10 := "Thomas    "; -- 2.1.8 was Percy, even if no carriages
   begin
      if N_Carriages_Train_1 = 0 then First_Name := "Toby      "; end if;
      Reset(N_Trains,
         --((1,N_Carriages_Train_1,3,50.0,First_Name),
         -- (2,0,22,100.0,"Bill      "),
          --(4,0,25,100.0,"Trammie   "),     -- pre 2.1.8 was Toby
            -- (3,0, 51,100.0, "Diesel    ")) );  -- north siding
            ((1,N_Carriages_Train_1,3,50.0,First_Name),
          (2,4,23,440.0,"Bill      "),
          (4,0,25,100.0,"Trammie   "),     -- pre 2.1.8 was Toby
          (3,0, 7,20.0, "Diesel    ")) );  -- north siding
   end Reset;
   ----------------------------------------------------------------
   procedure Test is
   separate;
   ----------------------------------------------------------------

   procedure Dump (N_History_Items : in Integer := 40) is
      Abbrev : constant
      array (Turnout_Pos) of String(1..2) := ("St","Tu","Md");
      use      Ada.Text_Io,
         Ada.Float_Text_Io;

   begin
      Put_Line("---Simulator Dump:");
      --Put_Line(" at last tick number:" & Current_Tick'Img);
      Command_Buffer.Dump(N_History_Items);  -- v2.1.8 was 20
      Sensor_History.Dump(N_History_Items);  -- 2005-06-22 was 20
      Print_Train_Heading;
      for Tn in 1..Num_Trains loop
         Print_Train(Trains(Train_Id(Tn)));
      end loop;
      Put("Sensors on:");
      for I in Sensor_States'range loop
         if Sensor_States(I) = On then
            Put(I'Img);
         end if;
      end loop;
      New_Line;
      Put_Line("Turnout ");
      Put("  bits:");
      for I in Turnouts'range loop
         if Dio192defs."="(Position_Bit(Turnouts(I)), Dio192defs.Busy) then
            Put(" 0 ");
         else
            Put(" 1 ");
         end if;
      end loop;
      New_Line;
      Put("states:");
      for I in Turnouts'range loop
         Put(" " & Abbrev(Position_Of(Turnouts(I))));
      end loop;
      New_Line;
      Put_Line("Block voltages:");
      for I in Blockdrivers'range loop
         Put(Get_Signed_Voltage(Blockdrivers(I)), Fore=>3, Aft=>1, Exp=>0);
      end loop;
      New_Line;
      Put_Line("---End of dump");
   end Dump;

end Simrail2;
