-- Simrail.ads  version 2.0
-- Simulate all hardware associated with the railroad including the
-- interface cards.  This package is meant to be called by student-written
-- control software via the non-DOS version of Io_Ports.
-- Simulated interrupts from the simulated interrupts card are by call-back
-- to a protected procedure.  This call-back nust be registered
-- using procedure Install_Int_Handling (orig Attach_Handler).
-- Based on simulation.ads by Winston Fletcher, 28 September 1999
-- Modification history:
-- 15-Nov-99 rka remove bugs
-- 23-Mar-00 rka : add strong typing, so fix bug in Checksensor
-- 26-Mar-00 rka : First version named simrail, restructure to eliminate
--             protected objects and use only one thread.
-- version 1.0: 24-Apr-00 rka  (has interrupt simulation)
-- version 1.5: 15-May-01 rka  (interrupt simulation matches dio_int4)
-- version 1.5.2: 10-Jul-01 rka  (internal changes, fifth "register")
-- Version 1.7    05-Aug-03 (crash w/o exception, dump improved)
-- version 1.8 26-Apr-04 hide use of package Interfaces for compatibility
--             with MaRTE_OS
-- version 1.9.9 remove old Attach_Handler
-- version 2.0.0: 8-Jul-2007 rka first simrail2, 8-byte interrupts
-- version 2.0.4: 2-Mar-2008: int32 simulation, as two 32-bit cards
-- version 2.1.1: 19-May-2008 generalised Reset for up to 4 trains
-- version 2.1.8  31-Mar-2011 internal improvements to dump display, param added
--             to Dump; param Slowness removed from old Reset; sound display.
--
-- Author: Rob Allen, Swinburne University of Technology
--
with Unsigned_Types;
with Raildefs;

package Simrail2 is

   ----------------------------------------------------
   -- types for Reset
   ----------------------------------------------------
   subtype String10 is String(1..10);
   type Init_Record is
      record
         Id            : Raildefs.Train_Idx;  -- eg 4
         Num_Carriages : Natural;             -- eg 0
         After         : Raildefs.Sensor_Idx; -- eg 26
         Mm            : Float;               -- > 5.0, eg 50.0
         Name          : String10;            -- eg "Toby      "
      end record;
   type Init_Array is array (1 .. 4) of Init_Record;

   --------------------------------------------------------------------------
   --             Subprogram Declarations
   --------------------------------------------------------------------------

   function Version return String;

   -- Install interrupt analyzer (the simrail version of halls2
   -- calls this procedure.)
   --
   procedure Install_Int_Handling (
         Analyzer : Raildefs.Proc4_Access );

   --------------------------------------------------------------------------
   -- Reset must be called to start (restart n.y.i.) the simulation.
   procedure Reset (
         N_Trains            : Positive := 2;
         N_Carriages_Train_1 : Natural  := 0 -- implemented (1.4)
         --Slowness            : Positive := 1  -- controls sleep_time, default 10ms or 55ms.
         );

   -- General version for up to 4 trains:
   procedure Reset (
         N_Trains  : Positive;
         Init_Data : Init_Array );

   procedure Test;  -- interactive using Text_Io

   procedure Dump(N_History_Items : in Integer := 40);  -- for debugging after a crash

   --------------------------------------------------------------------------
   --             Exception declarations
   --------------------------------------------------------------------------
   Train_Crash : exception; -- raised by NEXT call of the following reads/write
   -- after a simulated crash or derailment.  Dump can be called subsequently,
   -- but Reset doesnt work.

   --------------------------------------------------------------------------
   -- The following are meant to be called ONLY by the Simrail versions of
   -- package Io_Ports and Halls2, NOT BY ANY OTHER CLIENT.  They simulate access to
   -- the  registers on the various simulated i/o cards.
   function Read_Reg (
         Addr : Unsigned_Types.Unsigned_16 )
     return Unsigned_Types.Unsigned_8;
   procedure Write_Reg (
         Address : in     Unsigned_Types.Unsigned_16;   -- will be buffered
         Value   : in     Unsigned_Types.Unsigned_8 );  -- and change simulated cards.

end Simrail2;