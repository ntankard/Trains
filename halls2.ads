-- Halls2
--
-- This package provides the Hall objects that sense the 
-- position of the trains.  These active objects are implemented as
-- tasks that send information to the rest of the system when a Hall
-- sensor is triggered. The implementation uses interrupts with IRQ 5 and 7.

-- Written by John McCormick, March 2002
-- modified by Rob Allen for Swinburne, March 2008
-- version 1.1 13-May-08 Enable made public
--
with Raildefs;
package Halls2 is

   ----------------------------------------------------------------------------
   procedure Initialize;
   -- Set up the Hall sensor interface electronics (two INT32 cards).
   --  
   -- May be called any time you wish to reset the Hall sensor electronics to 
   -- a known, valid state.
   --
   -- Preconditions   : none
   --
   -- Postconditions  : The Hall sensor interface electronics are initialized.
   --                   Hall sensor interrupts are disabled.

   ----------------------------------------------------------------------------
   -- Install interrupt analyzer
   -- (Swinburne, 2008)
   --
   procedure Install_Int_Handling(Analyzer : Raildefs.Proc4_Access);
   -- Install a user-supplied second-stage handler for interrupts.
   --  
   -- Call once-only after Initialize.
   --
   -- Preconditions   : Initialize was called sometime previously
   --
   -- Postconditions  : The protected procedure passed (by access) is installed
   --                   so it will be called when either INT32 card raises an
   --                   interrupt.
   --                   Hall sensor interrupts are enabled.

   ----------------------------------------------------------------------------
   procedure Enable;  -- for convenience called from Install_Int_Handling
      
   procedure Disable;
   -- Enable or disable Hall sensor interrupts 
   --
   -- Preconditions : Initialize was called sometime previously 
   --
   -- When enabled, a triggered Hall sensor will generate an interrupt received
   -- by a task that calls back to the protected procedure (the "analyzer")
   -- previously installed.  The callback supplies an offset and 4 bytes 
   -- containing the current state of 32 sensors.
   --
   -- When disabled, the triggering of Hall sensors will be ignored
   --
   -- Note: The Initialize procedure disables Hall sensor interrupts.

   ----------------------------------------------------------------------------
   function State_Of (Sensor : in Raildefs.Sensor_ID) return Raildefs.Sensor_Bit;
   -- Returns On if the given Hall sensor currently has a magnet
   -- over it.  
   --
   -- Preconditions : Initialize was called sometime previously.
   --                 Interrupts need not be enabled for this function.
  
end Halls2;