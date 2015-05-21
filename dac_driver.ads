-- Dac_Driver
--
-- Software level driver for the digital to analogue converter
--
-- Operations
-- Initialize - Sets up the Dac hardware and software
-- Set_Voltage - Sets output voltage of the Dac
--
-- Nicholas Tankard(7191928), Angela Lau (7160852), Phillip Smith (7191731)
-- Last Revision (13/04/2015)

with Raildefs;
with Unsigned_Types;

package Dac_Driver is

   -- Init:
   -- Sets up the Dac hardware and software
   procedure Init;

   -- Set_Voltage:
   -- Sets output voltage of the Dac
   -- Note: The output of the Dac is only approximately equal to the value set
   -- in this procedure
   -- param: D		The ID of the Dac
   -- Value	The value to set in volts
   procedure Set_Voltage
     (D     : in Raildefs.Dac_Id;
      Value : in Unsigned_Types.Unsigned_8);

end Dac_Driver;
