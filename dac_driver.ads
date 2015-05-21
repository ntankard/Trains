with Raildefs;
with Unsigned_Types;         use Unsigned_Types;
package Dac_Driver is

   procedure Init;
   procedure Set_Voltage (D : in Raildefs.Dac_Id;
                          Value : in Unsigned_8);

   end Dac_Driver;
