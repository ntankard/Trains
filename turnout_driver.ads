-- Turnout_Driver
--
-- Software level driver for the turnouts
--
-- Nicholas Tankard(7191928), Angela Lau (7160852), Phillip Smith (7191731)
-- Last Revision (13/04/2015)

with Raildefs;
with Dio192defs;
package Turnout_Driver is

   -- Init:
   -- Sets up the turnout driver hardware
   -- Clears the local storage objects and sets all turnouts to straight
   procedure Init;

   -- Pull:
   -- Sets the position of a turnout
   -- This function will wait untill the turnout begins to move befor
   --returning.
   -- As a result of this Is_Busy is guaranteed to be true after this returns
   -- pre: Initialize was called sometime previously
   -- param: TN         The ID of the turnout
   -- param: Dir	The direction to set
   -- Note: If the hardware registers are modified outside of this package
   -- then this call may change other turnouts
   procedure Pull
     (Tn  : in Raildefs.Turnout_Id;
      Dir : in Raildefs.Turnout_Pos);

   procedure Poll
     (Tn    : in Raildefs.Turnout_Id;
      State : out Dio192defs.Turnout_Status_Bit;
      Dir   : out Raildefs.Turnout_Pos);

   procedure Check_All;

end Turnout_Driver;
