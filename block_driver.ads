-- Block_Driver
--
-- Software level driver for the blocks
--
-- Nicholas Tankard(7191928), Angela Lau (7160852), Phillip Smith (7191731)
-- Last Revision (21/05/2015)

with Raildefs;

package Block_Driver is

   -- Init:
   -- Sets up the block driver hardware
   -- Clears the local storage objects and sets all blocks to CAB 0 with
   -- positive polarity
   procedure Init;

   -- Set_Cab:
   -- Sets a block's CAB ID
   -- pre: Initialize was called sometime previously
   -- param: B             The ID of the block
   -- Cab          The cab type to set
   -- Note: The blocks polarity is not changed
   -- Note: If the hardware registers are modified outside of this package
   --then this call may change other block values
   procedure Set_Cab (B : in Raildefs.Block_Id; Cab : in Raildefs.Cab_Type);

   -- Set_Polarity:
   -- Sets a block's polarity
   -- pre: Initialize was called sometime previously
   -- param: B             The ID of the block
   -- Pol          The polarity to set
   -- Note: The blocks CAB number is not changed
   -- Note: If the hardware registers are modified outside of this package
   --then this call may change other block values
   procedure Set_Polarity
     (B   : in Raildefs.Block_Id;
      Pol : in Raildefs.Polarity_Type);

   -- Flip_Polarity:
   -- Reverses the polarity of a block
   -- pre: Initialize was called sometime previously
   -- param: B             The ID of the block
   -- Note: This function is safe to call even if the CAB is 0
   -- Note: If the hardware registers are modified outside of this package
   --then this call may change other block values
   procedure Flip_Polarity (B : in Raildefs.Block_Id);

end Block_Driver;
