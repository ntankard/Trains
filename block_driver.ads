with Raildefs;

package Block_Driver is

   procedure Init;

   procedure Set_Cab (B : in Raildefs.Block_Id;
                      Cab : in Raildefs.Cab_Type);

   procedure Set_Polarity(B   : in Raildefs.Block_Id;
                          Pol : in Raildefs.Polarity_Type);

   procedure Flip_Polarity(B   : in Raildefs.Block_Id);

end Block_Driver;

