with Raildefs;
with Dio192defs;
package Turnout_Driver is

   procedure Init;
   procedure Pull(Tn  : in Raildefs.Turnout_Id;
                  Dir : in Raildefs.Turnout_Pos);
 procedure Poll (Tn  : in Raildefs.Turnout_Id;
                     State : out Dio192defs.Turnout_Status_Bit;
                       Dir: out Raildefs.Turnout_Pos);
   end Turnout_Driver;
