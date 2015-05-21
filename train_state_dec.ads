with Topolog2;
with raildefs;
with Unsigned_Types;
with Buffers;
with command_Dec;
package Train_state_dec is

   Abs_max_speed: Unsigned_Types.Unsigned_8:= 250;
   Zone_Max_speed: Unsigned_Types.Unsigned_8:= 160;
  type Train_state is record
      Front_sensor   : Topolog2.Train_Position;
      Back_sensor:  Topolog2.Train_Position;

 	owned_Blocks: Buffers.Block_Buffer;

      Path_preference: command_Dec.Path_change ;


      Speed: Unsigned_Types.Unsigned_8;
      Max_speed: Unsigned_Types.Unsigned_8:= Abs_max_speed;
      Pre_zone_speed: Unsigned_Types.Unsigned_8;

      Relative_Direction : raildefs.Polarity_Type := raildefs.Normal_Pol;
      Absalute_Direction : raildefs.Polarity_Type :=raildefs.Normal_Pol;

      Block_waiting: raildefs.Block_Idx := raildefs.No_Block;
      Turnout_waiting:  raildefs.Turnout_Idx := raildefs.No_Turnout;
      Chain_Turnout_waiting:  raildefs.Turnout_Idx := raildefs.No_Turnout;
      Crossing_waiting:  raildefs.Crossing_Idx := raildefs.No_Crossing;
      Chain_Crossing_waiting:  raildefs.Crossing_Idx := raildefs.No_Crossing;
      Zone_waiting : topolog2.Zone_Idx:= topolog2.No_Zone;
      wait_count:natural:=0;

      Turnout_preference: raildefs.Turnout_Pos;
      Turnout_preference_chain: raildefs.Turnout_Pos;

      Moving : Boolean;

   end record;

end Train_state_dec;
