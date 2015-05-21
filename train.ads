with Raildefs;
with command_Dec;
package train is
	procedure Recive_sensor_event(Sensor: Raildefs.Sensor_Idx;
                               State : Raildefs.Sensor_Bit);
   procedure Init;

   procedure Change_Direction(Train: Raildefs.Train_Idx; Dir: Raildefs.Polarity_Type);
      procedure Change_Speed(Train: Raildefs.Train_Idx; Speed_change:command_Dec.Speed_change);
   procedure Change_Path(Train: Raildefs.Train_Idx; Path_change:command_Dec.Path_change);

end train;
