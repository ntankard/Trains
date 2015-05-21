with Train_command;
with command_Dec;
with Heartbeat;
package body train is
   use command_Dec;


procedure Init is
   begin--Init
      Train_command.Init(1);
      Train_command.Init(2);
            Train_command.Init(3);
      Train_command.Init(4);
   end Init;

	procedure Recive_sensor_event(Sensor: Raildefs.Sensor_Idx;
                               State : Raildefs.Sensor_Bit) is
        Command: Command_Type;
   begin --Recive_sensor_event
      Command :=(hall_event,Sensor,State);
      Train_command.add(0,Command);
   end Recive_sensor_event;

   procedure Change_Direction(Train: Raildefs.Train_Idx;
                              Dir: Raildefs.Polarity_Type) is
      Command: Command_Type;
      Input : User_input_record;
   begin
      Input:=(Polarity, Dir);
      Command:=(user_input,Input );
      Train_command.add(Train,Command);
   end Change_Direction;

   procedure Change_Speed(Train: Raildefs.Train_Idx;
                          Speed_change: command_Dec.Speed_change) is
      Command: Command_Type;
      Input : User_input_record;
   begin
      Input:=(Speed, Speed_change);
      Command:=(user_input,Input );
      Train_command.add(Train,Command);
   end Change_Speed;

   procedure Change_Path(Train: Raildefs.Train_Idx; Path_change:command_Dec.Path_change) is
      Command: Command_Type;
      Input : User_input_record;
   begin
      Input:=(Path, Path_change);
      Command:=(user_input,Input );
      Train_command.add(Train,Command);
   end Change_Path;


end train;
