with command_Dec;
with Raildefs;
package Train_command is

      procedure Init(Train_id:Raildefs.Train_ID);
   procedure add(Train:Raildefs.Train_IDx;
                 Command: command_Dec.Command_Type);


end Train_command;











