with command_def;
with Raildefs;
package Train_command is

      procedure Init(Train_id:Raildefs.Train_ID);
   procedure add(Train:Raildefs.Train_IDx;
                 Command: command_def.Command_Type);


end Train_command;











