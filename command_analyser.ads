with Train_state_def;
with raildefs;
with command_def;
Package Command_Analyser is





   procedure Analyse(Me: in Raildefs.Train_Id; My: in out train_state_def.Train_state;  Command: in command_def.Command_Type);
end Command_Analyser;
