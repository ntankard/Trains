with train_state_dec;
with raildefs;
with command_Dec;
Package Command_Analyser is





   procedure Analyse(Me: in Raildefs.Train_Id; My: in out train_state_dec.Train_state;  Command: in command_Dec.Command_Type);
end Command_Analyser;
