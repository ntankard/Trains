with Train_command;
with command_def;
with ada.Calendar;
with raildefs;
with Ada.Text_IO, Ada.Integer_Text_IO;--testing
package body Heartbeat is

   Task body tick is
      use command_def;
      use ada.Calendar;
      Future_time : Time := Clock;
      wait_time:Duration:=0.25; --times four trains is one heart beat per second
      train_to_call : positive :=1;
      Command: Command_Type:=(Eve=>HeartPulse);

   begin
      loop

         train_command.add(raildefs.Train_Id(train_to_call),Command);
         train_to_call:= (train_to_call) mod raildefs.Max_Trains +1;

         Future_time:=Future_time + wait_time;
         delay until Future_time;

      end loop;

   exception
         when others => Ada.Text_IO.Put_line("crash");
   end tick;
end Heartbeat;
