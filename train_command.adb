with Ada.Text_IO, Ada.Integer_Text_IO;--testing-with Topolog2;
with Dac_Driver;
with Rail_element;
with Buffers;
with Topolog2;
with Command_Analyser;
with Unsigned_Types;
with train_state_def;
package body Train_command is
use command_def;


Command_Buffer_presort_array : Buffers.Command_Buffer;
Command_Buffer_array : array (Raildefs.Train_Id) of Buffers.Command_Buffer;







   task sorter is
      pragma Priority (30) ;
   end sorter;

   task type Worker_Thread(Me: Raildefs.Train_Id )is
      pragma Priority (30) ;
   end Worker_Thread;

   Worker1: Worker_Thread(1);
   Worker2: Worker_Thread(2);
   Worker3: Worker_Thread(3);
   Worker4: Worker_Thread(4);

   task body Worker_Thread is
        Command: Command_Type;
      use Raildefs;
      Error:Boolean;
   begin --Worker_thread
      loop

            Command_Buffer_array(Me).Remove(Command, Error);
 	if(Error)then
            Ada.Text_IO.Put_line("Train " & Me'Img & " is falling behind. Buffer is full");
         end if;

         --for testing. \/ \/
         --set to a train number that is in train_id to run
         --set to no_trian to disable
      if Me = 0 then
         case Command.Eve is
         when User_Input =>
            null;
            when hall_event=>
               null;
              Ada.Text_IO.Put("sensor: ");
               Ada.Integer_Text_IO.Put(Integer(Command.Sensor));
               if( Command.State = On) then
                    Ada.Text_IO.Put_line(" on");

              else
                   Ada.Text_IO.Put_line(" off");
               end if;
            when Heartpulse =>
               null;
                --Ada.Text_IO.Put_line("boomp-boomp"&Me'img);
         end case;

         end if;
            --for testing /\ /\
         Command_Analyser.Analyse(Me,train_state_def.Train_state_array(Me),Command);

      end loop;
       exception
         when others=>
            Ada.Text_IO.Put_Line("Oh golly misa. It looks like y'all's code done break. Dont know how's but");
   end Worker_Thread;

   task body sorter  is
      use Raildefs; -- for compare
      Command: Command_Type;
      Allocated: Boolean:=false;
           Error:Boolean;
   begin --sorter
      loop
      Command_Buffer_presort_array.Remove(Command, Error);
   --Ada.Text_IO.Put_line("sorting");
      if(Error)then
         Ada.Text_IO.Put_line("sorting worker is falling behind. Buffer is full");
      end if;

      for T in Train_Id loop
            -- Ada.Text_IO.Put_line(T'img);
         if (Command.Sensor = train_state_def.Train_state_array(T).Front_sensor.Next_Expected or else Command.Sensor = train_state_def.Train_state_array(T).Back_sensor.Next_Expected) then
            Command_Buffer_array(T).insert(Command);
            Allocated:= true;
              --Ada.Text_IO.Put_line("send to "&T'img);
         end if;

      end loop;

      if(not Allocated) then --failed to find a train that requires this sensor, send to all
         Command_Buffer_array(1).insert(Command);
         Command_Buffer_array(2).insert(Command);
         Command_Buffer_array(3).insert(Command);
         Command_Buffer_array(4).insert(Command);
           --Ada.Text_IO.Put_line("failed to find train");
      end if;
      end loop;
      exception
         when others=>
         Ada.Text_IO.Put_Line("sorter crash");

   end sorter;


   procedure add(Train:Raildefs.Train_IDx;
                 Command: command_def.Command_Type) is
use Raildefs; -- for compare
   begin --add_hall

      if(Train=0) then

            Command_Buffer_presort_array.insert(Command); -- if the train to send to is unknonw, send to sorter
         else
            Command_Buffer_array(Train).insert(Command); --command was sent with train id, send to correct train
         end if;
   end add;

   procedure Init(Train_id:Raildefs.Train_ID) is
      use Unsigned_Types;
      Buffer_error: Boolean;
   begin--Init
      case Train_id is
      when 1=>
         train_state_def.Train_state_array(Train_id).owned_Blocks.insert(2,Buffer_error);
         if(Buffer_error) then Ada.text_io.Put_Line("Block Buffer Error"); end if;

         train_state_def.Train_state_array(Train_id).Front_sensor:=Topolog2.Initialise_After(3);
         train_state_def.Train_state_array(Train_id).Back_sensor:=Topolog2.Initialise_After(3);
      when 2 =>
         train_state_def.Train_state_array(Train_id).owned_Blocks.insert(20,Buffer_error);
         if(Buffer_error) then Ada.text_io.Put_Line("Block Buffer Error"); end if;

         train_state_def.Train_state_array(Train_id).Front_sensor:=Topolog2.Initialise_After(22);
         train_state_def.Train_state_array(Train_id).Back_sensor:=Topolog2.Initialise_After(22);
      when 3 =>
         train_state_def.Train_state_array(Train_id).owned_Blocks.insert(19,Buffer_error);
         if(Buffer_error) then Ada.text_io.Put_Line("Block Buffer Error"); end if;

         train_state_def.Train_state_array(Train_id).Front_sensor:=Topolog2.Initialise_After(51);
        train_state_def.Train_state_array(Train_id).Back_sensor:=Topolog2.Initialise_After(51);
      when 4 =>
         train_state_def.Train_state_array(Train_id).owned_Blocks.insert(1,Buffer_error);
          if(Buffer_error) then Ada.text_io.Put_Line("Block Buffer Error"); end if;
        train_state_def. Train_state_array(Train_id).Front_sensor:=Topolog2.Initialise_After(25);
         train_state_def.Train_state_array(Train_id).Back_sensor:=Topolog2.Initialise_After(25);
        train_state_def. Train_state_array(Train_id).Speed:=160;
      end case;

       Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Train_id),train_state_def.Train_state_array(Train_id).Speed);
	 if(train_state_def.Train_state_array(Train_id).Speed>0) then
         train_state_def.Train_state_array(Train_id).Moving:= true;
      else
      train_state_def.Train_state_array(Train_id).Moving:= false;
            end if;
      if(Rail_element.Assign_Block(Train_id,train_state_def.Train_state_array(Train_id).owned_Blocks.get_Front,
                                   train_state_def.Train_state_array(Train_id).Absalute_Direction)) then
         null;
      end if;
       if(Rail_element.Assign_Block(Train_id,train_state_def.Train_state_array(Train_id).owned_Blocks.get_Last ,
                                   train_state_def.Train_state_array(Train_id).Absalute_Direction)) then
         null;
         end if;




      end Init;

end Train_command;

