with Ada.Text_IO, Ada.Integer_Text_IO;--testing
with Rail_element;
with Dac_Driver;
with Ada.Calendar;
with Unsigned_Types;
with Topolog2;
with buffers;

Package body Command_Analyser is
 use command_Dec;
 use Unsigned_Types;



   function Turnout_preference(My: in train_state_dec.Train_state; Turnout: in raildefs.Turnout_Id) return raildefs.turnout_pos is
   use raildefs;
      Turnout_preference:turnout_pos;
   begin
     -- Ada.Text_IO.Put_Line(My.Path_preference'Img);
      case My.Path_preference is
      when Center =>
        Turnout_preference:=rail_element.Get_Turnout_Pos(Turnout);
      when Left =>
         if(My.Absalute_Direction=Normal_Pol) then
            Turnout_preference:=Turned;
         else
            Turnout_Preference:=Straight;
         end if;

      when Right =>
         if(My.Absalute_Direction=Normal_Pol)then
            Turnout_Preference:=Straight;
         else
            Turnout_preference:=Turned;
         end if;

      end case;
      if(topolog2.Turnout_data(Turnout).Straight_Is_Left) then
         --these turnouts are flipped
         if(Turnout_Preference=Straight) then
            Turnout_Preference:=turned;
         else
            Turnout_Preference:=Straight;
         end if;

      end if;

     -- Ada.Text_IO.Put_Line(Turnout_preference'Img);
      if(Turnout_preference=Middle) then
         Ada.Text_IO.Put_Line("Middle was found!!!!!!!!!!");
      end if;
      return Turnout_preference;
   end turnout_Preference;

   procedure Assign_Ahead(My: in out train_state_dec.Train_state;
                          Me: in Raildefs.Train_Id) is
     use raildefs; -- for /=
      Turnout:Turnout_Idx;
      Alerted:Boolean;
      Buffer_error: Boolean;
      Nxt: topolog2.Train_Position;
   begin

      Nxt:= My.Front_sensor;
      topolog2.Advance_Pos(Pos=> Nxt,
                  Stopping=> false,
                  Need_Setting=>Alerted,
                  For_Turnout=>Turnout);
      if (Alerted) then
         topolog2.Advance_Pos(Pos=>Nxt,
                     Stopping=>false,
                     Setting=> rail_element.Get_Turnout_Pos(Turnout));
      end if;

       --The next sensor will be in another block, thus the block change will be somewhere between this even and the next.
      --Ada.Text_IO.put_Line("next line "& Nxt.Block_Num'img&" front of que"&My.owned_Blocks.get_Front'Img);
      if(Nxt.Block_Num /= My.owned_Blocks.get_Front) then
        -- ada.Text_IO.put_line("new block requested "&Nxt.Block_Num'img);
         if( not Rail_element.Assign_Block(Me,Nxt.Block_Num, My.Absalute_Direction)) then
            My.Block_waiting := Nxt.Block_Num;
            Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Me),0);
             My.Moving:= false;

         end if;
         My.owned_Blocks.insert(Nxt.Block_Num,Buffer_error);
          if(Buffer_error) then Ada.text_io.Put_Line("Block Buffer Error"); end if;
      end if;
   end Assign_Ahead;

   procedure Resume(My: in out train_state_dec.Train_state;
                    Me: in Raildefs.Train_Id) is
      Turnout_needed:Boolean;
      Turnout:raildefs.Turnout_Idx;
   begin
     -- ada.Text_IO.Put_Line("resumeing");
       My.Moving:= true;
         topolog2.Check_Turnout_Info_Need(My.Front_sensor, Turnout_needed, Turnout);
      topolog2.Resume(My.Front_sensor,My.Turnout_preference);
      Assign_Ahead(my,me);

   end Resume;

   procedure Free_blocks(My: in out train_state_dec.Train_state) is
   use raildefs; --for /=
unassign_block: Block_Id;
   begin

      while (My.owned_Blocks.get_Last /= My.Back_sensor.Block_Num) loop
         My.owned_Blocks.Remove(unassign_block);
         Rail_Element.Unassign_Block(unassign_block);
      end loop; --free up blocks that are not needed any longer

      end Free_blocks;

      procedure Turn_around (My: in out train_state_dec.Train_state;
                          Me: in Raildefs.Train_Id ) is

 use raildefs; --needed for /=
use Ada.Calendar;
      Temp_position:Topolog2.Train_Position;

      temp_block: block_id;
      Block_buffer_size: natural:= my.owned_Blocks.get_count;
      temp_buffer: Buffers.Block_buffer_stack;

      Unrequired_check: Boolean;
      For_Turnout:raildefs.Turnout_Idx;
      Buffer_error: Boolean;

      Power_down_time: Time ;

   begin --Turn_around
      Dac_driver.Set_Voltage(raildefs.Cab_Type(Me), 0); --turn off befor doing stuff, if not turned off we could short when we swap one rail befor the other.
      Power_down_time := Clock + 0.1;
      My.Moving:= false;

      my.Block_waiting:= raildefs.No_Block;
      my.Turnout_waiting:= raildefs.No_Turnout;
      my.Chain_Turnout_waiting:= raildefs.No_Turnout;
      my.Crossing_waiting:= raildefs.No_Crossing;
      my.Chain_Crossing_waiting:= raildefs.No_Crossing;
     my. Zone_waiting:= topolog2.No_Zone;


      My.Absalute_Direction:= raildefs.Opposite(My.Absalute_Direction); --just toggel this one, as it will be swaped, but we dont know from what to what.
      	ada.Text_IO.Put_Line("turn around to"&My.Absalute_Direction'Img) ;

      Temp_position:= My.Front_sensor; --swap scenors
      My.Front_sensor:= My.Back_sensor;
      My.Back_sensor:= Temp_Position;

      topolog2.Turn_Around(My.Front_sensor); --turn sensors next expectd around
      topolog2.Turn_Around(My.Back_sensor);

      if(My.Front_sensor.Next_Expected=0) then -- turnout invoved with turn around, thus maust mannually do this
         topolog2.Check_Turnout_Info_Need(My.Front_sensor, Unrequired_check, For_Turnout);
         My.Front_sensor.Next_Expected:=topolog2.Next_Sensor_Expected(My.Front_sensor, rail_element.Get_Turnout_Pos(For_Turnout));
      end if;

      if(My.Back_sensor.Next_Expected=0) then -- turnout invoved with turn around, thus maust mannually do this
         topolog2.Check_Turnout_Info_Need(My.Back_sensor, Unrequired_check, For_Turnout);
         My.Back_sensor.Next_Expected:=topolog2.Next_Sensor_Expected(My.Back_sensor, rail_element.Get_Turnout_Pos(For_Turnout));
      end if;


	delay until Power_down_time;  --wait until train has finished powering down;
      while(Block_buffer_size>0) loop

         My.owned_Blocks.remove(temp_block);
         rail_element.Change_Direction(Me,temp_block);  --take all the blocks this train owns and change their polarity
         temp_buffer.insert(temp_block,Buffer_error);
          if(Buffer_error) then Ada.text_io.Put_Line("Block Buffer Error"); end if;
         Block_buffer_size:= Block_buffer_size-1;
      end loop;

      Block_buffer_size:= temp_buffer.get_count;
      while(Block_buffer_size>0) loop

         temp_buffer.remove(temp_block);			--put the blocks this train owns back, but in reverse
         My.owned_Blocks.insert(temp_block,Buffer_error);
          if(Buffer_error) then Ada.text_io.Put_Line("Block Buffer Error"); end if;
         Block_buffer_size:= Block_buffer_size-1;
      end loop;

      free_blocks(My);

      Dac_driver.Set_Voltage(raildefs.Cab_Type(Me), My.Speed);  --done, so lets turn the train back on
      if(My.Speed>0) then
        Resume(My,me);
      end if;

   end Turn_around;


   procedure Front_analyses (My: in out Train_state_dec.Train_state;
                             Me: in Raildefs.Train_Id)is
 use topolog2;
 use Raildefs; --for comparions
      Alerted,Converging  :       Boolean;
      Turnout_Alert        :        Turnout_Idx;

      Turnout_Alert_check  :         Turnout_Idx;
      Turnout_setting      :        Turnout_Pos;
      Chained              :        Chain_Type;

      Turnout_Alert_check_chain  :         Turnout_Idx;
      Turnout_setting_chained:        Turnout_Pos;

      Crossing_Alert       :        Crossing_Idx;
      Crossing_chain_Alert       :        Crossing_Idx;

      Zone_alert           :        Zone_Idx;

      Entering_Crossing     :        Boolean;
      tram_crossing :Boolean;

   begin--front analyses
--      Ada.Text_IO.Put_line(my.Speed'img);
         Advance_Pos(My.Front_sensor,not My.Moving,Alerted,Turnout_Alert);
      if (Alerted) then
         Advance_Pos(My.Front_sensor,not My.Moving,My.Turnout_preference);
      end if;

      Check_Entering_Turnout(Pos=>My.Front_sensor,
                             Entering=>Alerted,
                             Which=>Turnout_Alert_check,
                             Converging=>Converging,
                             Required_Setting=>Turnout_setting,
                             Chained=>Chained);
      if( Alerted) then
         --Ada.Text_IO.Put("entereing turnout ");
         --Ada.Integer_Text_IO.Put(Integer(Turnout_Alert_check));
         if(Converging) then
           -- Ada.Text_IO.Put("converging");

            My.Turnout_preference:=Turnout_setting;

            if(Turnout_Data(Turnout_Alert_check).Swap_Pol_Tu and then Turnout_setting=turned and then My.Moving ) then
               My.Absalute_Direction:= Opposite(My.Absalute_Direction);
               --Ada.Text_IO.Put_line(" flip to "& My.Absalute_Direction'img);
            end if;

         else
         --   Ada.Text_IO.Put(" not converging  ");
            My.Turnout_preference:=Turnout_preference(My, Turnout_Alert_check);
         end if; --converging

         if( not Rail_element.Set_Turnout(me, Turnout_Alert_check, My.Turnout_preference)) then
            My.Turnout_waiting  := Turnout_Alert_check;
            Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Me),0);
            My.Moving:= false;
         end if;

        -- Ada.Text_IO.Put_line("");
         if(Chained/=no) then

          --  Ada.Text_IO.Put_Line("checking chain");
            Check_Entering_Chained_Turnout (Pos=>My.Front_sensor,
                                            Setting1        => My.Turnout_preference,
                                            Entering2       =>Alerted,
                                            Which2           =>Turnout_Alert_check_chain,
                                            Converging2       =>Converging,
                                            Required_Setting2 =>Turnout_setting_chained  ,
                                            Entering_Crossing =>    Entering_Crossing,
                                            Which_Crossing    => Crossing_Alert);
            if(Alerted and then Chained = immediate) then
              -- Ada.Text_IO.Put_Line("Chain");
              -- Ada.Integer_Text_IO.Put(Integer(Turnout_Alert_check));
               if(Converging) then
             --     Ada.Text_IO.Put_line("converging");

                  My.Turnout_preference_chain:=Turnout_setting_chained;

                  --if(Turnout_Data(Turnout_Alert_check_chain).Swap_Pol_Tu and then Turnout_setting_chained=turned ) then
                   --  My.Absalute_Direction:= Opposite(My.Absalute_Direction);
                   --  Ada.Text_IO.Put_line(" flippy-chain "& My.Absalute_Direction'img);
                 -- end if;

               else
              --    Ada.Text_IO.Put_line(" not converging  ");
                  My.Turnout_preference_chain:=Turnout_preference(My, Turnout_Alert_check_chain);
               end if;--chain converging

                  if( not Rail_element.Set_Turnout(me, Turnout_Alert_check_chain, My.Turnout_preference_chain)) then
                     My.Chain_Turnout_waiting  := Turnout_Alert_check_chain;
                     Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Me),0);
                     My.Moving:= false;

               end if;


            end if;--chain check

               if( Entering_Crossing) then
               --   Ada.Text_IO.Put_line("entereing crossing");
                  if( not Rail_Element.Assign_Crossing(Me, Crossing_Alert)) then
                     My.Crossing_waiting := Crossing_Alert;
                     Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Me),0);
                     My.Moving:= false;
               end if;

            end if;--crossing check

         end if;--chain /= no


      Check_Entering_Sideswipe_Zone(Pos=>My.Front_sensor,
                                    Turnout=>Turnout_Alert_check,
                                    Setting=>My.Turnout_preference,
                                    Entering=>Alerted,
                                    Which=>Zone_alert );


      if( Alerted) then
       --  Ada.Text_IO.Put_line("entereing sideswipe zone");
         if(not Rail_Element.Assign_Zone(Me, Zone_Alert)) then
            My.Zone_waiting := Zone_Alert;
            Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Me),0);
            My.Moving :=false;
         end if;

      end if; --entering sideswipe



      else -- not turnout


         Check_Entering_Crossing(Pos=>My.Front_sensor,
                                 Entering=>tram_crossing,
                                 Which=>Crossing_Alert,
                                 Next =>Crossing_chain_Alert); --this check will run for tram crossing. other crossing already handled
         if( tram_crossing) then
            if( not Rail_Element.Assign_Crossing(Me, Crossing_Alert)) then
               My.Crossing_waiting := Crossing_Alert;
               Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Me),0);
               My.Moving:= false;
            end if;
            if(Crossing_chain_Alert /= No_crossing) then
               if( not Rail_Element.Assign_Crossing(Me, Crossing_chain_Alert)) then
                  My.Chain_Crossing_waiting := Crossing_chain_Alert;
                  Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Me),0);
                  My.Moving:= false;
               end if;
            end if;--entering chain crossing

         end if;--entering crossing



      end if; --main turount






      if(  Is_Leaving_Powered_Track(My.Front_sensor)  )then
         turn_around(My, Me);
      end if;
      Check_Entering_Zone(Pos=>My.Front_sensor,
                          Entering=>Alerted,
                          Which=>Zone_Alert);
      if( Alerted) then
       --  Ada.Text_IO.Put_line("entereing zone");

         if(not Rail_Element.Assign_Zone(Me, Zone_Alert)) then
            My.Zone_waiting := Zone_Alert;
            Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Me),0);
            My.Moving :=false;
         end if;
         My.Max_speed:=Train_state_dec.Zone_Max_speed;--crawl up to boundry
         My.Pre_zone_speed:= My.Speed;
         if(My.Speed>My.Max_speed) then
            My.Speed:= My.Max_speed;
            Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Me),My.Speed);
         end if;
      end if;


            Assign_Ahead(my,me);


   end Front_analyses;

   procedure Back_analyses(My: in out  Train_state_dec.Train_state; Me: in Raildefs.Train_Id) is
 use topolog2;
      use raildefs; -- for comparison

      Alerted, leaving_turnout: Boolean;
      Turnout_advance: Turnout_Idx;
      Turnout_Alert: Turnout_Idx;
      Turnout_chain_Alert: Turnout_Idx;
      crossing_alert: Crossing_Idx;
      crossing_alert_chain: Crossing_Idx;
      zone_alert: topolog2.Zone_Idx;

      Converging: Boolean;
      Chained: topolog2.Chain_Type;
      Leaving_crossing: Boolean;

   begin--back analyses


      free_blocks(My);

      Advance_Pos(Pos=>My.Back_sensor,
                  Stopping=>not My.Moving,
                  Need_Setting=>Alerted,
                  For_Turnout=>Turnout_advance);

      if (Alerted) then
         Advance_Pos(Pos=>My.Back_sensor,
                     Stopping=>not My.Moving,
                     Setting=> rail_element.Get_Turnout_Pos(Turnout_advance) );
      end if;
      ---tests and such  \/  \/

      --Ada.Text_IO.Put_line("check if leaving turnout");
      Check_Leaving_Turnout(Pos        =>My.Back_sensor,
                            Leaving    => leaving_turnout,
                            Which      =>Turnout_Alert,
                            Converging =>Converging,
                            Chained    =>Chained);
      --  Ada.Text_IO.Put_line("check if leaving turnout2");
      if(leaving_turnout) then
       --  Ada.Text_IO.Put_line("leaving turnout");

         if(Chained/= no) then
            Check_Leaving_Chained_Turnout (
                                           Pos       	     	=> My.Back_sensor,
                                           Setting2   		=> rail_element.Get_Turnout_Pos(Turnout_Alert),
                                           Leaving1    		=> Alerted,
                                           Which1      		=> Turnout_chain_Alert,
                                           Converging1 		=> Converging, --useless
                                           Leaving_Crossing 	=> Leaving_crossing,
                                           Which_Crossing   	=> crossing_alert );
            if(Alerted) then
               --     Ada.Text_IO.Put_line("leaving chained turnout");
               null;
            end if; --chain trunout

         if(Leaving_crossing) then
            Rail_Element.Unassign_Crossing(Crossing_Alert);
            end if; -- leaving crossing



      end if;--chained /= no


         Check_Leaving_Sideswipe_Zone (pos=> My.Back_sensor,
                                       Turnout =>Turnout_Alert,
                                       Setting  =>rail_element.Get_Turnout_Pos(Turnout_Alert),
                                       Leaving  =>alerted,
                                       Which   =>zone_alert );
         if(alerted)then
         --   Ada.Text_IO.Put_line("leaving sideswipe");
            Rail_element.Unassign_Zone(zone_alert);
         end if;

      else-- not leaving turnout
       -- Ada.Text_IO.Put_line("not leaving turout");
         Check_Leaving_Crossing(Pos=>My.Back_sensor,
                                Leaving=>Alerted,
                                Which=>Crossing_Alert,
                                Next=>crossing_alert_chain); -- tram crossing leaving
         if(Alerted) then
         --   Ada.Text_IO.Put_line("leaving tram crossing");
            Rail_Element.Unassign_Crossing(Crossing_Alert);
            if(crossing_alert_chain /= raildefs.No_Crossing) then
               Rail_Element.Unassign_Crossing(crossing_alert_chain);
            end if;

         end if;


      end if;

         Check_Leaving_Zone ( Pos     => My.Back_sensor,
                                   Leaving  =>alerted,
                                   Which   =>zone_alert      );
         if(alerted) then
           -- Ada.Text_IO.Put_line("leaving zone");
            Rail_element.Unassign_Zone(zone_alert);
         my.Max_speed:= Train_state_dec.Abs_max_speed;
         My.Speed:=My.Pre_zone_speed;
         if(My.Moving)then
            Dac_Driver.Set_Voltage(Raildefs.Cab_Type(Me),My.Speed); --just incase skid inot leaving zone
         end if;

         end if;

   end Back_analyses;

   procedure Check_Waiting(My: in out Train_state_dec.Train_state; Me: in Raildefs.Train_Id) is
      use raildefs; -- for the /=
      use topolog2; -- for /=
      Waiting_on_Something: Boolean:= false;

   begin--check waiting
      if(My.Block_waiting/=No_Block) then
         ada.Text_IO.Put_Line("is block ok now?");
         if(Rail_element.Assign_Block(Me, My.Block_waiting, My.Absalute_Direction)) then
            My.Block_waiting:= No_Block;
         else
            Waiting_on_Something :=true;
         end if;
      end if;

      if(My.Turnout_waiting/=raildefs.No_Turnout)then
         ada.Text_IO.Put("is turnout ok now? ");


         if( My.wait_count>2 and then Rail_element.Set_Turnout(Me, My.Turnout_waiting, My.Turnout_preference))  then
            ada.Text_IO.Put_Line("yes");
            My.Turnout_waiting:= No_Turnout;
         else
            ada.Text_IO.Put_Line("still no");
            Waiting_on_Something :=true;
         end if;
      end if;

       if(My.Chain_Turnout_waiting/=raildefs.No_Turnout)then
         ada.Text_IO.Put("is chian turnout ok now? ");
         if(My.wait_count>2 and then Rail_element.Set_Turnout(Me, My.Chain_Turnout_waiting, My.Turnout_preference_chain))  then
		ada.Text_IO.Put_Line("yes");
		My.Chain_Turnout_waiting:= No_Turnout;
         else
            ada.Text_IO.Put_Line("still no");
            Waiting_on_Something :=true;
         end if;
      end if;

      if(My.Crossing_waiting/=raildefs.No_Crossing) then
         ada.Text_IO.Put_Line("is crossing ok now?");
         if(Rail_element.Assign_Crossing(Me, My.Crossing_waiting)) then
            My.Crossing_waiting:= No_Crossing;
         else
            Waiting_on_Something :=true;
         end if;
      end if;

       if(My.Chain_Crossing_waiting/=raildefs.No_Crossing) then
         ada.Text_IO.Put_Line("is chain crossing ok now?");
         if(Rail_element.Assign_Crossing(Me, My.Chain_Crossing_waiting)) then
            My.Chain_Crossing_waiting:= No_Crossing;
         else
            Waiting_on_Something :=true;
         end if;
    	end if;

      if(My.Zone_waiting /= No_Zone) then
         ada.Text_IO.Put_Line("is zone ok now?");
         if( rail_element.Assign_Zone(Me, My.Zone_waiting)) then
            My.Zone_waiting := No_Zone;
         else
            Waiting_on_something :=true;
         end if;
      end if;


      if(not Waiting_on_Something) then
         --Everthing is ready for the train to start going again.
         --ada.Text_IO.Put_Line("all ok");

         Dac_Driver.Set_Voltage(Cab_Type(Me),My.Speed);
         if(My.Speed>0 and then My.Moving=false) then
            resume(My,me);
         end if;

         My.wait_count:=0;
      else
         My.wait_count:= My.wait_count+1;
      end if;


   end Check_Waiting;

   procedure User_Input_anlyse(Me: in Raildefs.Train_Id; My: in out Train_state_dec.Train_state; Command: in Command_Type)is
      use raildefs; --needed for /=
      use Topolog2; --needed for =
      input_type:User_input_type := Command.Input.input_type;

   begin
      case input_type is
      when Speed =>
         if (Command.Input.Speed_increase = speed_up)then
            if My.Speed < My.Max_speed then
               My.Speed := My.Speed + 10;
            end if;

	    if My.Pre_zone_speed <Train_state_dec.Abs_max_speed then
           	 My.Pre_zone_speed :=My.Pre_zone_speed+10;
            end if;

         else --must be slow down
            if My.Speed > 0 then
               My.Speed := My.Speed - 10;
            end if;

             if My.Pre_zone_speed > 0 then
               My.Pre_zone_speed := My.Pre_zone_speed - 10;
            end if;
         end if;

         if(
            (My.Turnout_waiting=No_Turnout)  and then
              (My.Chain_Turnout_waiting=No_Turnout)  and then
              (My.Crossing_waiting=No_Crossing) and then
              (My.Block_waiting=No_Block) and then
              (My.Zone_waiting=No_zone)
           ) then
            Dac_driver.Set_Voltage(Cab_Type(Me), My.Speed);

            if(My.Speed>0 ) then
               if My.Moving=false then
               Resume(My,me);
        	end if;
            else
               My.Moving:=false;
               my.Block_waiting:= raildefs.No_Block;
              my. Turnout_waiting:= raildefs.No_Turnout;
              my. Chain_Turnout_waiting:= raildefs.No_Turnout;
              my. Crossing_waiting:= raildefs.No_Crossing;
              my. Chain_Crossing_waiting:= raildefs.No_Crossing;
              my. Zone_waiting:= topolog2.No_Zone;
            end if;

            --still allow user to change held speed, but dont actually change unless the train is not waiting
            -- this stops a user forcing the train to start again if it was waiting.
         end if;

      when Polarity =>
         if( Command.Input.Polarity_update /= My.Relative_Direction) then
            My.Relative_Direction := Command.Input.Polarity_update;
            Turn_around(My, Me);

         end if;


      when Path =>
         My.Path_preference := Command.Input.Path_preference;
      end case;

   end User_Input_anlyse;


   procedure Analyse(Me: in Raildefs.Train_Id; My: in out train_state_dec.Train_state; Command: in Command_Type)is
      recived_sensor:Raildefs.Sensor_Idx;
      recived_State : Raildefs.Sensor_Bit;

      --My:Train_state := Train_state_array(Me);

   begin--Analyse
      --Ada.Text_IO.Put("train:");
      --Ada.Integer_Text_IO.Put(Integer(me));
      -- Ada.Text_IO.Put_line("");
      case Command.Eve is
         when User_Input =>
            User_Input_anlyse(Me, My, command);
         when hall_event=>
            recived_sensor:= Command.Sensor;
            recived_State:= Command.State;

            --look if this sensor is of consern to this train
            if( Integer(recived_sensor) = Integer(My.Front_sensor.Next_Expected)) then
               --Ada.Text_IO.Put_line("just as expected-front");
               Front_analyses(My,Me);

            elsif ( Integer(recived_sensor) = Integer(My.Back_sensor.Next_Expected)) then
               -- Ada.Text_IO.Put_line("just as expected-back");
               Back_analyses(My,Me);

            else
               --  Ada.Text_IO.Put_line("not for me");
               null;
            end if;

         when Heartpulse =>
            Check_Waiting(My, Me);

      end case;
      -- Train_state_array(Me):= My;
   end Analyse;


end Command_Analyser;


