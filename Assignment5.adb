-- Assignment 5 for Real time programing train
--
-- enter commands into Swindows dialog box to have train perform tasks.
--
--
--
-- requires Adagraph, (vsn 2.3.0 +), Raildefs (3.0), Dio192defs,
--  Dda06defs, Dda06defs, and Halls2 & Io_Ports for Simrail2
--
-- Original (test5): 16-Nov-94
-- version 2.0.6  11-Mar-08 for simrail2 2.0.6
-- version 2.0.8   3-Apr-08 improved Dialog_Loop, made it compilable
-- version 2.0.9  13-May-08 Halls2 init, Pull param
-- version 2.1.0  20-Feb-09 moved interrupt handler into separate package
-- version 2.1.1  16-Mar-10 hide protected inside the interrupt package
-- version 2.2    19-Mar-13 include 'D'=Slogger in dialog, 'S' now simrail dump
-- version 2.3     9-Mar-15 number is prefix in dialog, for raildefs 3.0
-- Lab 5 version 1 1-Apr-15 renamed for lab 5 work
--
-- Copyright: Dr R. K Allen, Faculty of SET, Swinburne UT
-- (For use within the unit HIT3047/SWE30001 Real-Time Programming only.)
--
-- Contributions by Phillip Smith, Nicolus Tankard, Angela Lau
-- for the unit HIT3047/SWE30001 Real-Time Programming only.

with Ada.Text_IO, Ada.Integer_Text_IO;
with Simrail2;
with Io_Ports;
with Swindows;                                 use Swindows;
with Unsigned_Types;                           use Unsigned_Types;  -- 1.8
with Raildefs;                                 use Raildefs;
with  Int32defs;
with Halls2;
with Interrupt_Hdlr;  -- 2.1
with Slogger;  -- 2.2
with Dac_Driver;
with Block_Driver;
with Turnout_Driver;
with Sound_Manager;
with track_setup;
with Dio192defs;
with Train_manager;
with command_def;

procedure Assignment5 is
   --package Iio is new Ada.Text_Io.Integer_Io(Integer);
   package Iio renames Ada.Integer_Text_IO;

   W_In, W_Info, W_Interrupts : Swindows.Window;

   -- vars and code for dio192: -------
   --





 procedure Init_Dio is
   begin

      -- init 24-bits mixed input & output you write  ??
	dac_driver.Init;
	Turnout_driver.init;
      Block_driver.init;
      Train_manager.Init;
   end Init_Dio;








   -- for Dialog_Loop: --

   Pos0 : constant := 48;  -- ASCII '0'
   Number : Integer := 0;  -- prefix for commands

   -------- Dac_Command ------------------------
   -- User syntax:  ndm
   -- where n and 'd' already read, supports m in 0..9
   -- Here voltage is m*factor (not very nice).
   ----------------------------------------------
  procedure Dac_Command is
      C   : Character;
      Dac : Cab_Type := 0;
      V   : Unsigned_8;
   begin
      if Number in 1..4 then
         Dac := Dac_Id(Number);
         Get_Char (W_In, C);
         if C in '0' .. '9' then
            V := Unsigned_8((Character'pos (C) - Pos0) * 27);
            Dac_driver.Set_Voltage (Dac, V);
         else
            Dac := 0;
         end if;
      end if;
      if Dac = 0 then
         Put_Line (W_In, "command ignored");
         delay 1.0;
      end if;
      Number := 0;  -- we used it
   end Dac_Command;



   procedure track_command is
   begin
       if Number =0  then
         	track_setup.oval_setup;
      elsif Number =8 then
             track_setup.firgur8_setup;
      else
          track_setup.flip;
   	end if;

                Number := 0;  -- wasnet used, but just reset it
   end track_command;


 procedure block_Command is
      C   : Character;
      Block : Block_Idx := 0;
      D   : Unsigned_8;
   begin
      if Number in 1..Num_Blocks then
         Block := Block_Id(Number);
         Get_Char (W_In, C);
         if C = '+'  then

            block_driver.Set_Polarity(Block, Normal_Pol);

         elsif C = '-'  then
            block_driver.Set_Polarity(Block, Reverse_Pol);
         else
            Block := 0;
         end if;

         if Block /= 0  then
            Get_Char (W_In, C);

         	if C in '0' .. '4' then
          	  D := Unsigned_8((Character'pos (C) - Pos0));  -- or something
           	 block_driver.Set_Cab (Block, Cab_Type(D));
         	else
           	 Block := 0;
        	 end if;
         end if;

      end if;
      if Block = 0  then
         Put_Line (W_In, "command ignored");
         delay 1.0;
      end if;
      Number := 0;  -- we used it
   end block_Command;

   procedure turnout_command(Dir: Turnout_Pos) is
   	turnout : Turnout_Id;
   begin
      if Number in 1..Num_Turnouts then
         turnout := Turnout_Id(Number);
         turnout_driver.Pull(turnout,Dir);

   else
       Put_Line (W_In, "command ignored");
      delay 1.0;
         end if;
       Number := 0;
      end turnout_command;

   procedure sound_command(sound_type: Character) is
   begin
    	if(Number>0 and then Number<5) then
         case sound_type is

           when 'h' =>
           Sound_Manager.Horn_sound(Train_Id(Number), 2.0);
  	 when 'b' =>
      	 Sound_Manager.Bell_sound(Train_Id(Number), 2.0);
   	when others =>
       		Put_Line (W_In, "Sound command type error");
   end case;
     else
                  Put_Line (W_In, "command ignored");
      end if;

      Number := 0;  -- forget it
   end sound_command;


   procedure Dialog_Loop is
      C : Character;
   begin
      loop
         Put_Line (W_In, "Command:");
         Get_Char (W_In, C);
         case C is

           when 'q' =>
                 Train_manager.Change_Direction(1,Raildefs.Normal_Pol);
            when 't' =>
            	Train_manager.Change_Direction(2,Raildefs.Normal_Pol);
             when 'i' =>
		Train_manager.Change_Direction(3,Raildefs.Normal_Pol);

            when 'z' =>
            	Train_manager.Change_Direction(1,Raildefs.Reverse_Pol);
             when 'b' =>
		Train_manager.Change_Direction(2,Raildefs.Reverse_Pol);
            when ',' =>
            	Train_manager.Change_Direction(3,Raildefs.Reverse_Pol);

            when 'w' =>
            	Train_manager.Change_speed(1,command_def.speed_up);
            when 'y' =>
               Train_manager.Change_speed(2,command_def.speed_up);
            when 'o' =>
               Train_manager.Change_speed(3,command_def.speed_up);
            when '=' =>
               Train_manager.Change_speed(4,command_def.speed_up);

            when 'x' =>
		Train_manager.Change_speed(1,command_def.slow_down);
           when 'n' =>
            	Train_manager.Change_speed(2,command_def.slow_down);
            when '.' =>
               Train_manager.Change_speed(3,command_def.slow_down);
            when '-' =>
               Train_manager.Change_speed(4,command_def.slow_down);

            when 'a' =>
               Train_manager.Change_Path(1,command_def.Left);
                when 'g' =>
               Train_manager.Change_Path(2,command_def.Left);
                when 'k' =>
               Train_manager.Change_Path(3,command_def.Left);

               when 'd' =>
               Train_manager.Change_Path(1,command_def.Right);
                when 'j' =>
               Train_manager.Change_Path(2,command_def.Right);
                when ';' =>
               Train_manager.Change_Path(3,command_def.Right);

               when 's' =>
               Train_manager.Change_Path(1,command_def.Center);
                when 'h' =>
               Train_manager.Change_Path(2,command_def.Center);
                when 'l' =>
               Train_manager.Change_Path(3,command_def.Center);


when '0'..'9' =>
               Number := Number*10 + (Character'Pos(C) - Pos0);

when '/' =>
               track_command;

            when others =>
               null;
         end case;


         Clear (W_In);
      end loop;
   end Dialog_Loop;

begin
   Ada.Text_IO.Put_Line (" Simple use of simrail2 " & Simrail2.Version);
   Simrail2.Reset (N_Trains => 4, N_Carriages_Train_1 => 2);
   Swindows.Open (W_In, 0, 0, 7, 35, "Input");
   Swindows.Open (W_Interrupts, 8, 0, 23, 79, "Interrupts"); -- 24 lines
   Interrupt_Hdlr.Init (W_Interrupts);

   Init_Dio;

   Halls2.Initialize;
   Interrupt_Hdlr.Install; -- calls Halls2

   Dialog_Loop;

end Assignment5;
