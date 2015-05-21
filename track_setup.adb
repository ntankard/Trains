with turnout_driver;
with Block_Driver;
with Raildefs;  use Raildefs;


package body track_setup is
   type BlockArray is array (Positive range <>) of Block_id;
   type TurnoutArray is array (Positive range <>) of Turnout_Id;

Oval_Turns: TurnoutArray:= (
                                                   Turnout_Id(13),
                                                   Turnout_Id(14),
                                                   Turnout_Id(17),
                                                   Turnout_Id(18)
                                                      ) ;
      --list of turnouts to make sure are straight for oval

Oval_blocks: BlockArray :=( Block_Id(13),
                            Block_Id(14),
                            Block_Id(17),
                            Block_Id(18)
             		) ;
		--list of blocks for oval


Fig8_blocks:BlockArray:=(Block_Id(22),Block_Id(23));
		--list of blocks for figure 8

Common_Turns:TurnoutArray:= (
                                                    Turnout_Id(12),
                                                    Turnout_Id(15),
                                                    Turnout_Id(16),
                                                    Turnout_Id(19)
                                                         ) ;
      --list of turnouts common to both setups

Common_and_Same_Blocks: BlockArray:=(
                                                     Block_Id(12),
                                                     Block_Id(19)
                                                       ) ;
Common_but_Reversed_Blocks: BlockArray :=(
                                                    Block_Id(15),
  						    Block_Id(16)
                                                       ) ;
 --split for figure 8
Common_Blocks:  BlockArray:= Common_and_Same_Blocks & Common_but_Reversed_Blocks;

  --list of turnouts common to both setups
type circuit is (None, Fig8, oval);
currect_circuit: circuit :=None;

   procedure reset_track is
   begin
      for I in Oval_blocks'range loop
       block_driver.Set_Cab (Oval_blocks(i), Cab_Type(0));
      end loop;

     for I in Common_Blocks'range loop
       block_driver.Set_Cab (Common_Blocks(i), Cab_Type(0));
      end loop;

      for I in Fig8_blocks'range loop
       block_driver.Set_Cab (Fig8_blocks(i), Cab_Type(0));
       end loop;
      end reset_track;

   procedure oval_setup is
      begin
	--reset_track;

   --   for I in Oval_Turns'range loop
    --    turnout_driver.Pull(Oval_Turns(I),Straight);
    --  end loop;

      for I in Common_Turns'range loop
        turnout_driver.Pull(Common_Turns(I),Straight);
   end loop;

    --   for I in Common_Blocks'range loop
    --     block_driver.Set_Cab (Common_Blocks(i), Cab_Type(2));
    --      block_driver.Set_Polarity(Common_Blocks(i), Normal_Pol);
   --   end loop;

   --for I in Oval_blocks'range loop
   --   block_driver.Set_Cab (Oval_blocks(i), Cab_Type(2));
   --   block_driver.Set_Polarity(Oval_blocks(i), Normal_Pol);
  -- end loop;
 currect_circuit := oval;
   end oval_setup;

   procedure firgur8_setup is
begin
   --   reset_track;

       for I in Common_Turns'range loop
        turnout_driver.Pull(Common_Turns(I),Turned);
   end loop;

   --     for I in Common_and_Same_Blocks'range loop
    --     block_driver.Set_Cab (Common_and_Same_Blocks(i), Cab_Type(2));
   --       block_driver.Set_Polarity(Common_and_Same_Blocks(i), Normal_Pol);
   --   end loop;

   --     for I in Common_but_Reversed_Blocks'range loop
    --     block_driver.Set_Cab (Common_but_Reversed_Blocks(i), Cab_Type(2));
   --       block_driver.Set_Polarity(Common_but_Reversed_Blocks(i), Reverse_Pol);
   --   end loop;



   --   for I in Fig8_blocks'range loop
   --      block_driver.Set_Cab (Fig8_blocks(i), Cab_Type(2));
   --   end loop;

   --       block_driver.Set_Polarity(Fig8_blocks(Fig8_blocks'First), Normal_Pol);
   --       block_driver.Set_Polarity(Fig8_blocks(Fig8_blocks'Last), Reverse_Pol);
	currect_circuit:= Fig8;
end firgur8_setup;

procedure flip is
   begin

      for I in Common_Blocks'range loop
          block_driver.Flip_Polarity(Common_Blocks(i));
      end loop;

      case currect_circuit is
      when oval=>

   for I in Oval_blocks'range loop
      block_driver.Flip_Polarity(Oval_blocks(i));
   end loop;


      when Fig8 =>

          for I in Fig8_blocks'range loop
      block_driver.Flip_Polarity(Fig8_blocks(i));
   end loop;

         when none =>
            null;
            end case;
end flip;
   end track_setup;

