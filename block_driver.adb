with Dio192defs;
with Io_Ports;
with Unsigned_Types;

package body Block_Driver is

   type Block_Reg_Array is
     array (Raildefs.Block_Idx range 0 .. 11) of Dio192defs.Block_Register;

   Block_Regs : Block_Reg_Array;

   procedure Set_Cab (B : in Raildefs.Block_Id; Cab : in Raildefs.Cab_Type) is

      use Raildefs, Dio192defs;
      Index  : Raildefs.Block_Idx := (B - 1) / 2; -- 0..11
      Nibble : Raildefs.Block_Idx := B mod 2;   -- note asymmetry for big-end
                                                -- first
   begin

      Block_Regs(index)(Nibble).Blk_Cab:=Cab;
      Io_Ports.Write_Io_Port (Block_Addr(Index),Unsigned ( Block_Regs(index)));



   end Set_Cab;

   procedure Set_Polarity(B   : in Raildefs.Block_Id;
      			  Pol : in Raildefs.Polarity_Type) is
   use Raildefs, Dio192defs;
   Index  : Raildefs.Block_Idx := (B - 1) / 2; -- 0..11
   Nibble : Raildefs.Block_Idx := B mod 2;   -- note asymmetry for big-end

   begin

      --A_byte(Nibble_No).Blk_Cab := Cab_type(Current_Train_Id);

      Block_Regs(index)(Nibble).Blk_Pol:=Pol;
      Io_Ports.Write_Io_Port (Block_Addr(Index), Unsigned (Block_Regs(index)));



   end Set_Polarity;

   procedure Flip_Polarity(B   : in Raildefs.Block_Id) is
   use Raildefs, Dio192defs;
   Index  : Raildefs.Block_Idx := (B - 1) / 2; -- 0..11
   Nibble : Raildefs.Block_Idx := B mod 2;   -- note asymmetry for big-end
   begin
      Block_Regs(index)(Nibble).Blk_Pol:= Opposite(Block_Regs(index)(Nibble).Blk_Pol);
      Io_Ports.Write_Io_Port (Block_Addr(Index), Unsigned (Block_Regs(index)));
        end Flip_Polarity;


   procedure Init is
     use Dio192defs;
   begin
      -- init 24-bits output 4 times
      Io_Ports.Write_Io_Port (Pctl1_Addr, Output_Init1);
      Io_Ports.Write_Io_Port (Qctl1_Addr, Output_Init1);
      Io_Ports.Write_Io_Port (Pctl2_Addr, Output_Init1);
      Io_Ports.Write_Io_Port (Qctl2_Addr, Output_Init1);


      for I in Block_Regs'range loop
         Block_Regs (I) := Zero_Normal;
         Io_Ports.Write_Io_Port (Block_Addr (I), Unsigned (Block_Regs (I)));
      end loop;

      -- finish initialisation, tristate on
      Io_Ports.Write_Io_Port (Pctl1_Addr, Output_Init2);
      Io_Ports.Write_Io_Port (Qctl1_Addr, Output_Init2);
      Io_Ports.Write_Io_Port (Pctl2_Addr, Output_Init2);
      Io_Ports.Write_Io_Port (Qctl2_Addr, Output_Init2);


   end Init;

end Block_Driver;
