with Raildefs;       use Raildefs;
with Unsigned_Types; use Unsigned_Types;
with Dio192defs;     use Dio192defs;
with IO_Ports;

package body Block_Driver is

   type Block_Reg_Array is array (Block_Idx range 0 .. 11) of Block_Register;
   Block_Regs : Block_Reg_Array;

   ----------------------------------------------------------------------------

   procedure Init is
   begin -- Init
      -- init 24-bits output 4 times
      IO_Ports.Write_IO_Port (Pctl1_Addr, Output_Init1);
      IO_Ports.Write_IO_Port (Qctl1_Addr, Output_Init1);
      IO_Ports.Write_IO_Port (Pctl2_Addr, Output_Init1);
      IO_Ports.Write_IO_Port (Qctl2_Addr, Output_Init1);

      for I in Block_Regs'Range loop
         Block_Regs (I) := Zero_Normal;
         IO_Ports.Write_IO_Port (Block_Addr (I), Unsigned (Block_Regs (I)));
      end loop;

      -- finish initialisation, tristate on
      IO_Ports.Write_IO_Port (Pctl1_Addr, Output_Init2);
      IO_Ports.Write_IO_Port (Qctl1_Addr, Output_Init2);
      IO_Ports.Write_IO_Port (Pctl2_Addr, Output_Init2);
      IO_Ports.Write_IO_Port (Qctl2_Addr, Output_Init2);

   end Init;

   ----------------------------------------------------------------------------

   procedure Set_Cab (B : in Raildefs.Block_Id; Cab : in Raildefs.Cab_Type) is

      Index  : Block_Idx := (B - 1) / 2; -- 0..11
      Nibble : Block_Idx := B mod 2;   -- note asymmetry for big-end first

   begin -- Set_Cab

      Block_Regs (Index) (Nibble).Blk_Cab  := Cab;
      IO_Ports.Write_IO_Port
        (Block_Addr (Index),
         Unsigned (Block_Regs (Index)));

   end Set_Cab;

   ----------------------------------------------------------------------------

   procedure Set_Polarity (B : in Block_Id; Pol : in Polarity_Type) is

      Index  : Block_Idx := (B - 1) / 2; -- 0..11
      Nibble : Block_Idx := B mod 2;   -- note asymmetry for big-end

   begin -- Set_Polarity

      Block_Regs (Index) (Nibble).Blk_Pol  := Pol;
      IO_Ports.Write_IO_Port
        (Block_Addr (Index),
         Unsigned (Block_Regs (Index)));

   end Set_Polarity;

   ----------------------------------------------------------------------------

   procedure Flip_Polarity (B : in Block_Id) is

      Index  : Block_Idx := (B - 1) / 2; -- 0..11
      Nibble : Block_Idx := B mod 2;   -- note asymmetry for big-end

   begin -- Flip_Polarity

      Block_Regs (Index) (Nibble).Blk_Pol  :=
        Opposite (Block_Regs (Index) (Nibble).Blk_Pol);
      IO_Ports.Write_IO_Port
        (Block_Addr (Index),
         Unsigned (Block_Regs (Index)));

   end Flip_Polarity;

end Block_Driver;
