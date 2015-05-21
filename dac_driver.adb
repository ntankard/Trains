
with Dda06defs;
with Io_Ports;

package body Dac_Driver is

   type Dac_Reg_Array is array(Raildefs.Dac_Id) of Dda06defs.Da_Register;

   Dac_Regs : Dac_Reg_array;

   procedure Set_Voltage (D : in Raildefs.Dac_Id;
                          Value : in Unsigned_8)
   is
      use Raildefs, Dda06defs;
      --hi, lo: Unsigned_8;

   begin

      -- hi:= (Value/32)+8;
      --lo:= (Value*8);

      --Io_ports.Write_Io_port(Dalo_Addr(D),lo);
      --Io_ports.Write_Io_port(Dahi_Addr(D),hi);

      Dac_Regs(D).lo :=(Value*8); -- remove top bits
      Dac_Regs(D).hi :=(Value/32)+8; --remove bottom bits and  set constant bit

      Io_ports.Write_Io_port(Dalo_Addr(D),Dac_Regs(D).lo);
      Io_ports.Write_Io_port(Dahi_Addr(D),Dac_Regs(D).hi);


   end Set_Voltage;

   procedure Init is
      begin
      for I in 1..Raildefs.Max_Trains
        loop
         Set_voltage(Raildefs.Dac_Id(I),0);
         end loop;
        end Init;

end Dac_Driver;
