with Raildefs;       use Raildefs;
with Unsigned_Types; use Unsigned_Types;
with Dda06defs;      use Dda06defs;
with IO_Ports;       use IO_Ports;

package body Dac_Driver is

   Three_Shift : constant := 8;
   Five_Shift  : constant := 32;
   Hi_Init     : constant := 2#00001000#;

   ----------------------------------------------------------------------------

   procedure Init is
   begin -- Init
      for I in 1 .. Max_Trains loop
         Set_Voltage (Dac_Id (I), 0);
      end loop;
   end Init;

   ----------------------------------------------------------------------------

   procedure Set_Voltage (D : in Dac_Id; Value : in Unsigned_8) is

      hi : Unsigned_8;
      lo : Unsigned_8;

   begin -- Set_Voltage

      hi := (Value / Five_Shift) or Hi_Init; -- Right shift by 5
      lo := Value * Three_Shift;   -- Left shift by 3

      Write_IO_Port (Dalo_Addr (D), lo); -- must be first
      Write_IO_Port (Dahi_Addr (D), hi);

   end Set_Voltage;

end Dac_Driver;
