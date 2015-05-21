with Dio192defs;
with Io_Ports;
with Unsigned_Types; use Unsigned_Types;
package body turnout_Driver is


    Turnout_drive_Array :
     array (Dio192defs.Turnout_Drive_Range) of Dio192defs.Turnout_Drive_Register;


   procedure Init is
      use Dio192defs, raildefs;

   begin
      Io_Ports.Write_Io_Port(Dio192defs.Pctl3_Addr, Dio192defs.Pctl3_Init1);
      Io_Ports.Write_Io_Port(Dio192defs.Qctl3_Addr, Dio192defs.Qctl3_Init1);
      for I in Turnout_drive_Array'range loop
         Turnout_drive_Array(I) := Turnout_Drive_Init;
         Io_Ports.Write_Io_Port(Turnout_Drive_Addr(I),
                                Unsigned(Turnout_drive_Array(I)));
      end loop;
   end Init;

   procedure Pull (Tn  : in Raildefs.Turnout_Id;
                   Dir : in Raildefs.Turnout_Pos  -- ignore if Middle
                  ) is
      use Raildefs, Dio192defs;
      Ndx   : Raildefs.Turnout_Idx :=  ( Tn - 1)/ 8;
      Bit   : Raildefs.Turnout_Idx := (Tn - 1) mod 8;
      Drive : Dio192defs.Turnout_Drive_Bit;
   begin
      case Dir is
         when Middle =>
            return;
         when Straight =>
            Drive := Straighten;
         when Turned =>
            Drive := Turn;
      end case;
      if Turnout_drive_Array(Ndx)(Bit) /= Drive then
         Turnout_drive_Array(Ndx)(Bit) := Drive;
         Io_Ports.Write_Io_Port(Turnout_Drive_Addr(Ndx),  Unsigned(Turnout_drive_Array(Ndx)) );

      end if;

   end Pull;


     procedure Poll (Tn  : in Raildefs.Turnout_Id;
                     State : out Dio192defs.Turnout_Status_Bit;
                       Dir: out Raildefs.Turnout_Pos
                  ) is
      use Raildefs, Dio192defs;
      Ndx   : Raildefs.Turnout_Idx :=  ( Tn - 1)/ 8;
      Bit   : Raildefs.Turnout_Idx := (Tn - 1) mod 8;
      registr: Unsigned_8;
   begin
      Io_ports.Read_IO_Port(Turnout_State_Addr(Ndx),  registr);
      State:= Unsigned_8_To_Turnout_Status_Register(registr)(Bit);


         case Turnout_drive_Array(Ndx)(Bit) is
            when Straighten =>
               Dir:=Straight;
            when Turn =>
               Dir:= Turned;
         end case;



      end Poll;

end turnout_Driver;
