with Io_Ports;
with Unsigned_Types;
with Das08defs;
package body Sound_Manager is
   use Unsigned_Types;


  AS_Reg : Das08defs.Sound_Register;

   procedure Horn_sound(T: in Raildefs.Train_Id;
                  Period: in Duration)
   is
   use Das08defs;
   begin

      --toggle horn
     AS_Reg(T).Horn:=AS_Reg(T).Horn+1;
      Io_Ports.Write_Io_Port (Das08defs.PA_Addr,Unsigned(AS_Reg));

      end Horn_sound;


   procedure Bell_sound(T: in Raildefs.Train_Id;
                  Period: in Duration)
   is
      use Das08defs;
   begin
     --toggle bell
     AS_Reg(T).Bell:=AS_Reg(T).Bell+1;
     Io_Ports.Write_Io_Port (Das08defs.PA_Addr,Unsigned(AS_Reg));


   end Bell_sound;

end Sound_Manager;
