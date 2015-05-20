with Io_Ports;
with Unsigned_Types;
with Das08defs;
with Ada.Calendar;
with Ada.Text_IO;
package body Sound_Manager_auto is
   use Unsigned_Types;


   --AS_reg declaration
    protected As_reg is
      procedure On_Reg(train: in Raildefs.Train_Id; Horn: in Boolean);
      procedure Off_Reg(train: in Raildefs.Train_Id; Horn: in Boolean);
     function Get_Reg return Unsigned_8;
   private
      Reg : Das08defs.Sound_Register;
   end As_reg;

   protected body As_reg is
      procedure On_Reg(train: in Raildefs.Train_Id; Horn: in Boolean) is
      use das08defs;
      begin
         if(Horn=true) then
           Reg(train).Horn := Sound_on;
      else
         Reg(train).Bell := Sound_on;
         end if;
      end On_Reg;

      procedure Off_Reg(train: in Raildefs.Train_Id; Horn: in Boolean) is
       use das08defs;
      begin
        if(Horn=true) then
           Reg(train).Horn := Sound_off;
      else
         Reg(train).Bell := Sound_off;
         end if;
      end Off_Reg;

      function Get_Reg return Unsigned_8 is
      	output: Unsigned_8:= 2#00000000#;
      begin
         for I in reverse Raildefs.Train_Id range 1..4 loop
                       output := output*2 + Unsigned_8(reg(I).Bell);

            output := output*2 + Unsigned_8(reg(I).Horn);
         end loop;

         	return output;
         end Get_Reg;
   end As_reg;


   --make a list of noices still running
   type Active_record is record
      Stop_time: Ada.Calendar.Time;
      On: Boolean;
   end record;

   type Active_noice_array is array(Raildefs.Train_Id) of Active_record;
   Actived_horns : Active_noice_array;
   Actived_bells : Active_noice_array;

   task  Active_noice_control;



   procedure Horn_sound(T: in Raildefs.Train_Id; Period: in Duration)
   is
   use Ada.Calendar; -- needed for '+'
   begin

      --turn on horn
      Ada.Text_IO.Put("here");
      Ada.Text_IO.Put(As_reg.Get_Reg'img);
       Ada.Text_IO.Put(" > ");
      As_reg.On_Reg(T,True);
      Ada.Text_IO.Put_Line(As_reg.Get_Reg'img);
      Io_Ports.Write_Io_Port (Das08defs.PA_Addr,As_reg.Get_Reg);

      --set up for Task to turn off noice
      Actived_horns(T).Stop_time := Ada.Calendar.Clock + Period;
      Actived_horns(T).On:=True;

      end Horn_sound;


   procedure Bell_sound(T: in Raildefs.Train_Id; Period: in Duration)
   is
        use Ada.Calendar; -- needed for '+'
   begin
      --turn on bell
      As_reg.On_Reg(T,False);
      Io_Ports.Write_Io_Port (Das08defs.PA_Addr,As_reg.Get_Reg);

      --set up for Task to turn off noice
      Actived_bells(T).Stop_time := Ada.Calendar.Clock + Period;
         Actived_bells(T).On:=True;

   end Bell_sound;


   task  body  Active_noice_control is
        use Ada.Calendar; -- needed for '<'
      begin
      loop

         for H in Actived_horns'Range loop
            if(Actived_horns(H).On = True and then Actived_horns(H).Stop_time<Ada.Calendar.Clock) then
               --turn off horn
               Ada.Text_IO.Put(As_reg.Get_Reg'img);
       Ada.Text_IO.Put(" > ");
     As_reg.Off_Reg(H,True);
      Ada.Text_IO.Put_Line(As_reg.Get_Reg'img);

               Io_Ports.Write_Io_Port (Das08defs.PA_Addr,As_reg.Get_Reg);
               Actived_horns(H).On := False;
            end if;
         end loop;

          for B in Actived_bells'Range loop
            if(Actived_bells(B).On = True and then Actived_bells(B).Stop_time<Ada.Calendar.Clock) then
               --turn off bell
      		As_reg.Off_Reg(B,False);
               Io_Ports.Write_Io_Port (Das08defs.PA_Addr,As_reg.Get_Reg);
               Actived_bells(B).On := False;
            end if;
         end loop;

      end loop;
end Active_noice_control;




end Sound_Manager_auto;
