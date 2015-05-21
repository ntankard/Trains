with Raildefs;       use Raildefs;
with Unsigned_Types; use Unsigned_Types;
with Dio192defs;     use Dio192defs;
with IO_Ports;
with Ada.Text_IO; use  Ada.Text_IO;

package body Turnout_Driver is

   type Turnout_Drive_Array is
     array (Turnout_Drive_Range) of Turnout_Drive_Register;

   type Turnout_State is (Fixed, Pending_Busy, Busy);
   type Turnout_State_Array is array (Turnout_Idx) of Turnout_State;

   protected Turnout_Registers is

      procedure Init;
      procedure Set_Control (Tn : Turnout_Id; Pos : Turnout_Drive_Bit);
      function Get_Control (Tn : Turnout_Id) return Turnout_Drive_Bit;
      function Is_Busy (Tn : in Turnout_Id) return Boolean;
      procedure Update_All;

   private

      procedure Update_Status (Tn : Turnout_Id);
      Virtual_Registers : Turnout_Drive_Array :=
        (others => Turnout_Drive_Init);
      Busy_State        : Turnout_State_Array := (others => Fixed);

   end Turnout_Registers;

   ----------------------------------------------------------------------------

   procedure Init is
   begin -- Init
      Turnout_Registers.Init;
   end Init;

   ----------------------------------------------------------------------------

   procedure Pull (Tn : in Turnout_Id; Dir : in Turnout_Pos) is

      Ndx   : Turnout_Idx := (Tn - 1) / 8;
      Bit   : Turnout_Idx := (Tn - 1) mod 8;
      Drive : Turnout_Drive_Bit;

   begin -- Pull

      case Dir is
         when Middle =>
            return;
         when Straight =>
            Drive := Straighten;
         when Turned =>
            Drive := Turn;
      end case;
      Turnout_Registers.Set_Control (Tn, Drive);

   end Pull;

   ----------------------------------------------------------------------------

   procedure Poll
     (Tn    : in Turnout_Id;
      State : out Turnout_Status_Bit;
      Dir   : out Turnout_Pos)
   is

      Setting : Turnout_Drive_Bit;

   begin -- Poll

      Setting := Turnout_Registers.Get_Control (Tn);
      if Setting = Pull_St then
         Dir := Straight;
      else
         Dir := Turned;
      end if;

      if Turnout_Registers.Is_Busy (Tn) then
         State := Busy;
      else
         State := In_Position;
      end if;

   end Poll;

   ----------------------------------------------------------------------------

   procedure Check_All is
   begin
      Turnout_Registers.Update_All;
   end Check_All;

   ----------------------------------------------------------------------------

   protected body Turnout_Registers is
      procedure Init is
      begin -- Init
         -- init 24-bits mixed input & output
         IO_Ports.Write_IO_Port (Pctl3_Addr, Pctl3_Init1);
         IO_Ports.Write_IO_Port (Qctl3_Addr, Qctl3_Init1);

         for I in Virtual_Registers'Range loop
            Virtual_Registers (I) := Turnout_Drive_Init;
            IO_Ports.Write_IO_Port
              (Turnout_Drive_Addr (I),
               Unsigned (Virtual_Registers (I)));
         end loop;

         IO_Ports.Write_IO_Port (Pctl3_Addr, Pctl3_Init2);
         IO_Ports.Write_IO_Port (Qctl3_Addr, Qctl3_Init2);
      end Init;

      -------------------------------------------------------------------------

      procedure Set_Control (Tn : in Turnout_Id; Pos : Turnout_Drive_Bit) is
         Ndx : Turnout_Idx := (Tn - 1) / 8;
         Bit : Turnout_Idx := (Tn - 1) mod 8;
      begin -- Set_Control
         Virtual_Registers (Ndx) (Bit)  := Pos;
         IO_Ports.Write_IO_Port
           (Turnout_Drive_Addr (Ndx),
            Unsigned (Virtual_Registers (Ndx)));
      end Set_Control;

      -------------------------------------------------------------------------

      function Get_Control (Tn : in Turnout_Id) return Turnout_Drive_Bit is
         Ndx : Turnout_Idx := (Tn - 1) / 8;
         Bit : Turnout_Idx := (Tn - 1) mod 8;
      begin -- Get_Control
         return Virtual_Registers (Ndx) (Bit);
      end Get_Control;

      -------------------------------------------------------------------------

      function Is_Busy (Tn : in Turnout_Id) return Boolean is
      begin -- Is_Busy
         --Update_Status(Tn);
         if Busy_State (Tn) = Fixed then
            return False;
         else
            return True;
         end if;
      end Is_Busy;

      -------------------------------------------------------------------------

      procedure Update_All is
      begin -- Update_All
         for I in Turnout_Id'Range loop
            Turnout_Registers.Update_Status (I);
         end loop;
      end Update_All;

      -------------------------------------------------------------------------

      procedure Update_Status (Tn : Turnout_Id) is
         Ndx   : Turnout_Idx := (Tn - 1) / 8;
         Bit   : Turnout_Idx := (Tn - 1) mod 8;
         Value : Unsigned_8;
         State : Turnout_Status_Register;
      begin -- Update_Status
         IO_Ports.Read_IO_Port (Turnout_State_Addr (Ndx), Value);

         if Value /= 0 then
            State := Unsigned_8_To_Turnout_Status_Register (Value);
            case Busy_State (Tn) is
               when Fixed =>
                  null;
               when Pending_Busy =>
                  if State (Bit) = Busy then
                     Busy_State (Tn) := Busy;
                  end if;
               when Busy =>
                  if State (Bit) /= Busy then
                     Busy_State (Tn) := Fixed;
                  end if;
            end case;
         end if;
      end Update_Status;
   end Turnout_Registers;

end Turnout_Driver;
