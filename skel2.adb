-- skeleton test program 2.3 for trains
--
-- requires Adagraph, Simrail2 (vsn 2.3.0 +), Raildefs (3.0), Dio192defs,
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
--
-- Copyright: Dr R. K Allen, Faculty of SET, Swinburne UT
-- (For use within the unit HIT3047/SWE30001 Real-Time Programming only.)
--
with Ada.Text_IO, Ada.Integer_Text_IO;
with Simrail2;
with Io_Ports;
with Swindows;                                 use Swindows;
with Unsigned_Types;                           use Unsigned_Types;  -- 1.8
with Raildefs;                                 use Raildefs;
with Dio192defs, Dda06defs, Int32defs, Halls2;
with Interrupt_Hdlr;  -- 2.1
with Slogger;  -- 2.2

procedure Skel2 is
   --package Iio is new Ada.Text_Io.Integer_Io(Integer);
   package Iio renames Ada.Integer_Text_IO;

   W_In, W_Info, W_Interrupts : Swindows.Window;

   -- vars and code for dio192: -------
   --
   type Block_Reg_Array is
     array (Raildefs.Block_Idx range 0 .. 11) of Dio192defs.Block_Register;
   Block_Regs : Block_Reg_Array;

   --   type Turnout_Drive_Array is ... Tn_Drives : ...
   --
   -- NB the above arrays must persist between calls of procs below
   --  (should be inside Ada sep package, maybe protected)

   procedure Init_Dio is
      -- todo: split into two procedures in two packages
      -- for blocks and turnouts separately
      use Dio192defs;
   begin
      -- init 24-bits output 4 times
      Io_Ports.Write_Io_Port (Pctl1_Addr, Output_Init1);
      Io_Ports.Write_Io_Port (Qctl1_Addr, Output_Init1);
      Io_Ports.Write_Io_Port (Pctl2_Addr, Output_Init1);
      Io_Ports.Write_Io_Port (Qctl2_Addr, Output_Init1);
      -- init 24-bits mixed input & output
      --      Io_Ports.Write_Io_Port(Pctl3_Addr, Pctl3_Init1);
      --      Io_Ports.Write_Io_Port(Qctl3_Addr, Qctl3_Init1);

      for I in Block_Regs'range loop
         Block_Regs (I) := Zero_Normal;
         Io_Ports.Write_Io_Port (Block_Addr (I), Unsigned (Block_Regs (I)));
      end loop;

      --      for I in Tn_Drives'range loop
      --         Tn_Drives(I) := Turnout_Drive_Init;
      --         Io_Ports.Write_Io_Port(Turnout_Drive_Addr(I),
      --Unsigned(Tn_Drives(I)));
      --      end loop;

      -- finish initialisation, tristate on
      Io_Ports.Write_Io_Port (Pctl1_Addr, Output_Init2);
      Io_Ports.Write_Io_Port (Qctl1_Addr, Output_Init2);
      Io_Ports.Write_Io_Port (Pctl2_Addr, Output_Init2);
      Io_Ports.Write_Io_Port (Qctl2_Addr, Output_Init2);
      -- init 24-bits mixed input & output you write

   end Init_Dio;

   procedure Pull
     (Tn  : in Raildefs.Turnout_Id;
      Dir : in Raildefs.Turnout_Pos  -- ignore if Middle
        ) is
      --
      use Dio192defs;
      Ndx   : Raildefs.Turnout_Idx := (Tn - 1) / 8;
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
      --      Tn_Drives(Ndx)(Bit) := Drive;
      --      Io_Ports.Write_Io_Port(Turnout_Drive_Addr(Ndx),
      --      Unsigned(Tn_Drives(Ndx)) );
   end Pull;

   procedure Set_Cab (B : in Raildefs.Block_Id; Cab : in Raildefs.Cab_Type) is
      use Raildefs, Dio192defs;
      Index  : Raildefs.Block_Idx := (B - 1) / 2; -- 0..11
      Nibble : Raildefs.Block_Idx := B mod 2;   -- note asymmetry for big-end
                                                -- first
   begin
      null;  -- you write
   end Set_Cab;

   procedure Set_Polarity
     (B   : in Raildefs.Block_Id;
      Pol : in Raildefs.Polarity_Type) is
      use Raildefs, Dio192defs;
   begin
      null;  -- you write
   end Set_Polarity;

   procedure Set_Voltage (D : in Raildefs.Dac_Id; Value : in Unsigned_8) is
      use Raildefs, Dda06defs;
      hi, lo: Unsigned_8;

   begin

      hi:= (Value/32)+8;

      lo:= (Value*16);


      Io_ports.Write_Io_port(Base_Address+unsigned_16((D-1)*2),lo);
      Io_ports.Write_Io_port(Base_Address+unsigned_16((D-1)*2+1),hi);

   end Set_Voltage;

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
            V := Unsigned_8((Character'pos (C) - Pos0)*27);  -- or something
            Set_Voltage (Dac, V);
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

   procedure Dialog_Loop is
      C : Character;
   begin
      loop
         Put_Line (W_In, "Command:");
         Get_Char (W_In, C);
         case C is
            when '0'..'9' =>
               Number := Number*10 + (Character'Pos(C) - Pos0);

            when 'D' =>
               Slogger.Finalize;   -- dump goes to a disk file
               Ada.Text_IO.Put_Line ("simlogger_out.txt closed");
               Number := 0;  -- forget it

            when 'S' =>
               Simrail2.Dump;  -- to console
               Number := 0;  -- forget it

            when 'd' =>
               Dac_Command;

            when others =>
               null;
         end case;
         Clear (W_In);
      end loop;
   end Dialog_Loop;

begin
   Ada.Text_IO.Put_Line (" Simple use of simrail2 " & Simrail2.Version);
   Simrail2.Reset (N_Trains => 3, N_Carriages_Train_1 => 2);
   Swindows.Open (W_In, 0, 0, 7, 35, "Input");
   Swindows.Open (W_Interrupts, 8, 0, 23, 79, "Interrupts"); -- 24 lines
   Interrupt_Hdlr.Init (W_Interrupts);

   Init_Dio;

   Halls2.Initialize;
   Interrupt_Hdlr.Install; -- calls Halls2

   Dialog_Loop;

end Skel2;
