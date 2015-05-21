-- Package Body of Io_Ports for Simrail vsn 2.0+
-- Only the 8-bit read/writes are implemented -- they call Simrail2.
-- The handhelds are not yet implemented. The interrupt controls do nothing.
--
-- Author: Rob Allen, Swinburne University of Technology
-- Current Version: 2.0
-- Revised (renamed Simdio48, Simpc14) 03-Apr-01 Rob Allen
-- Version 1.6.7 24-Jun-02 recognize (but ignore) Dda06d refs, Interrupt_Control
-- Version 1.8 27-Apr-04 use own Unsigned types requires Simrail v1.8
-- version 2.0.0 8-Jul-2007 initial version for Simrail2
-- version 2.1 25-Feb-2008: new package names
-- version 2.2 30-Mar-2011: recognize (but ignore) DAS08, digital parts of DDA06
-- version 2.2s 15-Apr-2011: add Slogger
-- version 2.3   4-Feb-2013: check Slogger.On
--
with Ada.Text_Io;
use Ada.Text_Io;

with Simrail2;
with Dio192defs;
with Int32defs;
with Dda06defs;
with Slogger;

package body Io_Ports is

   -----------------------
   -- DDA06 digital; DAS08 card --
   -----------------------
   Dda06d_Base : constant Unsigned_16 := Dda06Defs.Base_Address + 12;
   Dda06d_Ctrl : constant Unsigned_16 := Dda06d_Base + 3;
   Das08_Base : constant Unsigned_16 := 16#310#;

   -----------------------
   -- INTERRUPT CONTROL --
   -----------------------
   Interrupt_Control : constant Unsigned_16:= 16#20#;
   Interrupt_Controlp1 : constant Unsigned_16:= 16#21#;

   procedure Enable_Interrupts is
   begin
      null;
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      null;
   end Disable_Interrupts;


   --------------------------------------------------------------------------
   --                 READING IO PORTS
   --------------------------------------------------------------------------

   procedure Read_Io_Port (Address : in Unsigned_16;
         Value   : out Unsigned_8) is
   begin
      case Address is
         when Dio192defs.Pa3_Addr | Dio192defs.Pc3_Addr | Dio192defs.Qb3_Addr -- turnouts
              | Int32defs.Pa1_Addr | Int32defs.Pb1_Addr  --sensors
              | Int32defs.Qa1_Addr | Int32defs.Qb1_Addr
              | Int32defs.Pa2_Addr | Int32defs.Pb2_Addr
              | Int32defs.Qa2_Addr | Int32defs.Qb2_Addr =>
              Value := Simrail2.Read_Reg(Address);
         when Dio192defs.Pa1_Addr..Dio192defs.Qctl2_Addr
              | Dio192defs.Pb3_Addr | Dio192defs.Qa3_Addr | Dio192defs.Qc3_Addr
              | Dio192defs.Pctl3_Addr | Dio192defs.Qctl3_Addr
              | Dda06d_Ctrl  =>
              --| Int32defs.Enable_Chg_State_Int
            Put_Line("Warning: reading output register" & Address'img);
            Value := 0;
         when Dda06d_Base..Dda06d_Base+2 =>
            Value := 2#00011111#;
         when Das08_Base+1 =>
            Value := 128;
         when Das08_Base | Das08_Base+2 | Das08_Base+3 =>
            Value := 0;
         when Interrupt_Control | Interrupt_Controlp1 =>
            Value := 0;
         when others =>
            Put_Line("Error invalid read request: unknown register" & Address'img);
            raise Program_Error;
      end case;
   end Read_Io_Port;

--   procedure Read_IO_Port (Address : in     Unsigned_16;
--                           Value   :    out Unsigned_16) is
--   begin
--      Put_Line("16-bit read_io_port not implemented in simulation");
--      Value := 0;
--      raise Program_Error;
--   end Read_IO_Port;

--   procedure Read_IO_Port (Address : in     Unsigned_16;
--                           Value   :    out Unsigned_32) is
--   begin
--      Put_Line("32-bit read_io_port not implemented in simulation");
--      Value := 0;
--      raise Program_Error;
--   end Read_IO_Port;

   ---------------------------------------------------------------------------------
   --                                WRITING IO PORTS
   ---------------------------------------------------------------------------------

   procedure Write_Io_Port (Address : in Unsigned_16;
         Value   : in Unsigned_8) is
   begin
      case Address is
         when Dio192defs.Pa1_Addr..Dio192defs.Qctl2_Addr
              | Dio192defs.Pb3_Addr | Dio192defs.Qa3_Addr | Dio192defs.Qc3_Addr
              | Dio192defs.Pctl3_Addr | Dio192defs.Qctl3_Addr
              | Dda06defs.Base_Address..Dda06defs.Base_Address+7
              | Das08_Base+3  =>  -- sound control
            if Slogger.On then
               Slogger.Send_Event ('W', Integer (Address), Value);
            end if;
            Simrail2.Write_Reg(Address, Value);
         -- Some registers are configured for input so shouldn't be writing
         -- to them -- ignore with warning (possibly raise exception?)
         when Dio192defs.Pa3_Addr | Dio192defs.Pc3_Addr | Dio192defs.Qb3_Addr -- turnouts
              | Int32defs.Pa1_Addr | Int32defs.Pb1_Addr  --sensors
              | Int32defs.Qa1_Addr | Int32defs.Qb1_Addr
              | Int32defs.Pa2_Addr | Int32defs.Pb2_Addr
              | Int32defs.Qa2_Addr | Int32defs.Qb2_Addr
              | Dda06d_Base .. Dda06d_Base+2  -- handhelds
              | Das08_Base  =>
            Put_Line("Warning: ignoring write to input register" & Address'img);
--         when xxx =>
--            Put_Line("Warning: ignoring write to unused register" & Address'img);
         when Interrupt_Control | Interrupt_Controlp1
              | Dda06d_Ctrl
              | Das08_Base+1 .. Das08_Base+2 =>
            null;
         when others =>
            Put_Line("Error invalid write request: unknown register" & Address'Img);
            --raise Program_Error;
      end case;

   end Write_Io_Port;

--   procedure Write_IO_Port (Address : in Unsigned_16;
--                            Value   : in Unsigned_16) is
--   begin
--      Put_Line("16-bit read_io_port not implemented in simulation");
--      raise Program_Error;
--   end Write_IO_Port;

--   procedure Write_IO_Port (Address : in Unsigned_16;
--                            Value   : in Unsigned_32) is
--   begin
--      Put_Line("32-bit read_io_port not implemented in simulation");
--      raise Program_Error;
--   end Write_IO_Port;


end Io_Ports;
