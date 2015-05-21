-- Int32defs v1.0
-- This Package contains the definitions of register addresses
-- and bit assignments for the two CIO-SIM32 Cards connected to
-- the Hall effect sensors.

-- Also contains unchecked_conversion functions to convert register
-- records to Unsigned_8 which is needed when calling Write_IO_Port
-- and Read_IO_Port in Io_Ports package.

-- Author : Rob Allen
-- version 0.0 7-Jul-07 derived SimInt32 from Dio48s v2.0.1
--       same type for all sensor bytes
-- version 1.0 25-Feb-08 renamed Int32defs
--
with Unchecked_Conversion; -- used for type conversions
with Unsigned_Types;  -- defines Unsigned_8 (v1.8)
with Raildefs;
use Raildefs;

package Int32defs is

   use Unsigned_Types;

   One_Byte : constant := 8;

-- in Raildefs:
--   type Sensor_Idx is range 0..64;
--   subtype Sensor_Id is Sensor_Idx range 1..64;
--   No_Sensor : constant Sensor_Idx := 0;

   IRQ1 : constant := 5;
   IRQ2 : constant := 7;
   ----- Register I/O Addresses. ---------------------------------------------

   Base_Address1 : constant := 16#250#;
   Pa1_Addr   : constant := Base_Address1 + 2;  -- sensors 1..8
   Pb1_Addr   : constant := Base_Address1 + 1;  -- sensors 9..16
   Qa1_Addr   : constant := Base_Address1 + 6;  -- sensors 17..24
   Qb1_Addr   : constant := Base_Address1 + 5;  -- sensors 25..32
   Pctl1_Addr : constant := Base_Address1 + 3;
   Qctl1_Addr : constant := Base_Address1 + 7;
   Base_Address2 : constant := 16#258#;
   Pa2_Addr   : constant := Base_Address2 + 2;  -- sensors 33..40
   Pb2_Addr   : constant := Base_Address2 + 1;  -- sensors 31..48
   Qa2_Addr   : constant := Base_Address2 + 6;  -- sensors 49..56
   Qb2_Addr   : constant := Base_Address2 + 5;  -- sensors 57..64
   Pctl2_Addr : constant := Base_Address2 + 3;
   Qctl2_Addr : constant := Base_Address2 + 7;

   -- convenient array of byte addresses for sensor input:
   -- index by (sensor_no - 1)/8
   Sensor_Addr : constant array (Sensor_Idx range 0..7) of Unsigned_16 :=
   (Pa1_Addr, Pb1_Addr, Qa1_Addr, Qb1_Addr, Pa2_Addr, Pb2_Addr, Qa2_Addr, Qb2_Addr);

   -- NB: port control (ctl) registers are command on output, status when read
   -- Status:

--   MICR1_Addr :  constant := Base_Address1 + 0;
--   MICR2_Addr :  constant := Base_Address2 + 0;
--   Clear_Chg_State_Int : constant := Base_Address + 15;

   ----- Declarations of Register Records, bit assignments -------------------

   --type Sensor_Bit is (On, Off);  -- note order F=0=on, T=1=off  (see Raildefs)

   type Sensor_Register is array(Sensor_Idx range 0..7) of Raildefs.Sensor_Bit;  -- v2.0
   for Sensor_Register'Component_Size use 1;  -- v2.0
   for Sensor_Register'Size use One_Byte;
   All_Off : constant Sensor_Register := (others=>Raildefs.Off);

   -- The following functions are used for type conversions between register
   -- record types and unsigned_8 types.
   -- Unsigned_8 types are required for write_io_port and read_io_port
   -- function calls in the io_ports package.

   function Unsigned is new Unchecked_Conversion
      (   Source => Sensor_Register,
      Target => Unsigned_8);

   function Unsigned_8_To_Sensor_Register is new Unchecked_Conversion
      (   Source => Unsigned_8,
      Target => Sensor_Register);

end Int32defs;
