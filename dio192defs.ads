-- Dio192defs
-- This Package contains the definitions of register addresses
-- and bit assignments for the first 3 48-bit sections of the CIO-192
-- card connected to the blocks and turnouts.

-- Also contains unchecked_conversion functions to convert register
-- records to Unsigned_8 which is needed when calling Write_IO_Port
-- and Read_IO_Port in Io_Ports package.

-- Author : Rob Allen
-- version 0.0    7-Jul-07 derived Simcio192 from Pc14defs,Dio48def v2.0.1
--    same type for all bytes, convenience arrays of addresses
-- version 1.0   25-Feb-08 renamed Dio192defs
-- version 1.0.1  1-Mar-08 fixed base address
-- version 1.0.2  7-Mar-08 new version of Dio192defs.Block_Register
-- version 1.0.3  9-Mar-12 fixed comment re Raildefs, compatible raildefs 2.5
-- version 1.0.4  7-Feb-13 added subtype Turnout_Drive_Range
-- version 1.0.5 25-Feb-15 comment change only (for raildefs v3.0)

--
with Unchecked_Conversion; -- used for type conversions
with Unsigned_Types;  -- defines Unsigned_8 needed for
   -- compatiblity with Marte
with Raildefs;
use Raildefs;

package Dio192defs is

   use Unsigned_Types;

   -- defined in package Raildefs:
   --subtype Block_Idx is Integer range 0..24;
   --subtype Block_Id is Block_Idx range 1..24;

   --type Sensor_Idx is range 0..64;
   --subtype Sensor_Id is Sensor_Idx range 1..64;
   --No_Sensor : constant Sensor_Idx := 0;

   --type Turnout_Idx is new Integer range 0 .. 24;
   --subtype Turnout_Id is Turnout_Idx range 1 .. 19;
   --No_Turnout : constant Turnout_Idx := 0;

   --type Cab_Type is mod 8;
   --subtype Dac_Id is Cab_Type range 1..Max_Trains;
   --subtype Pwm_Id is Cab_Type range Max_Trains+1 .. 7;

   -- type Polarity_Type is (Normal_Pol, Reverse_Pol);
   --       value 0 = normal polarity, 1 = reverse polarity

   ----- Register I/O Addresses. ---------------------------------------------

   Base_Address1 : constant := 16#220#;  -- was 208
   Pa1_Addr   : constant := Base_Address1 + 0;  -- blocks 1..2
   Pb1_Addr   : constant := Base_Address1 + 1;  -- blocks 3..4
   Pc1_Addr   : constant := Base_Address1 + 2;  -- blocks 5..6
   Pctl1_Addr : constant := Base_Address1 + 3;
   Qa1_Addr   : constant := Base_Address1 + 4;  -- blocks 7..8
   Qb1_Addr   : constant := Base_Address1 + 5;  -- blocks 9..10
   Qc1_Addr   : constant := Base_Address1 + 6;  -- blocks 11..12
   Qctl1_Addr : constant := Base_Address1 + 7;

   Base_Address2 : constant := 16#228#;
   Pa2_Addr   : constant := Base_Address2 + 0;  -- blocks 13..14
   Pb2_Addr   : constant := Base_Address2 + 1;  -- blocks 15..16
   Pc2_Addr   : constant := Base_Address2 + 2;  -- blocks 17..18
   Pctl2_Addr : constant := Base_Address2 + 3;
   Qa2_Addr   : constant := Base_Address2 + 4;  -- blocks 19..20
   Qb2_Addr   : constant := Base_Address2 + 5;  -- blocks 21..22
   Qc2_Addr   : constant := Base_Address2 + 6;  -- blocks 23..24
   Qctl2_Addr : constant := Base_Address2 + 7;

   -- convenient array of byte addresses for block control:
   -- index by (block_no - 1)/2
   Block_Addr : constant array (Block_Idx range 0..11) of Unsigned_16 :=
   (Pa1_Addr, Pb1_Addr, Pc1_Addr, Qa1_Addr, Qb1_Addr, Qc1_Addr,
    Pa2_Addr, Pb2_Addr, Pc2_Addr, Qa2_Addr, Qb2_Addr, Qc2_Addr);

   Base_Address3 : constant := 16#230#;  -- for turnouts
   Pa3_Addr   : constant := Base_Address3 + 0;  -- turnout state 9..16
   Pb3_Addr   : constant := Base_Address3 + 1;  -- turnout control 17..24
   Pc3_Addr   : constant := Base_Address3 + 2;  -- turnout state   17..24
   Pctl3_Addr : constant := Base_Address3 + 3;
   Qa3_Addr   : constant := Base_Address3 + 4;  -- turnout control 1..8
   Qb3_Addr   : constant := Base_Address3 + 5;  -- turnout state   1..8
   Qc3_Addr   : constant := Base_Address3 + 6;  -- turnout control 9..16
   Qctl3_Addr : constant := Base_Address3 + 7;

   -- convenient array of byte addresses for turnout control:
   -- index by (turnout_no - 1)/8
   subtype Turnout_Drive_Range is Turnout_Idx range 0..2;
   Turnout_Drive_Addr : constant array (Turnout_Drive_Range) of Unsigned_16 :=
   (Qa3_Addr, Qc3_Addr, Pb3_Addr);
   -- and state:
   Turnout_State_Addr : constant array (Turnout_Drive_Range) of Unsigned_16 :=
   (Qb3_Addr, Pa3_Addr, Pc3_Addr);

   -- initialisation:
   -- x 0  0  a ch 0  b cl
   -- where x = MSB = 1 to program, zeroes select mode 0 of 82C55 chip
   -- other bits 0=output, 1=input  (ch = C high nibble, cl = C low nibble)

   Top_Bit   : constant Unsigned_8 := 2#10000000#;
   Output_Init2 : constant Unsigned_8 := 2#00000000#; -- Pa,Pb,Pc output
   Output_Init1 : constant Unsigned_8 := Top_Bit + Output_Init2;

   Pctl3_Init2 : constant Unsigned_8 := 2#00011001#; -- Pb:out, Pa,Pc:in
   Pctl3_Init1 : constant Unsigned_8 := Top_Bit + Pctl3_Init2;
   Qctl3_Init2 : constant Unsigned_8 := 2#00000010#; -- Pa,Pc:out, Pb:in
   Qctl3_Init1 : constant Unsigned_8 := Top_Bit + Qctl3_Init2;

   ----- Declarations of Register Records, bit assignments -------------------
   One_Byte : constant := 8;

   Off  : constant Cab_Type := 0;
   --
   --  Cab_Type and Polarity_Type are declared in raildefs

   type Block_Nibble is -- specifies CAB and polarity for 1 block
   record
      Blk_Cab : Cab_Type;
      Blk_Pol : Polarity_Type;
   end record;
   for Block_Nibble'Size use 4;  -- bits
   for Block_Nibble use record
      Blk_Cab at 0 range 0..2;
      Blk_Pol at 0 range 3..3;
   end record;
   type Block_Register is array (Block_Idx range 0..1) of Block_Nibble;
   for Block_Register'Component_Size use 4;
   for Block_Register'Size use One_Byte;
   -- NB this places index 0 at the low end BUT the electronics has blocks
   -- in order from left to right ie high end first.  BEWARE!!
   -- If b:Block_Id   ie (1,2,3,..24)
   -- use (b mod 2) to select nibble but (b-1)/2 to select the byte within
   -- array Block_Addr (which indexes from 0).

   Zero_Normal : constant Block_Register :=
    ((Off,Normal_Pol), (Off,Normal_Pol));

   ------------------------------------------------

   -- Turnout_Drive_register used to drive turnouts
   -- Note: numbered 0..7 within the byte in the conventional order,
   -- ie from bit 0 right (LSB) to bit 7 left (MSB)
   --
   type Turnout_Drive_Bit is (Pull_St, Pull_Tu);
   Straighten : constant Turnout_Drive_Bit := Pull_St;
   Turn       : constant Turnout_Drive_Bit := Pull_Tu;

   type Turnout_Drive_Register is array(Turnout_Idx range 0..7) of Turnout_Drive_Bit;  -- v2.0
   for Turnout_Drive_Register'Component_Size use 1;
   for Turnout_Drive_Register'Size use One_Byte;
   Turnout_Drive_Init : constant Turnout_Drive_Register :=
    (others=>Pull_St);

   ------------------------------------------------

   -- Turnout_Status_register is used to read turnout positions
   -- (Each bit will change to Busy when drive starts and will not change until
   -- the turnout's slider switch reaches the other side, or the original slide
   -- if the drive is reversed.)
   -- Note: numbered 0..7 within the byte
   --
   type Turnout_Status_Bit is (Busy, In_Position);
   type Turnout_Status_Register is array(Turnout_Idx range 0..7) of Turnout_Status_Bit;  -- v2.0
   for Turnout_Status_Register'Component_Size use 1;
   for Turnout_Status_Register'Size use One_Byte;
   All_In_Position : constant Turnout_Status_Register :=
    (others=>In_Position);

   ------------------------------------------------

   -- The following functions are used for type conversions between register
   -- record types and unsigned_8 types.
   -- Unsigned_8 types are required for write_io_port and read_io_port
   -- function calls in the io_ports package.

   function Unsigned is new Unchecked_Conversion
      (   Source => Block_Register,
      Target => Unsigned_8);

   function Unsigned is new Unchecked_Conversion
      (   Source => Turnout_Drive_Register,
      Target => Unsigned_8);

   function Unsigned_8_To_Turnout_Status_Register is new Unchecked_Conversion
      (   Source => Unsigned_8,
      Target => Turnout_Status_Register);

   -- the following are for simrail2 use (not needed in student code):

   function Unsigned_8_To_Block_Register is new Unchecked_Conversion
      (   Source => Unsigned_8,
      Target => Block_Register);

   function Unsigned_8_To_Turnout_Drive_Register is new Unchecked_Conversion
      (   Source => Unsigned_8,
      Target => Turnout_Drive_Register);

   function Unsigned is new Unchecked_Conversion
      (   Source => Turnout_Status_Register,
      Target => Unsigned_8);

end Dio192defs;
