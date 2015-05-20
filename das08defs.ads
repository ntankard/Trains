with Unchecked_Conversion; -- used for type conversions
with Unsigned_Types, Raildefs;

package Das08defs is
   use Unsigned_Types;

   --Register I/O --
   Base_Address2 : constant := 16#310#;

   AD_lo_Addr : constant := Base_Address2 +0;
   AD_hi_Addr : constant := Base_Address2 +1;
   CS_Addr    : constant := Base_Address2 +2;
   PA_Addr    : constant := Base_Address2 +3;


   One_Byte : constant := 8;
   One_Bit : constant := 1;
   type Sound_bit is mod 2;

   Sound_on: constant := 1; --not used in Sound_mag2, just in auto turnoff version
   Sound_off: constant := 0;--not used in Sound_mag2, just in auto turnoff version

   type Train_Sound is record
      Horn:Sound_bit :=0;
      Bell:Sound_bit :=0;
   end record;

   for Train_Sound'Size use 2;
   for Train_sound use record
      Horn at 0 range 0..0;
      Bell at 0 range 1..1;
   end record;

   type Sound_Register is array ( Raildefs.Train_Id range 1..4) of Train_Sound;
   for Sound_Register'Component_Size use 2;
   for Sound_Register'Size use One_Byte;

   function Unsigned is new Unchecked_Conversion
      (   Source => Sound_Register,
      Target => Unsigned_8);

   function Unsigned_8_To_Sound_Register is new Unchecked_Conversion
      (   Source => Unsigned_8,
      Target => Sound_Register);

end Das08defs;
