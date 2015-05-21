-- Dda06defs v1.0
-- This Package contains the definitions of register addresses
-- and bit assignments for the 4 DACs of the CIO-DDA06/Jr
-- card connected to the CAB board.

-- Also contains unchecked_conversion functions to convert register
-- records to Unsigned_8 which is needed when calling Write_IO_Port
-- in Io_Ports package.

-- Author : Rob Allen
-- version 0.0 7-Jul-07 derived Simdda06 from Pc14defs,Dio48def v2.0.1
--    same type for all bytes, convenience arrays of addresses
-- version 1.0 25-Feb-08 renamed Dda06defs
-- version 1.1  3-Apr-08 changed index type for Daxx_Addr
--
with Unchecked_Conversion; -- used for type conversions
with Unsigned_Types;  -- defines Unsigned_8 needed for
   -- compatiblity with Marte
with Raildefs;  -- for Dac_Id

package Dda06defs is

   use Unsigned_Types;

   ----- Register I/O Addresses. ---------------------------------------------

   Base_Address : constant := 16#240#;
   Da0lo_Addr   : constant := Base_Address + 0;
   DA0hi_Addr   : constant := Base_Address + 1;
   Da1lo_Addr   : constant := Base_Address + 2;
   DA1hi_Addr   : constant := Base_Address + 3;
   Da2lo_Addr   : constant := Base_Address + 4;
   DA2hi_Addr   : constant := Base_Address + 5;
   Da3lo_Addr   : constant := Base_Address + 6;
   DA3hi_Addr   : constant := Base_Address + 7;

   -- convenient array of IO addresses for DACs:
   -- (v1.1) index type changed from Integer to Dac_Id
   -- (A type conversion from Train_Id to Dac_Id, previously to
   -- Integer, will still be needed.  However occurrence of Integer
   -- was unexpected and the new type conversion may be
   -- done "upstream" from code that uses these arrays.)
   --
   Dalo_Addr : constant array (Raildefs.Dac_Id) of Unsigned_16 :=
   (Da0lo_Addr, Da1lo_Addr, Da2lo_Addr, Da3lo_Addr);
   Dahi_Addr : constant array (Raildefs.Dac_Id) of Unsigned_16 :=
   (Da0hi_Addr, Da1hi_Addr, Da2hi_Addr, Da3hi_Addr);

   ----- Declarations of Register Records, bit assignments -------------------

   subtype Dalo_Register is Unsigned_8;
   subtype Dahi_Register is Unsigned_8;
   type Da_Register is
      record
         hi: Dahi_Register;
         lo: Dalo_Register;
      end record;

   -- the following function is unnecessary but here for consistency
   -- with other packages.  (This really is a "do nothing"!)
   function Unsigned is new Unchecked_Conversion
      (   Source => Dalo_Register,
      Target => Unsigned_8);

--   function Unsigned is new Unchecked_Conversion
--      (   Source => Dahi_Register,
--      Target => Unsigned_8);

   --function Dalo_register(Value: in unsigned_8) return Dalo_register;
   --function Dahi_register(Value: in unsigned_8) return Dahi_register;

end Dda06defs;
