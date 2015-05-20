with Interfaces;
--with Basic_Integer_Types;  -- MaRTE_OS
package Unsigned_Types is
   type Unsigned_8  is new Interfaces.Unsigned_8;
   type Unsigned_16 is new Interfaces.Unsigned_16;
--   type Unsigned_8  is range 0..255;
--   for Unsigned_8'size use 8;
--   type Unsigned_16 is range 0..65535;
--   for Unsigned_16'size use 16;
-- function "and" etc
end Unsigned_Types;