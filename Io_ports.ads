--  io_ports.ads    for train simulation v1.8   Win32  April 2004
--  This package was originally written by Jerry van Dijk for DOS
--  but has been ported to Win32 GNAT3.13 for use with simrail and
--  to MaRTE_OS/GNAT3.14 Linux for use with hardware.  In practice
--  the spec is the same but the body varies.
--  Last modified: 26-Apr-04 eliminate references to Interfaces in
--  client code as this in incompatible with Basic_Integer_Types in
--  MaRTE_OS v1.4.  Rob Allen, Swinburne University of Technology
-----------------------------------------------------------------------
--
--  File:        io_ports.ads
--  Description: package for reading x86 I/O ports
--  Rev:         0.1
--  Date:        19-nov-1997
--  Author:      Jerry van Dijk
--  Mail:        jdijk@acm.org
--
--  Copyright (c) Jerry van Dijk, 1997
--  Billie Holidaystraat 28
--  2324 LK  LEIDEN
--  THE NETHERLANDS
--  tel int + 31 71 531 43 65
--
--  Permission granted to use for any purpose, provided this copyright
--  remains attached and unmodified.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--
-----------------------------------------------------------------------

with Unsigned_Types;  -- Win32 and DOS and MaRTE_OS
use Unsigned_Types;
package IO_Ports is

   -- Interrupt control
   procedure Enable_Interrupts;
   procedure Disable_Interrupts;

   -- Writing IO ports
   procedure Write_IO_Port (Address : in Unsigned_16;
                            Value   : in Unsigned_8);

   -- Reading IO ports
   procedure Read_IO_Port (Address : in     Unsigned_16;
                           Value   :    out Unsigned_8);

-- private

--   pragma Inline (Read_IO_Port);
--   pragma Inline (Write_IO_Port);
--   pragma Inline (Enable_Interrupts);
--   pragma Inline (Disable_Interrupts);

end IO_Ports;