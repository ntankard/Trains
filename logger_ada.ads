------------------------------------------------------------------------------
--  ------------------         S i m r a i l 2         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.9 2009-08-19
--                                                             V2.0 2011-02-10
--                            'Logger_Ada'
--
--                                 Spec
--
--
--  File 'logger_ada.ads'           MaRTE OS vsn by Sangorrin
--	       in slogger_sim/        Simrail2 vsn by Rob Allen (RKA),Swinburne U
--  NOTE:  This is a regular pkg spec that does NOT bind to logger.c
--  ----------------------------------------------------------------------
--  This is a Swinburne simrail2 version of slogger for testing/dev.
--  (original MaRTE copyright below)
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
------------------------------------------------------------------------------
--with MaRTE.Timespec;    -- unused in sim vsn (RKA)
with Ada.Real_Time;   -- replaces MaRTE.Timespec in sim vsn (RKA)
with Interfaces.C;
--with System;    -- unused in sim vsn (RKA)

package Logger_Ada is

   package C renames Interfaces.C;

   -----------
   -- Types --
   -----------

   type Log_Device is (Log_Console, Log_Ethernet, Log_Disk, Log_Serial);
   for Log_Device use (Log_Console => 0, Log_Ethernet => 1, Log_Disk => 2,
                       Log_Serial => 3);  -- added (RKA)

   -----------------
   -- Logger_Init --
   -----------------
   -- sets the logging device and initializes internal data
   function Logger_Init (Dev : in Log_Device) return C.Int;
   --pragma Import (C, Logger_Init, "logger_init");

   MAX_BYTES_TO_READ : constant C.Int := 1000;  -- added (RKA)
   -- The effective MAX_BYTES_TO_READ is defined in <arch>include/misc/logger.h
   -- The above copy must match!  (RKA)
   --------------------------
   -- Logger_Thread_Create -- nyi
   --------------------------
   -- Creates a new thread that will read a maximum of 'MAX_BYTES_TO_READ' each
   -- cycle from the file /dev/membuff and writes them on the logging device
   -- Note Timespec is record Tv_Sec : C.Int;  Tv_Nsec : C.Int; end record
   function Logger_Thread_Create
--      (Period : access MaRTE.Timespec.Timespec) return C.Int;
      (Period : access Ada.Real_Time.Time_Span) return C.Int;
   --pragma Import (C, Logger_Thread_Create, "logger_thread_create");

   ------------------------
   -- Logger_Manual_Call -- nyi
   ------------------------
   -- Instead of having a periodic thread (through the previous operation)
   -- collecting data, we could prefer to do it manually. That is what this
   -- operation is intended for. When called it reads a maximum of
   -- 'MAX_BYTES_TO_READ' from the file /dev/membuff and writes them on the
   -- logging device.  Returns number of bytes read (and written).
   function Logger_Manual_Call return C.Int;
   --pragma Import (C, Logger_Manual_Call, "logger_manual_call");

   ------------------------
   -- Logger_Direct_Call --
   ------------------------
   -- Send the buffer contents directly to the log device without passing
   -- through the membuffer.
   function Logger_Direct_Call (Logger_Buffer : String;  --was access System.Address;
                                Nbytes        : in C.Int) return C.Int;
   --pragma Import (C, Logger_Direct_Call, "logger_direct_call");

   ------------------------
   -- Logger_Close       --  (Swinburne)
   ------------------------
   -- Close the ultimate log device (if using membuff leave membuff open)
   procedure Logger_Close;
   --pragma Import (C, Logger_Close, "logger_close");

end Logger_Ada;
