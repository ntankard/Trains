------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                            V1.5s 2013-01-23
--
--                      'S l o g g e r'
--
--                                 Spec
--
--
--  Was file 'kernel/marte-spy.ads' prev 'tasks_inspector.ads'         By MAR.
--                                       Rob Allen (RKA), Swinburne University
--
--  Operations to send user trace events to membuff circular buffer.
--  Later these can be written to console or serial port using
 -- Logger_Ada.Logger_Manual_Call or periodically using Logger_Thread_Create
--  see misc/logger_ada.ads and misc/logger.c.
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
-------------------------------------------------------------------------------
with Logger_Ada;
with Unsigned_Types;
with Raildefs;

package Slogger is         -- Swinburne railroad project

   On : Boolean := True; -- v1.5.  Not used internally.
      -- Used by HW versions of: Halls2.adb v2.6 on and Io_Ports v1.4 on.
      -- If you want to speed things up set to False **** AT RUN TIME ****
      -- (so that any log will contain program initialisation actions)
      --
      -- For your own logging write, eg
      --     if Slogger.On then
      --         Slogger.Send_Event('E',
      --          Integer(My.Id)*100 + 1, Unsigned_Types.Unsigned_8(Ev.Block));
      --     end if;
      -- See read_log.adb for event codes and formats.

   Slogger_Error : exception;  -- v1.3 2011
   
   procedure Initialize;  -- v1.3 2011
   -- Initializes using platform defaults.  This is automatically called during
   -- package instantiation.
   pragma Inline (Initialize);

   procedure Initialize(
                        Device     : in Logger_Ada.Log_Device;
                        Use_Buffer : in Boolean );
   --  (Re-)initializes the ultimate output device and, if Use_Buffer, the membuff
   --  and sends the initial timestamp data.
   pragma Inline (Initialize);

   procedure Send_Event (
                         Operation : in Character;  -- see read_log.adb for codes
                         Id        : in Integer;
                         Val       : in Unsigned_Types.Unsigned_8);
   -- note Timestamp : Marte.HAL.HWTime is prefixed automatically
   pragma Inline (Send_Event);


   procedure Send_Event (  -- v1.1 2010
                         Operation : in Character;  -- I or (v1.5s)i
                         Id        : in Integer;
                         Values    : in Raildefs.Four_Registers);
   -- note Timestamp : Ada.Real_Time.Time_Span in microseconds is prefixed automatically
   pragma Inline (Send_Event);

   procedure Flush;  -- v1.3 2011
   -- flushes the buffer, if relevant, and ensures events reach the output device

   procedure Finalize;  -- v1.3 2011
   -- empties buffer, if relevant, and closes output device

end Slogger;

