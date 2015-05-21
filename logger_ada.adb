------------------------------------------------------------------------------
--  ------------------   for   S i m r a i l 2         -------------------  --
------------------------------------------------------------------------------
--                                                        sim    V1.1 2011-02-10
--                            'Logger_Ada'              comments only 2013-01-23     
--
--                    Body for simrail version
--
--
--  File 'logger_ada.adb'                   By Rob Allen (RKA),Swinburne U
--  ----------------------------------------------------------------------
--with Ada.Real_Time;
--with Interfaces.C;
--with System;
with Ada.Text_Io;
package body Logger_Ada is
   Output : Ada.Text_Io.File_Type;
   Out_Name : constant String := "simlogger_out.txt";

   -----------------
   -- Logger_Init --
   -----------------

   function Logger_Init (Dev : in Log_Device) return C.Int is
   begin
      if Dev /= Log_Disk then
         raise Program_Error;
      end if;
      Ada.Text_Io.Create(Output, name=>Out_Name, Mode=>Ada.Text_Io.Out_File);
      return 0;
   end Logger_Init;

   ------------------------
   -- Logger_Close       --  (Swinburne)
   ------------------------
   -- Close the log device, assumed disk.  This version DOES NOT immediately
   --   re-open it to get flush effect.
   procedure Logger_Close is
   begin
      --if Dev = Log_Disk then
      Ada.Text_Io.Close(Output);
      -- the next line was supposed to provide a flush effect, but doesnt work
      --Ada.Text_Io.Open(Output, name=>Out_Name, Mode=>Ada.Text_Io.Append_File);
      --end if;
   end Logger_Close;

   --------------------------
   -- Logger_Thread_Create --  (Swinburne not impl)
   --------------------------

   function Logger_Thread_Create
     (Period : access Ada.Real_Time.Time_Span)
      return C.Int
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return 0;
   end Logger_Thread_Create;

   ------------------------
   -- Logger_Manual_Call --  (Swinburne not impl)
   ------------------------

   function Logger_Manual_Call return C.Int is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return 0;
   end Logger_Manual_Call;

   ------------------------
   -- Logger_Direct_Call --
   ------------------------

   function Logger_Direct_Call
     (Logger_Buffer : String;
      Nbytes        : in C.Int)
      return C.Int
   is
   begin
      Ada.Text_Io.Put_Line(Output,
         Logger_Buffer(Logger_Buffer'first .. Logger_Buffer'first+Integer(Nbytes)-1));
      return NBytes;
   exception
      when others=>
         return C.Int(-1);
   end Logger_Direct_Call;

end Logger_Ada;
