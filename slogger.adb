------------------------------------------------------------------------------
--  ------------------         S i m r a i l 2         -------------------  --
------------------------------------------------------------------------------
--                                                             V1.3 2011-02-10
--                                                            V1.5s 2013-01-24
--                      'S l o g g e r'
--
--                                 Body
--
--
--  Was file 'kernel/marte-spy.adb' prev 'tasks_inspector.adb'         By MAR.
--                                       Rob Allen (RKA), Swinburne University
--
--  Operations to send user trace events to membuff circular buffer.
--  Later these can be written to console or serial port using
--    Logger_Ada.Logger_Manual_Call or periodically using Logger_Thread_Create
--  see misc/logger_ada.ads and misc/logger.c.
--  ----------------------------------------------------------------------
--  This is a Swinburne simrail2 version of slogger for testing/development.
--  (see spec for original MaRTE copyright)
-------------------------------------------------------------------------------
--with MaRTE.Integer_Types; use MaRTE.Integer_Types;
--with MaRTE.Debug_Messages;
--with MaRTE.Configuration_Parameters;
--with MaRTE.Kernel.Debug;
--with MaRTE.Direct_IO;
with Unsigned_Types;  -- Swinburne railroad project
--with MaRTE.HAL;  -- for Timestamp and to check if interrupts enabled
--with MaRTE.Kernel; -- implicit in .Debug
with Logger_Ada;  -- for types and init
with Interfaces.C;
use Interfaces.C;
with Ada.Real_Time;
with Ada.Text_Io;
with Ada.Integer_Text_Io;

package body Slogger is

   package DIO renames Ada.Text_Io;  --MaRTE.Direct_IO;
   package LIO is new Ada.Text_Io.Integer_Io(Long_Integer);
   package IIO renames Ada.Integer_Text_Io;

   subtype HWTime is Long_Integer;
   HWClock_Frequency : constant Integer := 1_000_000;

   T0 : Ada.Real_Time.Time;

   function Get_HWTime return HWTime is
      use Ada.Real_Time;
      Now : Duration := To_Duration(Clock - T0);
   begin
      return HWTime(HWClock_Frequency*Now);
   end Get_HWTime;

   BUFF_LENGTH : constant := 50;

   ----------------------------------
   -- Replacements for 'slogger_c.c' --
   ----------------------------------
   procedure Slogger_Init
     (Log_Device : Logger_Ada.Log_Device;
      Use_Buffer : Boolean) is
   --pragma Import (C, Slogger_Init, "slogger_init");
   begin
      if Use_Buffer /= False then
         Ada.Text_Io.Put_Line("**** slogger_init error: membuffer not supported");
         raise Program_Error;
      end if;
      if Logger_Ada.Logger_Init(Log_Device) /= 0 then
         Ada.Text_Io.Put_Line("**** slogger_init error: logger_ada failed");
         raise Program_Error;
      end if;
      T0 := Ada.Real_Time.Clock;
   end Slogger_init;


   function Width_For(V : Int) return Natural is
      W : Natural := 0;
      X : Int := V;
   begin
      if V <= 0 then
         X := -V;
         W := 1;
      end if;
      while X /= 0 loop
         W := W + 1;
         X := X / 10;
      end loop;
      return W;
   end Width_For;

   function Width_For(Timestamp : HWTime) return Natural is
      W : Natural := 0;
   begin
      if Timestamp < 1_000_000 then
         W := Width_For(Int(Timestamp));
      elsif Timestamp < 10_000_000 then
         W := 7;
      elsif Timestamp < 100_000_000 then
         W := 8;
      elsif Timestamp < 1_000_000_000 then
         W := 9;
      else
         W := 10;  -- for tick = 1us limit will be 9999 secs = 3 hours
            -- ok for simrail2 testing
      end if;
      return W;
   end Width_For;

   procedure Slogger_Send_Event_One_Object (
                                        Timestamp : in HWTime;
                                        C_Op1     : in Character;
                                        Id1       : in Int;
                                        Val1     : in Unsigned_Types.Unsigned_8) is
   --pragma Import (C, Slogger_Send_Event_One_Object,
   --               "slogger_send_event_one_object");
      S : String(1..BUFF_LENGTH);
      Next : Natural := 1;
      W : Natural;
   begin
      -- snprintf(buff, BUFF_LENGTH, "$%llu,%c,%llu,\n", timestamp, c_op1, t);
      S(1) := '$';
      if Timestamp = 0 then
         S(2) := '0';
         W := 1;
      else
         W := Width_For(Timestamp);
         LIO.Put(S(2..W+1), Timestamp);
      end if;
      Next := W + 2;
      S(Next) := ',';
      S(Next+1) := C_Op1;
      S(Next+2) := ',';
      Next := Next + 3;
      W := Width_For(Id1);
      IIO.Put(S(Next..Next+W-1), Integer(Id1));
      S(Next+W) := ',';
      Next := Next + W + 1;
      W := Width_For(Int(Val1));
      IIO.Put(S(Next..Next+W-1), Integer(Val1));
      S(Next+W) := ',';
      --Ada.Text_IO.Put_Line(Output, S(1..Next+W));
      if Logger_Ada.Logger_Direct_Call(S, Int(Next+W)) < 0 then
         DIO.Put_Line("**** Slogger.sseoo: log file not open - logging now disabled");
         On := False;
      end if;
   end Slogger_Send_Event_One_Object;

   procedure Slogger_Send_Event_With_Array (
      Timestamp : in HWTime;
      C_Op1     : in Character;
      Id1       : in Int;
      Values    : in Raildefs.Four_Registers) is
      --  Val0, Val1, Val2, Val3, Val4: in Int ) is
   --pragma Import (C, Slogger_Send_Event_With_Array,
   --               "slogger_send_event_with_array");
      S : String(1..BUFF_LENGTH);
      Next : Natural := 1;
      W : Natural;
      V : Int;
   begin
      -- snprintf(buff, BUFF_LENGTH, "$%llu,%c,%1d,%1d,%1d,%1d,%1d,%1d,\n",
      -- timestamp, c_op1, id, v0, v1, v2, v3, v4);
      S(1) := '$';
      W := Width_For(Timestamp);
      LIO.Put(S(2..W+1), Timestamp);
      Next := W + 2;
      S(Next) := ',';
      S(Next+1) := C_Op1;
      S(Next+2) := ',';
      Next := Next + 3;
      W := Width_For(Id1);
      IIO.Put(S(Next..Next+W-1), Integer(Id1));
      S(Next+W) := ',';
      Next := Next + W + 1;
      for I in Values'range loop
         V := Int(Values(I));
         W := Width_For(V);
         IIO.Put(S(Next..Next+W-1), Integer(V));
         S(Next+W) := ',';
         Next := Next + W + 1;
      end loop;
      --Ada.Text_IO.Put_Line(Output, S(1..Next-1));
      if Logger_Ada.Logger_Direct_Call(S, Int(Next-1)) < 0 then
         DIO.Put_Line("**** Slogger.ssewa log file not open - logging now disabled");
         On := False;
      end if;
   end Slogger_Send_Event_With_Array;

   -----------------
   -- State vars --
   -----------------

   Initialized : Boolean := False;
   Using_Membuffer : Boolean := False;

   --------------------------
   -- Initialize (Default for Swinburne 486) --
   --------------------------
   procedure Initialize is  -- v1.3 2011
   -- Initializes using platform defaults.  This is automatically called during
   -- package instantiation.
   begin
      Initialize( Logger_Ada.Log_Disk, False);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize(
                        Device     : in Logger_Ada.Log_Device;
                        Use_Buffer : in Boolean ) is

      --Interrupts_Enabled : Boolean := False;
      --Flags : Integer;
      use type Int;
   begin
      if Initialized then
         Logger_Ada.Logger_Close;
      end if;

      DIO.Put ("Initializing slogger...");

      --  Initialise the tracer mechanism

      Slogger_Init (Device, Use_Buffer);

      --  Set state vars

      Initialized := True;
      Using_Membuffer:= Use_Buffer;

      --  Send Initial data
      --     0,F,Clock_freq,0,

      Slogger_Send_Event_One_Object
        (0, 'F',
         Int (HWClock_Frequency), 0);

      --     init_timestamp,Z,0,0,

      Slogger_Send_Event_One_Object
        (Get_HWTime,   -- was Hal.Get_HWTime prev Hal.Get_HWTime_Slow (RKA)
         'Z', 0, 0);

      DIO.Put (" OK"); DIO.New_Line;

   end Initialize;

   ----------------
   --    Flush   --
   ----------------
   procedure Flush is  -- v1.3

   -- flushes the buffer, if relevant, and ensures events reach the output device
      Count : Interfaces.C.Int;

   begin
      if Using_Membuffer then  -- in fact irrel for simrail version
         loop
		Count := Logger_Ada.Logger_Manual_Call;
            if Count < 0 then
               raise Slogger_Error;
            end if;
		exit when Count < Logger_Ada.MAX_BYTES_TO_READ;
         end loop;
      end if;
      --Logger_Ada.Logger_Flush;  -- nyi needs a version for simrail2
   end Flush;

   ----------------
   --  Finalize  --
   ----------------
   procedure Finalize is  -- v1.3

   -- empties buffer, if relevant, and closes output device
   begin
      Flush;
      Logger_Ada.Logger_Close;
   end Finalize;

   ----------------
   -- Send_Event --
   ----------------
   procedure Send_Event (
                         Operation : in Character;  -- see read_log.adb for codes
                         Id        : in Integer;
                         Val       : in Unsigned_Types.Unsigned_8) is
   begin
      if On then
         Slogger_Send_Event_One_Object
           (Get_HWTime,
            Operation, Int(Id), (Val));
      end if;
   end Send_Event;

   ----------------
   -- Send_Event -- 'I' or possibly 'i'
   ----------------
   procedure Send_Event (
                         Operation : in Character;
                         Id        : in Integer;
                         Values    : in Raildefs.Four_Registers) is
   begin
      if On then
         Slogger_Send_Event_With_Array
            (Get_HWTime,
            Operation,
            Int(Id),
            Values );
      end if;
   end Send_Event;


begin  -- v1.3 2011
   Initialize;
end Slogger;
