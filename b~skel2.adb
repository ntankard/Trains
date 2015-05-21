pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~skel2.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~skel2.adb");

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "system__fat_flt_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "system__fat_llf_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "ada__io_exceptions_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "ada__numerics_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__tags_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "ada__streams_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "interfaces__c_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "interfaces__c__strings_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "system__exceptions_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "system__finalization_root_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "ada__finalization_E");
   E098 : Short_Integer; pragma Import (Ada, E098, "system__storage_pools_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "system__finalization_masters_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "system__storage_pools__subpools_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "system__task_info_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__calendar_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar__delays_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "system__pool_global_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "system__file_control_block_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "system__file_io_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "system__random_seed_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__secondary_stack_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "system__os_lib_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "system__tasking__initialization_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "system__tasking__protected_objects_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "ada__real_time_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "ada__text_io_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "system__tasking__protected_objects__entries_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "system__tasking__queuing_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "system__tasking__stages_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "adagraph_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "logger_ada_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "swindows_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "interrupt_hdlr_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "io_ports_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "dio192defs_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "halls2_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "simrail2_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "simtrack2_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "simtrack2__display_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "slogger_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "swindows__finalize_body");
      begin
         E248 := E248 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "simrail2__finalize_body");
      begin
         E133 := E133 - 1;
         F2;
      end;
      E222 := E222 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F3;
      end;
      E060 := E060 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "ada__text_io__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__file_io__finalize_body");
      begin
         E075 := E075 - 1;
         F5;
      end;
      E090 := E090 - 1;
      E104 := E104 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__file_control_block__finalize_spec");
      begin
         E088 := E088 - 1;
         F6;
      end;
      E100 := E100 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__pool_global__finalize_spec");
      begin
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__storage_pools__subpools__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__finalization_masters__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");
   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Zero_Cost_Exceptions : Integer;
      pragma Import (C, Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (True, True, True, True, False, False, False, True, 
           False, True, True, True, True, False, False, True, 
           False, False, True, True, False, True, True, True, 
           True, True, True, False, True, True, False, True, 
           False, False, False, False, True, True, False, True, 
           False, True, True, False, True, False, False, False, 
           False, False, False, True, True, True, True, True, 
           False, False, True, False, False, True, False, True, 
           False, False, True, True, True, False, True, True, 
           True, True, True, False, True, False),
         Count => (2, 3, 2, 2, 0, 3, 0),
         Unknown => (False, False, False, False, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Zero_Cost_Exceptions := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Fat_Flt'Elab_Spec;
      E152 := E152 + 1;
      System.Fat_Llf'Elab_Spec;
      E140 := E140 + 1;
      System.Exception_Table'Elab_Body;
      E025 := E025 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E080 := E080 + 1;
      Ada.Numerics'Elab_Spec;
      E153 := E153 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E061 := E061 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E031 := E031 + 1;
      System.Finalization_Root'Elab_Spec;
      E079 := E079 + 1;
      Ada.Finalization'Elab_Spec;
      E077 := E077 + 1;
      System.Storage_Pools'Elab_Spec;
      E098 := E098 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Task_Info'Elab_Spec;
      E177 := E177 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E008 := E008 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E006 := E006 + 1;
      System.Pool_Global'Elab_Spec;
      E100 := E100 + 1;
      System.File_Control_Block'Elab_Spec;
      E088 := E088 + 1;
      System.Random_Seed'Elab_Body;
      E159 := E159 + 1;
      E104 := E104 + 1;
      System.Finalization_Masters'Elab_Body;
      E090 := E090 + 1;
      E082 := E082 + 1;
      E050 := E050 + 1;
      Ada.Tags'Elab_Body;
      E063 := E063 + 1;
      System.Soft_Links'Elab_Body;
      E015 := E015 + 1;
      System.Secondary_Stack'Elab_Body;
      E019 := E019 + 1;
      System.Os_Lib'Elab_Body;
      E085 := E085 + 1;
      System.File_Io'Elab_Body;
      E075 := E075 + 1;
      System.Tasking.Initialization'Elab_Body;
      E226 := E226 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E206 := E206 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E161 := E161 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E060 := E060 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E222 := E222 + 1;
      System.Tasking.Queuing'Elab_Body;
      E232 := E232 + 1;
      System.Tasking.Stages'Elab_Body;
      E240 := E240 + 1;
      Adagraph'Elab_Spec;
      Adagraph'Elab_Body;
      E204 := E204 + 1;
      E246 := E246 + 1;
      Swindows'Elab_Spec;
      Dio192defs'Elab_Spec;
      E128 := E128 + 1;
      Simrail2'Elab_Spec;
      E130 := E130 + 1;
      Simtrack2'Elab_Spec;
      E195 := E195 + 1;
      Simtrack2.Display'Elab_Body;
      E202 := E202 + 1;
      Simrail2'Elab_Body;
      E133 := E133 + 1;
      Swindows'Elab_Body;
      E248 := E248 + 1;
      Slogger'Elab_Spec;
      Slogger'Elab_Body;
      E244 := E244 + 1;
      E250 := E250 + 1;
      Interrupt_Hdlr'Elab_Body;
      E242 := E242 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_skel2");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\Adagraph.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\logger_ada.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\Unsigned_Types.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\raildefs.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\dda06defs.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\dio192defs.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\int32defs.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\simdefs2.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\halls2.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\simtrack2.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\simtrack2-display.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\simrail2.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\Swindows.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\slogger.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\Io_ports.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\interrupt_hdlr.o
   --   E:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\skel2.o
   --   -LE:\Dropbox\sem 9\real time\lab 3\Simrail2_src_230\
   --   -LC:/Apps/GNAT/lib/gcc/i686-pc-mingw32/4.5.4/adalib/
   --   -ladagraph
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -Xlinker
   --   --stack=0x200000,0x1000
   --   -mthreads
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
