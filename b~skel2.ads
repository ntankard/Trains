pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2012 (20120509)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_skel2" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#3861bb3b#;
   pragma Export (C, u00001, "skel2B");
   u00002 : constant Version_32 := 16#3935bd10#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#63cfd057#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#45724809#;
   pragma Export (C, u00005, "ada__calendar__delaysB");
   u00006 : constant Version_32 := 16#474dd4b1#;
   pragma Export (C, u00006, "ada__calendar__delaysS");
   u00007 : constant Version_32 := 16#8ba0787e#;
   pragma Export (C, u00007, "ada__calendarB");
   u00008 : constant Version_32 := 16#e791e294#;
   pragma Export (C, u00008, "ada__calendarS");
   u00009 : constant Version_32 := 16#1ee4165a#;
   pragma Export (C, u00009, "ada__exceptionsB");
   u00010 : constant Version_32 := 16#ad007709#;
   pragma Export (C, u00010, "ada__exceptionsS");
   u00011 : constant Version_32 := 16#16173147#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerB");
   u00012 : constant Version_32 := 16#e3a511ca#;
   pragma Export (C, u00012, "ada__exceptions__last_chance_handlerS");
   u00013 : constant Version_32 := 16#6daf90c4#;
   pragma Export (C, u00013, "systemS");
   u00014 : constant Version_32 := 16#0071025c#;
   pragma Export (C, u00014, "system__soft_linksB");
   u00015 : constant Version_32 := 16#fc13008d#;
   pragma Export (C, u00015, "system__soft_linksS");
   u00016 : constant Version_32 := 16#27940d94#;
   pragma Export (C, u00016, "system__parametersB");
   u00017 : constant Version_32 := 16#db4d9c04#;
   pragma Export (C, u00017, "system__parametersS");
   u00018 : constant Version_32 := 16#17775d6d#;
   pragma Export (C, u00018, "system__secondary_stackB");
   u00019 : constant Version_32 := 16#79c1b76a#;
   pragma Export (C, u00019, "system__secondary_stackS");
   u00020 : constant Version_32 := 16#ace32e1e#;
   pragma Export (C, u00020, "system__storage_elementsB");
   u00021 : constant Version_32 := 16#9762ed5c#;
   pragma Export (C, u00021, "system__storage_elementsS");
   u00022 : constant Version_32 := 16#4f750b3b#;
   pragma Export (C, u00022, "system__stack_checkingB");
   u00023 : constant Version_32 := 16#ce0d2ce8#;
   pragma Export (C, u00023, "system__stack_checkingS");
   u00024 : constant Version_32 := 16#7b9f0bae#;
   pragma Export (C, u00024, "system__exception_tableB");
   u00025 : constant Version_32 := 16#fcc14c61#;
   pragma Export (C, u00025, "system__exception_tableS");
   u00026 : constant Version_32 := 16#84debe5c#;
   pragma Export (C, u00026, "system__htableB");
   u00027 : constant Version_32 := 16#ee07deca#;
   pragma Export (C, u00027, "system__htableS");
   u00028 : constant Version_32 := 16#8b7dad61#;
   pragma Export (C, u00028, "system__string_hashB");
   u00029 : constant Version_32 := 16#4b334850#;
   pragma Export (C, u00029, "system__string_hashS");
   u00030 : constant Version_32 := 16#aad75561#;
   pragma Export (C, u00030, "system__exceptionsB");
   u00031 : constant Version_32 := 16#61515873#;
   pragma Export (C, u00031, "system__exceptionsS");
   u00032 : constant Version_32 := 16#010db1dc#;
   pragma Export (C, u00032, "system__exceptions_debugB");
   u00033 : constant Version_32 := 16#55dfb510#;
   pragma Export (C, u00033, "system__exceptions_debugS");
   u00034 : constant Version_32 := 16#b012ff50#;
   pragma Export (C, u00034, "system__img_intB");
   u00035 : constant Version_32 := 16#6f747006#;
   pragma Export (C, u00035, "system__img_intS");
   u00036 : constant Version_32 := 16#dc8e33ed#;
   pragma Export (C, u00036, "system__tracebackB");
   u00037 : constant Version_32 := 16#0c2844b1#;
   pragma Export (C, u00037, "system__tracebackS");
   u00038 : constant Version_32 := 16#907d882f#;
   pragma Export (C, u00038, "system__wch_conB");
   u00039 : constant Version_32 := 16#d244bef9#;
   pragma Export (C, u00039, "system__wch_conS");
   u00040 : constant Version_32 := 16#22fed88a#;
   pragma Export (C, u00040, "system__wch_stwB");
   u00041 : constant Version_32 := 16#ff5592f8#;
   pragma Export (C, u00041, "system__wch_stwS");
   u00042 : constant Version_32 := 16#b8a9e30d#;
   pragma Export (C, u00042, "system__wch_cnvB");
   u00043 : constant Version_32 := 16#ccba382f#;
   pragma Export (C, u00043, "system__wch_cnvS");
   u00044 : constant Version_32 := 16#129923ea#;
   pragma Export (C, u00044, "interfacesS");
   u00045 : constant Version_32 := 16#75729fba#;
   pragma Export (C, u00045, "system__wch_jisB");
   u00046 : constant Version_32 := 16#98c8a33b#;
   pragma Export (C, u00046, "system__wch_jisS");
   u00047 : constant Version_32 := 16#ada34a87#;
   pragma Export (C, u00047, "system__traceback_entriesB");
   u00048 : constant Version_32 := 16#3f8e7e85#;
   pragma Export (C, u00048, "system__traceback_entriesS");
   u00049 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00049, "interfaces__cB");
   u00050 : constant Version_32 := 16#f05a3eb1#;
   pragma Export (C, u00050, "interfaces__cS");
   u00051 : constant Version_32 := 16#3fcdd715#;
   pragma Export (C, u00051, "system__os_primitivesB");
   u00052 : constant Version_32 := 16#dd7e1ced#;
   pragma Export (C, u00052, "system__os_primitivesS");
   u00053 : constant Version_32 := 16#3ead0efd#;
   pragma Export (C, u00053, "system__win32S");
   u00054 : constant Version_32 := 16#aa4baafd#;
   pragma Export (C, u00054, "system__win32__extS");
   u00055 : constant Version_32 := 16#ee80728a#;
   pragma Export (C, u00055, "system__tracesB");
   u00056 : constant Version_32 := 16#9fb2f86e#;
   pragma Export (C, u00056, "system__tracesS");
   u00057 : constant Version_32 := 16#f64b89a4#;
   pragma Export (C, u00057, "ada__integer_text_ioB");
   u00058 : constant Version_32 := 16#f1daf268#;
   pragma Export (C, u00058, "ada__integer_text_ioS");
   u00059 : constant Version_32 := 16#bc0fac87#;
   pragma Export (C, u00059, "ada__text_ioB");
   u00060 : constant Version_32 := 16#36d750a9#;
   pragma Export (C, u00060, "ada__text_ioS");
   u00061 : constant Version_32 := 16#1358602f#;
   pragma Export (C, u00061, "ada__streamsS");
   u00062 : constant Version_32 := 16#5331c1d4#;
   pragma Export (C, u00062, "ada__tagsB");
   u00063 : constant Version_32 := 16#c49b6a94#;
   pragma Export (C, u00063, "ada__tagsS");
   u00064 : constant Version_32 := 16#074eccb2#;
   pragma Export (C, u00064, "system__unsigned_typesS");
   u00065 : constant Version_32 := 16#e6965fe6#;
   pragma Export (C, u00065, "system__val_unsB");
   u00066 : constant Version_32 := 16#17e62189#;
   pragma Export (C, u00066, "system__val_unsS");
   u00067 : constant Version_32 := 16#46a1f7a9#;
   pragma Export (C, u00067, "system__val_utilB");
   u00068 : constant Version_32 := 16#660205db#;
   pragma Export (C, u00068, "system__val_utilS");
   u00069 : constant Version_32 := 16#b7fa72e7#;
   pragma Export (C, u00069, "system__case_utilB");
   u00070 : constant Version_32 := 16#c0b3f04c#;
   pragma Export (C, u00070, "system__case_utilS");
   u00071 : constant Version_32 := 16#7a48d8b1#;
   pragma Export (C, u00071, "interfaces__c_streamsB");
   u00072 : constant Version_32 := 16#a539be81#;
   pragma Export (C, u00072, "interfaces__c_streamsS");
   u00073 : constant Version_32 := 16#773a2d5d#;
   pragma Export (C, u00073, "system__crtlS");
   u00074 : constant Version_32 := 16#4a803ccf#;
   pragma Export (C, u00074, "system__file_ioB");
   u00075 : constant Version_32 := 16#60d89729#;
   pragma Export (C, u00075, "system__file_ioS");
   u00076 : constant Version_32 := 16#8cbe6205#;
   pragma Export (C, u00076, "ada__finalizationB");
   u00077 : constant Version_32 := 16#22e22193#;
   pragma Export (C, u00077, "ada__finalizationS");
   u00078 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00078, "system__finalization_rootB");
   u00079 : constant Version_32 := 16#225de354#;
   pragma Export (C, u00079, "system__finalization_rootS");
   u00080 : constant Version_32 := 16#b46168d5#;
   pragma Export (C, u00080, "ada__io_exceptionsS");
   u00081 : constant Version_32 := 16#62120d5e#;
   pragma Export (C, u00081, "interfaces__c__stringsB");
   u00082 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00082, "interfaces__c__stringsS");
   u00083 : constant Version_32 := 16#a50435f4#;
   pragma Export (C, u00083, "system__crtl__runtimeS");
   u00084 : constant Version_32 := 16#721198aa#;
   pragma Export (C, u00084, "system__os_libB");
   u00085 : constant Version_32 := 16#a6d80a38#;
   pragma Export (C, u00085, "system__os_libS");
   u00086 : constant Version_32 := 16#4cd8aca0#;
   pragma Export (C, u00086, "system__stringsB");
   u00087 : constant Version_32 := 16#da45da00#;
   pragma Export (C, u00087, "system__stringsS");
   u00088 : constant Version_32 := 16#b2907efe#;
   pragma Export (C, u00088, "system__file_control_blockS");
   u00089 : constant Version_32 := 16#6d35da9a#;
   pragma Export (C, u00089, "system__finalization_mastersB");
   u00090 : constant Version_32 := 16#075a3ce8#;
   pragma Export (C, u00090, "system__finalization_mastersS");
   u00091 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00091, "system__address_imageB");
   u00092 : constant Version_32 := 16#cc430dfe#;
   pragma Export (C, u00092, "system__address_imageS");
   u00093 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00093, "system__img_boolB");
   u00094 : constant Version_32 := 16#9876e12f#;
   pragma Export (C, u00094, "system__img_boolS");
   u00095 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00095, "system__ioB");
   u00096 : constant Version_32 := 16#f3ed678b#;
   pragma Export (C, u00096, "system__ioS");
   u00097 : constant Version_32 := 16#a7a37cb6#;
   pragma Export (C, u00097, "system__storage_poolsB");
   u00098 : constant Version_32 := 16#be018fa9#;
   pragma Export (C, u00098, "system__storage_poolsS");
   u00099 : constant Version_32 := 16#ba5d60c7#;
   pragma Export (C, u00099, "system__pool_globalB");
   u00100 : constant Version_32 := 16#d56df0a6#;
   pragma Export (C, u00100, "system__pool_globalS");
   u00101 : constant Version_32 := 16#88cd69c1#;
   pragma Export (C, u00101, "system__memoryB");
   u00102 : constant Version_32 := 16#a7242cd1#;
   pragma Export (C, u00102, "system__memoryS");
   u00103 : constant Version_32 := 16#17551a52#;
   pragma Export (C, u00103, "system__storage_pools__subpoolsB");
   u00104 : constant Version_32 := 16#738e4bc9#;
   pragma Export (C, u00104, "system__storage_pools__subpoolsS");
   u00105 : constant Version_32 := 16#f6fdca1c#;
   pragma Export (C, u00105, "ada__text_io__integer_auxB");
   u00106 : constant Version_32 := 16#b9793d30#;
   pragma Export (C, u00106, "ada__text_io__integer_auxS");
   u00107 : constant Version_32 := 16#515dc0e3#;
   pragma Export (C, u00107, "ada__text_io__generic_auxB");
   u00108 : constant Version_32 := 16#a6c327d3#;
   pragma Export (C, u00108, "ada__text_io__generic_auxS");
   u00109 : constant Version_32 := 16#ef6c8032#;
   pragma Export (C, u00109, "system__img_biuB");
   u00110 : constant Version_32 := 16#c16c44ff#;
   pragma Export (C, u00110, "system__img_biuS");
   u00111 : constant Version_32 := 16#10618bf9#;
   pragma Export (C, u00111, "system__img_llbB");
   u00112 : constant Version_32 := 16#80ab5401#;
   pragma Export (C, u00112, "system__img_llbS");
   u00113 : constant Version_32 := 16#9777733a#;
   pragma Export (C, u00113, "system__img_lliB");
   u00114 : constant Version_32 := 16#7ce0c515#;
   pragma Export (C, u00114, "system__img_lliS");
   u00115 : constant Version_32 := 16#f931f062#;
   pragma Export (C, u00115, "system__img_llwB");
   u00116 : constant Version_32 := 16#29c77797#;
   pragma Export (C, u00116, "system__img_llwS");
   u00117 : constant Version_32 := 16#b532ff4e#;
   pragma Export (C, u00117, "system__img_wiuB");
   u00118 : constant Version_32 := 16#af2dc36d#;
   pragma Export (C, u00118, "system__img_wiuS");
   u00119 : constant Version_32 := 16#7993dbbd#;
   pragma Export (C, u00119, "system__val_intB");
   u00120 : constant Version_32 := 16#250abafb#;
   pragma Export (C, u00120, "system__val_intS");
   u00121 : constant Version_32 := 16#936e9286#;
   pragma Export (C, u00121, "system__val_lliB");
   u00122 : constant Version_32 := 16#f78b7664#;
   pragma Export (C, u00122, "system__val_lliS");
   u00123 : constant Version_32 := 16#68f8d5f8#;
   pragma Export (C, u00123, "system__val_lluB");
   u00124 : constant Version_32 := 16#7dbc9bc0#;
   pragma Export (C, u00124, "system__val_lluS");
   u00125 : constant Version_32 := 16#98de19f6#;
   pragma Export (C, u00125, "dda06defsS");
   u00126 : constant Version_32 := 16#bdc9387b#;
   pragma Export (C, u00126, "raildefsS");
   u00127 : constant Version_32 := 16#f8ada779#;
   pragma Export (C, u00127, "unsigned_typesS");
   u00128 : constant Version_32 := 16#892a9cea#;
   pragma Export (C, u00128, "dio192defsS");
   u00129 : constant Version_32 := 16#14c58806#;
   pragma Export (C, u00129, "halls2B");
   u00130 : constant Version_32 := 16#04f569b5#;
   pragma Export (C, u00130, "halls2S");
   u00131 : constant Version_32 := 16#a84be135#;
   pragma Export (C, u00131, "int32defsS");
   u00132 : constant Version_32 := 16#e6635d38#;
   pragma Export (C, u00132, "simrail2B");
   u00133 : constant Version_32 := 16#d1dadc2b#;
   pragma Export (C, u00133, "simrail2S");
   u00134 : constant Version_32 := 16#e18a47a0#;
   pragma Export (C, u00134, "ada__float_text_ioB");
   u00135 : constant Version_32 := 16#e61b3c6c#;
   pragma Export (C, u00135, "ada__float_text_ioS");
   u00136 : constant Version_32 := 16#d5f9759f#;
   pragma Export (C, u00136, "ada__text_io__float_auxB");
   u00137 : constant Version_32 := 16#f854caf5#;
   pragma Export (C, u00137, "ada__text_io__float_auxS");
   u00138 : constant Version_32 := 16#6d0081c3#;
   pragma Export (C, u00138, "system__img_realB");
   u00139 : constant Version_32 := 16#aa07c126#;
   pragma Export (C, u00139, "system__img_realS");
   u00140 : constant Version_32 := 16#b2944ef4#;
   pragma Export (C, u00140, "system__fat_llfS");
   u00141 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00141, "system__float_controlB");
   u00142 : constant Version_32 := 16#8d53d3f8#;
   pragma Export (C, u00142, "system__float_controlS");
   u00143 : constant Version_32 := 16#06417083#;
   pragma Export (C, u00143, "system__img_lluB");
   u00144 : constant Version_32 := 16#4e87cc71#;
   pragma Export (C, u00144, "system__img_lluS");
   u00145 : constant Version_32 := 16#194ccd7b#;
   pragma Export (C, u00145, "system__img_unsB");
   u00146 : constant Version_32 := 16#98baf045#;
   pragma Export (C, u00146, "system__img_unsS");
   u00147 : constant Version_32 := 16#3ddff6b3#;
   pragma Export (C, u00147, "system__powten_tableS");
   u00148 : constant Version_32 := 16#730c1f82#;
   pragma Export (C, u00148, "system__val_realB");
   u00149 : constant Version_32 := 16#9386e7d5#;
   pragma Export (C, u00149, "system__val_realS");
   u00150 : constant Version_32 := 16#0be1b996#;
   pragma Export (C, u00150, "system__exn_llfB");
   u00151 : constant Version_32 := 16#ec2b8e2b#;
   pragma Export (C, u00151, "system__exn_llfS");
   u00152 : constant Version_32 := 16#ee76e913#;
   pragma Export (C, u00152, "system__fat_fltS");
   u00153 : constant Version_32 := 16#84ad4a42#;
   pragma Export (C, u00153, "ada__numericsS");
   u00154 : constant Version_32 := 16#ac5daf3d#;
   pragma Export (C, u00154, "ada__numerics__float_randomB");
   u00155 : constant Version_32 := 16#ac27f55b#;
   pragma Export (C, u00155, "ada__numerics__float_randomS");
   u00156 : constant Version_32 := 16#5ffcea55#;
   pragma Export (C, u00156, "system__random_numbersB");
   u00157 : constant Version_32 := 16#21858c24#;
   pragma Export (C, u00157, "system__random_numbersS");
   u00158 : constant Version_32 := 16#7d397bc7#;
   pragma Export (C, u00158, "system__random_seedB");
   u00159 : constant Version_32 := 16#7e93c81d#;
   pragma Export (C, u00159, "system__random_seedS");
   u00160 : constant Version_32 := 16#2f095d0b#;
   pragma Export (C, u00160, "ada__real_timeB");
   u00161 : constant Version_32 := 16#41de19c7#;
   pragma Export (C, u00161, "ada__real_timeS");
   u00162 : constant Version_32 := 16#93d8ec4d#;
   pragma Export (C, u00162, "system__arith_64B");
   u00163 : constant Version_32 := 16#df271247#;
   pragma Export (C, u00163, "system__arith_64S");
   u00164 : constant Version_32 := 16#8f3bd8ab#;
   pragma Export (C, u00164, "system__taskingB");
   u00165 : constant Version_32 := 16#117023e3#;
   pragma Export (C, u00165, "system__taskingS");
   u00166 : constant Version_32 := 16#9f1b736c#;
   pragma Export (C, u00166, "system__task_primitivesS");
   u00167 : constant Version_32 := 16#1faa77d9#;
   pragma Export (C, u00167, "system__os_interfaceS");
   u00168 : constant Version_32 := 16#527a2bd4#;
   pragma Export (C, u00168, "system__task_primitives__operationsB");
   u00169 : constant Version_32 := 16#00837a4c#;
   pragma Export (C, u00169, "system__task_primitives__operationsS");
   u00170 : constant Version_32 := 16#6f001a54#;
   pragma Export (C, u00170, "system__exp_unsB");
   u00171 : constant Version_32 := 16#3a826f18#;
   pragma Export (C, u00171, "system__exp_unsS");
   u00172 : constant Version_32 := 16#1826115c#;
   pragma Export (C, u00172, "system__interrupt_managementB");
   u00173 : constant Version_32 := 16#92c564a4#;
   pragma Export (C, u00173, "system__interrupt_managementS");
   u00174 : constant Version_32 := 16#c313b593#;
   pragma Export (C, u00174, "system__multiprocessorsB");
   u00175 : constant Version_32 := 16#55030fb7#;
   pragma Export (C, u00175, "system__multiprocessorsS");
   u00176 : constant Version_32 := 16#5052be8c#;
   pragma Export (C, u00176, "system__task_infoB");
   u00177 : constant Version_32 := 16#ef1d87cb#;
   pragma Export (C, u00177, "system__task_infoS");
   u00178 : constant Version_32 := 16#652aa403#;
   pragma Export (C, u00178, "system__tasking__debugB");
   u00179 : constant Version_32 := 16#f32cb5c6#;
   pragma Export (C, u00179, "system__tasking__debugS");
   u00180 : constant Version_32 := 16#39591e91#;
   pragma Export (C, u00180, "system__concat_2B");
   u00181 : constant Version_32 := 16#967f6238#;
   pragma Export (C, u00181, "system__concat_2S");
   u00182 : constant Version_32 := 16#ae97ef6c#;
   pragma Export (C, u00182, "system__concat_3B");
   u00183 : constant Version_32 := 16#1b8592ae#;
   pragma Export (C, u00183, "system__concat_3S");
   u00184 : constant Version_32 := 16#c9fdc962#;
   pragma Export (C, u00184, "system__concat_6B");
   u00185 : constant Version_32 := 16#aa6565d0#;
   pragma Export (C, u00185, "system__concat_6S");
   u00186 : constant Version_32 := 16#def1dd00#;
   pragma Export (C, u00186, "system__concat_5B");
   u00187 : constant Version_32 := 16#7d965e65#;
   pragma Export (C, u00187, "system__concat_5S");
   u00188 : constant Version_32 := 16#3493e6c0#;
   pragma Export (C, u00188, "system__concat_4B");
   u00189 : constant Version_32 := 16#6ff0737a#;
   pragma Export (C, u00189, "system__concat_4S");
   u00190 : constant Version_32 := 16#1eab0e09#;
   pragma Export (C, u00190, "system__img_enum_newB");
   u00191 : constant Version_32 := 16#eaa85b34#;
   pragma Export (C, u00191, "system__img_enum_newS");
   u00192 : constant Version_32 := 16#7b8aedca#;
   pragma Export (C, u00192, "system__stack_usageB");
   u00193 : constant Version_32 := 16#a5188558#;
   pragma Export (C, u00193, "system__stack_usageS");
   u00194 : constant Version_32 := 16#6ff9d10a#;
   pragma Export (C, u00194, "simdefs2S");
   u00195 : constant Version_32 := 16#8052bb28#;
   pragma Export (C, u00195, "simtrack2S");
   u00196 : constant Version_32 := 16#03e83d1c#;
   pragma Export (C, u00196, "ada__numerics__elementary_functionsB");
   u00197 : constant Version_32 := 16#9c80fa8f#;
   pragma Export (C, u00197, "ada__numerics__elementary_functionsS");
   u00198 : constant Version_32 := 16#3e0cf54d#;
   pragma Export (C, u00198, "ada__numerics__auxB");
   u00199 : constant Version_32 := 16#9f6e24ed#;
   pragma Export (C, u00199, "ada__numerics__auxS");
   u00200 : constant Version_32 := 16#6214eb0a#;
   pragma Export (C, u00200, "system__machine_codeS");
   u00201 : constant Version_32 := 16#501a243d#;
   pragma Export (C, u00201, "simtrack2__displayB");
   u00202 : constant Version_32 := 16#da090cb6#;
   pragma Export (C, u00202, "simtrack2__displayS");
   u00203 : constant Version_32 := 16#00b1d196#;
   pragma Export (C, u00203, "adagraphB");
   u00204 : constant Version_32 := 16#0b1102d2#;
   pragma Export (C, u00204, "adagraphS");
   u00205 : constant Version_32 := 16#bb8952df#;
   pragma Export (C, u00205, "system__tasking__protected_objectsB");
   u00206 : constant Version_32 := 16#0e06b2d3#;
   pragma Export (C, u00206, "system__tasking__protected_objectsS");
   u00207 : constant Version_32 := 16#2a89d93b#;
   pragma Export (C, u00207, "system__soft_links__taskingB");
   u00208 : constant Version_32 := 16#6ac0d6d0#;
   pragma Export (C, u00208, "system__soft_links__taskingS");
   u00209 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00209, "ada__exceptions__is_null_occurrenceB");
   u00210 : constant Version_32 := 16#24d5007b#;
   pragma Export (C, u00210, "ada__exceptions__is_null_occurrenceS");
   u00211 : constant Version_32 := 16#ec38a9a5#;
   pragma Export (C, u00211, "system__concat_7B");
   u00212 : constant Version_32 := 16#71f863de#;
   pragma Export (C, u00212, "system__concat_7S");
   u00213 : constant Version_32 := 16#5b942b2e#;
   pragma Export (C, u00213, "system__concat_8B");
   u00214 : constant Version_32 := 16#87be9fe2#;
   pragma Export (C, u00214, "system__concat_8S");
   u00215 : constant Version_32 := 16#dd13bf65#;
   pragma Export (C, u00215, "system__exn_lliB");
   u00216 : constant Version_32 := 16#ec38eb6b#;
   pragma Export (C, u00216, "system__exn_lliS");
   u00217 : constant Version_32 := 16#276453b7#;
   pragma Export (C, u00217, "system__img_lldB");
   u00218 : constant Version_32 := 16#9e8d99ad#;
   pragma Export (C, u00218, "system__img_lldS");
   u00219 : constant Version_32 := 16#8da1623b#;
   pragma Export (C, u00219, "system__img_decB");
   u00220 : constant Version_32 := 16#c382991f#;
   pragma Export (C, u00220, "system__img_decS");
   u00221 : constant Version_32 := 16#ab9350d0#;
   pragma Export (C, u00221, "system__tasking__protected_objects__entriesB");
   u00222 : constant Version_32 := 16#db92b260#;
   pragma Export (C, u00222, "system__tasking__protected_objects__entriesS");
   u00223 : constant Version_32 := 16#386436bc#;
   pragma Export (C, u00223, "system__restrictionsB");
   u00224 : constant Version_32 := 16#2162204d#;
   pragma Export (C, u00224, "system__restrictionsS");
   u00225 : constant Version_32 := 16#582b91d0#;
   pragma Export (C, u00225, "system__tasking__initializationB");
   u00226 : constant Version_32 := 16#93a57cc9#;
   pragma Export (C, u00226, "system__tasking__initializationS");
   u00227 : constant Version_32 := 16#87bf522d#;
   pragma Export (C, u00227, "system__tasking__protected_objects__operationsB");
   u00228 : constant Version_32 := 16#c3da2e0f#;
   pragma Export (C, u00228, "system__tasking__protected_objects__operationsS");
   u00229 : constant Version_32 := 16#adff1e5c#;
   pragma Export (C, u00229, "system__tasking__entry_callsB");
   u00230 : constant Version_32 := 16#84b0eb9c#;
   pragma Export (C, u00230, "system__tasking__entry_callsS");
   u00231 : constant Version_32 := 16#7b8939c7#;
   pragma Export (C, u00231, "system__tasking__queuingB");
   u00232 : constant Version_32 := 16#ca5254e7#;
   pragma Export (C, u00232, "system__tasking__queuingS");
   u00233 : constant Version_32 := 16#11a73c38#;
   pragma Export (C, u00233, "system__tasking__utilitiesB");
   u00234 : constant Version_32 := 16#541e5a71#;
   pragma Export (C, u00234, "system__tasking__utilitiesS");
   u00235 : constant Version_32 := 16#bd6fc52e#;
   pragma Export (C, u00235, "system__traces__taskingB");
   u00236 : constant Version_32 := 16#52029525#;
   pragma Export (C, u00236, "system__traces__taskingS");
   u00237 : constant Version_32 := 16#195cdc00#;
   pragma Export (C, u00237, "system__tasking__rendezvousB");
   u00238 : constant Version_32 := 16#34f28e26#;
   pragma Export (C, u00238, "system__tasking__rendezvousS");
   u00239 : constant Version_32 := 16#5217a8a3#;
   pragma Export (C, u00239, "system__tasking__stagesB");
   u00240 : constant Version_32 := 16#34822145#;
   pragma Export (C, u00240, "system__tasking__stagesS");
   u00241 : constant Version_32 := 16#498b0e50#;
   pragma Export (C, u00241, "interrupt_hdlrB");
   u00242 : constant Version_32 := 16#ef4a55cc#;
   pragma Export (C, u00242, "interrupt_hdlrS");
   u00243 : constant Version_32 := 16#6b494690#;
   pragma Export (C, u00243, "sloggerB");
   u00244 : constant Version_32 := 16#6a46fca2#;
   pragma Export (C, u00244, "sloggerS");
   u00245 : constant Version_32 := 16#3c011662#;
   pragma Export (C, u00245, "logger_adaB");
   u00246 : constant Version_32 := 16#6524f756#;
   pragma Export (C, u00246, "logger_adaS");
   u00247 : constant Version_32 := 16#21e58cc3#;
   pragma Export (C, u00247, "swindowsB");
   u00248 : constant Version_32 := 16#a676c5b7#;
   pragma Export (C, u00248, "swindowsS");
   u00249 : constant Version_32 := 16#42e390f5#;
   pragma Export (C, u00249, "io_portsB");
   u00250 : constant Version_32 := 16#6e2ad8e5#;
   pragma Export (C, u00250, "io_portsS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.arith_64%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.exn_lli%s
   --  system.exn_lli%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_dec%s
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_dec%b
   --  system.img_lld%s
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.img_lld%b
   --  system.img_real%s
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.multiprocessors%s
   --  system.os_primitives%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.arith_64%b
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  system.soft_links%s
   --  system.traces%s
   --  system.traces%b
   --  system.unsigned_types%s
   --  system.exp_uns%s
   --  system.exp_uns%b
   --  system.fat_flt%s
   --  system.fat_llf%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.img_real%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.val_int%s
   --  system.val_lli%s
   --  system.val_llu%s
   --  system.val_real%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_real%b
   --  system.val_llu%b
   --  system.val_lli%b
   --  system.val_int%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.address_image%s
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.concat_7%s
   --  system.concat_7%b
   --  system.concat_8%s
   --  system.concat_8%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.numerics.aux%s
   --  ada.numerics.aux%b
   --  ada.numerics.elementary_functions%s
   --  ada.numerics.elementary_functions%b
   --  ada.tags%s
   --  ada.streams%s
   --  interfaces.c%s
   --  system.multiprocessors%b
   --  interfaces.c.strings%s
   --  system.crtl.runtime%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.win32%s
   --  system.os_interface%s
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking%b
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.traces.tasking%s
   --  system.traces.tasking%b
   --  system.win32.ext%s
   --  system.task_primitives.operations%b
   --  system.os_primitives%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.random_numbers%s
   --  ada.numerics.float_random%s
   --  ada.numerics.float_random%b
   --  system.random_seed%s
   --  system.random_seed%b
   --  system.secondary_stack%s
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  interfaces.c.strings%b
   --  interfaces.c%b
   --  ada.tags%b
   --  system.soft_links%b
   --  system.secondary_stack%b
   --  system.random_numbers%b
   --  system.address_image%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.file_io%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.tasking.entry_calls%s
   --  system.tasking.initialization%s
   --  system.tasking.initialization%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.utilities%s
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.float_aux%s
   --  ada.float_text_io%s
   --  ada.float_text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.float_aux%b
   --  ada.text_io.integer_aux%s
   --  ada.text_io.integer_aux%b
   --  ada.integer_text_io%s
   --  ada.integer_text_io%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%b
   --  system.tasking.rendezvous%s
   --  system.tasking.protected_objects.operations%s
   --  system.tasking.protected_objects.operations%b
   --  system.tasking.rendezvous%b
   --  system.tasking.entry_calls%b
   --  system.tasking.stages%s
   --  system.tasking.stages%b
   --  adagraph%s
   --  adagraph%b
   --  logger_ada%s
   --  logger_ada%b
   --  swindows%s
   --  interrupt_hdlr%s
   --  unsigned_types%s
   --  io_ports%s
   --  raildefs%s
   --  dda06defs%s
   --  dio192defs%s
   --  halls2%s
   --  int32defs%s
   --  simdefs2%s
   --  simrail2%s
   --  halls2%b
   --  simtrack2%s
   --  simtrack2.display%s
   --  simtrack2.display%b
   --  simrail2%b
   --  swindows%b
   --  slogger%s
   --  slogger%b
   --  io_ports%b
   --  interrupt_hdlr%b
   --  skel2%b
   --  END ELABORATION ORDER


end ada_main;
