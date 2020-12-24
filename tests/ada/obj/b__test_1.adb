pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test_1.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test_1.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E077 : Short_Integer; pragma Import (Ada, E077, "system__os_lib_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "ada__exceptions_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exception_table_E");
   E042 : Short_Integer; pragma Import (Ada, E042, "ada__containers_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "ada__io_exceptions_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "ada__strings_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "ada__strings__maps_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__strings__maps__constants_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "interfaces__c_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "system__exceptions_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "system__object_reader_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "system__dwarf_lines_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__soft_links__initialize_E");
   E041 : Short_Integer; pragma Import (Ada, E041, "system__traceback__symbolic_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "ada__tags_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "ada__streams_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "system__file_control_block_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "system__finalization_root_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "ada__finalization_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "system__file_io_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__calendar_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar__delays_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "ada__text_io_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E109 := E109 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "ada__text_io__finalize_spec");
      begin
         if E109 = 0 then
            F1;
         end if;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "system__file_io__finalize_body");
      begin
         E115 := E115 - 1;
         if E115 = 0 then
            F2;
         end if;
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
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

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
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

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
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      if E010 = 0 then
         Ada.Exceptions'Elab_Spec;
      end if;
      if E015 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E027 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E027 := E027 + 1;
      if E042 = 0 then
         Ada.Containers'Elab_Spec;
      end if;
      E042 := E042 + 1;
      if E072 = 0 then
         Ada.Io_Exceptions'Elab_Spec;
      end if;
      E072 := E072 + 1;
      if E057 = 0 then
         Ada.Strings'Elab_Spec;
      end if;
      E057 := E057 + 1;
      if E059 = 0 then
         Ada.Strings.Maps'Elab_Spec;
      end if;
      E059 := E059 + 1;
      if E063 = 0 then
         Ada.Strings.Maps.Constants'Elab_Spec;
      end if;
      E063 := E063 + 1;
      if E047 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      E047 := E047 + 1;
      if E029 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E029 := E029 + 1;
      if E083 = 0 then
         System.Object_Reader'Elab_Spec;
      end if;
      E083 := E083 + 1;
      if E052 = 0 then
         System.Dwarf_Lines'Elab_Spec;
      end if;
      E052 := E052 + 1;
      if E077 = 0 then
         System.Os_Lib'Elab_Body;
      end if;
      E077 := E077 + 1;
      if E023 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E023 := E023 + 1;
      E015 := E015 + 1;
      if E041 = 0 then
         System.Traceback.Symbolic'Elab_Body;
      end if;
      E041 := E041 + 1;
      E010 := E010 + 1;
      if E103 = 0 then
         Ada.Tags'Elab_Spec;
      end if;
      if E103 = 0 then
         Ada.Tags'Elab_Body;
      end if;
      E103 := E103 + 1;
      if E111 = 0 then
         Ada.Streams'Elab_Spec;
      end if;
      E111 := E111 + 1;
      if E119 = 0 then
         System.File_Control_Block'Elab_Spec;
      end if;
      E119 := E119 + 1;
      if E118 = 0 then
         System.Finalization_Root'Elab_Spec;
      end if;
      E118 := E118 + 1;
      if E116 = 0 then
         Ada.Finalization'Elab_Spec;
      end if;
      E116 := E116 + 1;
      if E115 = 0 then
         System.File_Io'Elab_Body;
      end if;
      E115 := E115 + 1;
      if E008 = 0 then
         Ada.Calendar'Elab_Spec;
      end if;
      if E008 = 0 then
         Ada.Calendar'Elab_Body;
      end if;
      E008 := E008 + 1;
      if E006 = 0 then
         Ada.Calendar.Delays'Elab_Body;
      end if;
      E006 := E006 + 1;
      if E109 = 0 then
         Ada.Text_Io'Elab_Spec;
      end if;
      if E109 = 0 then
         Ada.Text_Io'Elab_Body;
      end if;
      E109 := E109 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test_1");

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
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/jose/Development/Malef/tests/ada/obj/test_1.o
   --   -L/home/jose/Development/Malef/tests/ada/obj/
   --   -L/home/jose/Development/Malef/tests/ada/obj/
   --   -L/home/jose/Development/Malef/alire/build/lib-linux/
   --   -L/ada/lib/gcc/x86_64-pc-linux-gnu/9.3.1/adalib/
   --   -shared
   --   -lgnat-2020
   --   -ldl
--  END Object file/option list   

end ada_main;
