-- Halls2 simrail2 version
-- version 1.0 11-Mar-08
-- version 1.1 13-May-08 Enable made public
--
with Unsigned_Types, Raildefs, Int32defs;
with Simrail2;
package body Halls2 is

   -------------
   -- Disable --
   -------------

   procedure Disable is
   begin
      null;  -- todo!
   end Disable;

   -------------
   -- Enable --
   -------------

   procedure Enable is
   begin
      null;  -- todo!
   end Enable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   --------------------------
   -- Install_Int_Handling --
   --
   -- Install interrupt analyzer : this is the simrail version of the 
   -- procedure.  (March 2008)
   --------------------------

   procedure Install_Int_Handling (Analyzer : Raildefs.Proc4_Access) is
   begin
      Simrail2.Install_Int_Handling(Analyzer);
   end Install_Int_Handling;

   ------------------
   -- State_Of --
   ------------------

   function State_Of (Sensor : in Raildefs.Sensor_Id) return Raildefs.Sensor_Bit is
      use Unsigned_Types, Raildefs, Int32defs;
      Addr : Unsigned_16;
      Value : Unsigned_8;
      Reg : Sensor_Register;
   begin
      Addr := Sensor_Addr((Sensor-1)/8);
      Value := Simrail2.Read_Reg(Addr);
      Reg := Unsigned_8_To_Sensor_Register(Value);
      return Reg((Sensor-1) mod 8);
   end State_Of;

end Halls2;

