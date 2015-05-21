-- Simrail2-Blockdriver_Pkg.adb  version 2.0.0
--
-- Author: Rob Allen, Swinburne Univ Tech.
-- Version 2.0.0   30-Jul-07 (version 2) separate 
-- Version 2.0.1   11-aug-07 change order of 2-char display, eg +2, prev 2+ 
--
separate (Simrail2)
package body Blockdriver_Pkg is
   use Dio192defs;

   procedure Init (
         B   : in out Blockdriver; 
         Num : in     Block_Id     ) is 
   begin
      B.Id := Num;
   end Init;

   procedure Tell (
         B   : in out Blockdriver;     
         Cab : in     Cab_Type ) is 
   begin
      --Put_Line("sim.blockdriver.tell" & B.Id'Img & " B.Cab:" & Cab'img);
      B.Cab := Cab;
   end Tell;

   procedure Change_Polarity (
         B       : in out Blockdriver; 
         Dir     : in     Polarity_Type;     
         Changed : in out Boolean      ) is 
   begin
      --Put_Line("sim.blockdriver.Change_Polarity" & B.Id'Img & " Dir:" & Dir'img);
      Tell(B.Relay, Dir, Changed);
   end Change_Polarity;

   procedure Tick (
         B       : in out Blockdriver; 
         Changed : in out Boolean      ) is 
   begin
      Tick(B.Relay, Changed);
   end Tick;

   function Is_Open_Circuit (
         B : Blockdriver ) 
     return Boolean is 
   begin
      return Is_Open_Circuit(B.Relay);
   end Is_Open_Circuit;

   function Get_Cab (
         B : Blockdriver ) 
     return Cab_Type is 
      -- returns which DAC selected
   begin
      return B.Cab;
   end Get_Cab;

   function Get_Signed_Cab (
         B : Blockdriver ) 
     return String2 is 
      -- returns eg "-2" " 0" "+0"
      Result : String2 := "  ";  
   begin
      Result(2) := Character'Val(48 + Integer(B.Cab));
      if not Is_Open_Circuit(B.Relay) then
         if Polarity(B.Relay) > 0.0 then
            Result(1) := '+';
         else
            Result(1) := '-';
         end if;
      end if;
      return Result;
   end Get_Signed_Cab;

   function Get_Signed_Voltage (
         B : Blockdriver ) 
     return Float is 
      -- returns voltage (0 - 10 V)
   begin
      --Put_Line("simrail2-bdp.Get_Signed_Voltage(Block" & B.Id'Img & " B.Cab:" & B.Cab'img);
      if B.Cab = 0 then
         return 0.0;
      else
         return Get_Voltage(Dacs(B.Cab))*Polarity(B.Relay);
      end if;
   end Get_Signed_Voltage;

end Blockdriver_Pkg;
