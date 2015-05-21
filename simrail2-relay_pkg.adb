-- Simrail2-Relay_Pkg.adb  version 2.0.0
--
-- Author: Rob Allen, Swinburne Univ Tech.
-- Version 2.0.0   30-Jul-07 (version 2) separate 
--
separate (Simrail2)
package body Relay_Pkg is
   use Dio192defs;

   procedure Tell (
         R           : in out Relay_Type; 
         Dir_Command : in     Polarity_Type;    
         Changed     : in out Boolean     ) is 
      --
      -- Possibly start relay moving.
      -- If a premature end to the command the relay will
      -- drop back effectively from halfway.
      -- T.Time is set to zero at the start of a movement 
      -- (Go_Norm, Go_Rev) and it takes 2 ticks to complete.
      --
   begin
      case R.State is
         when Norm =>
            if Dir_Command /= Normal_Pol then
               R.State := Go_Rev;
               R.Time := 0;
            end if;
         when Rev =>
            if Dir_Command = Normal_Pol then
               R.State := Go_Norm;
               R.Time := 0;
            end if;
         when Go_Norm =>
            if Dir_Command /= Normal_Pol then
               R.State := Go_Rev;
               R.Time := 1;
            end if;
         when Go_Rev =>
            if Dir_Command = Normal_Pol then
               R.State := Go_Norm;
               R.Time := 1;
            end if;
      end case;
      Changed := True;
   end Tell;

   procedure Tick (
         R       : in out Relay_Type; 
         Changed : in out Boolean     ) is 
      --
      -- advance R.time by 10 ms (usually)
      -- Must be called BEFORE Tell().
      -- If in motion (Go_Norm, Go_Rev) might arrive (2 ticks).
      -- 
   begin
      case R.State is
         when Norm =>
            null;
         when Rev =>
            null;
         when Go_Norm =>
            R.Time := R.Time + 1;
            if R.Time = Tflip then
               R.State := Norm;
               R.Time := 0;
               Changed := True;
            end if;
         when Go_Rev =>
            R.Time := R.Time + 1;
            if R.Time = Tflip then
               R.State := Rev;
               R.Time := 0;
               Changed := True;
            end if;
      end case;
   end Tick;

   function Is_Open_Circuit (
         R : Relay_Type ) 
     return Boolean is 
   begin
      return R.State > Rev;
   end Is_Open_Circuit;

   Result : constant
   array (Relay_State) of Float :=
      (
      Norm             => 1.0,   
      Rev              => - 1.0, 
      Go_Norm | Go_Rev => 0.0);

   function Polarity (
         R : Relay_Type ) 
     return Float is 
      -- return -1.0, 0, +1.0
   begin
      return Result(R.State);
   end Polarity;

end Relay_Pkg;