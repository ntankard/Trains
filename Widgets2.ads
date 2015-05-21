-- Widgets2 : an example multi-Sporadic where there are two Start methods
-- with different parameter profiles.  This is easier for client code to call.
-- R K Allen, Swinburne Univ Tech.  13-May-03
with Projdefs;  use Projdefs;
package Widgets2 is

   procedure Start_Run(
      Id : in Widget_Id;
      Speed: in Speed_Type);
      
   procedure Start_Stop(
      Id : in Widget_Id);
      
end Widgets2;
