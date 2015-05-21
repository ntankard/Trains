with Turnout_Driver;
with Block_Driver;
with Dio192defs;
with Ada.Text_IO, Ada.Integer_Text_IO;
package body Rail_element is

   protected type Assosiation is
      procedure Assign(requester: in raildefs.Train_idx; valid : out Boolean);
      procedure Release;
      function Get_Owner return raildefs.Train_idx;
   private
      Owner: raildefs.Train_idx;
   end Assosiation;

   Block_owernship: array(raildefs.Block_Id) of Assosiation;
   Crossing_owernship: array(raildefs.Crossing_Id) of Assosiation;
   Zone_owernship: array(topolog2.Zone_Id) of Assosiation;

   function Assign_Block(requester: raildefs.Train_Id;
                         requested:raildefs.Block_Id;
                         direction: raildefs.Polarity_Type)
                         return Boolean is
      Valid:Boolean;
   begin --Assing_Block
      Block_owernship(requested).Assign(requester,Valid);
      if(Valid) then
         Block_Driver.Set_Cab(requested, raildefs.Cab_Type(requester));
         Block_Driver.Set_Polarity(requested, direction);
      end if;
      return Valid;
   end Assign_Block;

   function Assign_Zone(requester: raildefs.Train_Id;
                         requested:topolog2.Zone_Id)
                         return Boolean is
      Valid:Boolean;
        begin -- Assing Zone
       Zone_owernship(requested).Assign(requester,Valid);
      return Valid;
   end Assign_Zone;


   function Assign_Crossing(requester: raildefs.Train_Id;
                            requested:raildefs.Crossing_Id)
       return Boolean is
       Valid:Boolean;
   begin --Assing_turnout
      Crossing_owernship(requested).Assign(requester,Valid);
       return Valid;
   end Assign_Crossing;

   procedure Unassign_Block(requested:raildefs.Block_Id) is
      begin --Unassign_Block
      Block_owernship(requested).Release;
      Block_Driver.Set_Cab(requested,0);
   end Unassign_Block;

   procedure Unassign_Crossing(requested:raildefs.Crossing_Id) is
      begin --Unassign_Crossing
      Crossing_owernship(requested).Release;
   end Unassign_Crossing;

   procedure Unassign_Zone(requested:topolog2.Zone_Id) is
      begin --Unassign_Zone
      Zone_owernship(requested).Release;
   end Unassign_Zone;


   function Set_Turnout(requester: raildefs.Train_Id;
                        requested:raildefs.Turnout_Id;
                          position: raildefs.Turnout_Pos)
                           return Boolean is
      use Dio192defs; --for compare
      use raildefs;--for compare
      Valid:Boolean;
      State:Turnout_Status_Bit;
      Current_Pos: Turnout_Pos;

   begin --Assing_turnout
         --first see if this train has the right to set turnout
     -- Ada.Text_IO.Put_Line("Owner"&Block_owernship(topolog2.Turnout_Data(requested).Block_Num).Get_Owner'Img);
      --Ada.Text_IO.Put_Line("req"&requester'img);
      Valid:=  Block_owernship(topolog2.Turnout_Data(requested).Block_Num).Get_Owner = requester;
	if Valid then
         Turnout_driver.poll(requested, State, Current_Pos);
         if(State = busy) then
            Valid:=false; --rail is moving, wait
         end if;

         if(Current_Pos /= position) then
           Turnout_Driver.Pull(requested, position);
           Valid:=false; --rail is not in position, wait
         end if;
      else
         Ada.Text_IO.Put_Line("trunout access denied");
	end if;
      return Valid;
   end Set_Turnout;

   function Get_Turnout_Pos(Turnout: raildefs.Turnout_Id)
                            return raildefs.Turnout_Pos
   is
       use Dio192defs;
        State: Turnout_Status_Bit;
      Current_Pos: raildefs.Turnout_Pos;
   begin
      Turnout_driver.poll(Turnout, State, Current_Pos);
      --ada.Text_IO.Put_Line(Turnout'Img &"is at"&Current_pos'img);
      return Current_Pos;
   end Get_Turnout_pos;





   procedure Change_Direction(requester: raildefs.Train_Id;
                              requested:raildefs.Block_Id) is
      use Raildefs;-- for =
   begin --Change_Direction
      if( Block_owernship(requested).Get_Owner = requester) then
         --this is my owner, I will listen to what he tells me
         block_driver.Flip_Polarity(requested);
      end if;

   end Change_Direction;


   protected body Assosiation is

      procedure Assign(requester: in raildefs.Train_idx;
                       valid : out Boolean) is
         use raildefs;  --for comparisons
      begin --assing
         valid:= false;
         if(Owner = No_Train or else Owner = requester ) then -- added or in the event train asks for its own element it 'gets it' event though it only keeps it. THis is for zones.
            Owner:= requester;
            valid:= true;
         end if;
      end Assign;

      procedure Release is
         begin --release
         Owner:=raildefs.No_Train;
         end Release;

         function Get_Owner return raildefs.Train_idx is
         begin
         	return Owner;
         end Get_Owner;
   end Assosiation;

end Rail_element;
