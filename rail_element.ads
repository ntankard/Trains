with raildefs;
with Topolog2;
package Rail_element is

  function Assign_Block(requester: raildefs.Train_Id;
                         requested:raildefs.Block_Id;
                         direction: raildefs.Polarity_Type)
                        return Boolean;


   function Assign_Crossing(requester: raildefs.Train_Id;
                         requested:raildefs.Crossing_Id)
                            return Boolean;

   function Assign_Zone(requester: raildefs.Train_Id;
                        requested:topolog2.Zone_Id)
    			 return Boolean;




   procedure Unassign_Block(requested:raildefs.Block_Id);
   procedure Unassign_Crossing(requested:raildefs.Crossing_Id);
   procedure Unassign_Zone(requested:topolog2.Zone_Id);


   function Set_Turnout( requester: raildefs.Train_Id;
                        requested:raildefs.Turnout_Id;
                          position: raildefs.Turnout_Pos)
                           return Boolean;

   procedure Change_Direction(requester: raildefs.Train_Id;
                              requested:raildefs.Block_Id);



   function Get_Turnout_Pos(Turnout: raildefs.Turnout_Id) return raildefs.Turnout_Pos;



end Rail_element;
