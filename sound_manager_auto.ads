with Raildefs;
package Sound_Manager_auto is

   procedure Horn_sound(T: in Raildefs.Train_Id; Period: in Duration);
   procedure Bell_sound(T: in Raildefs.Train_Id; Period: in Duration);
end Sound_Manager_auto;
