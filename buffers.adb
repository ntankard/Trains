package body Buffers is

  protected  body Command_Buffer is --commands for trains, including interupts, heartbeats and user made

      procedure insert(C : in command_Dec.Command_Type)
      is
      begin --Add
         if(not Has_Room) then
            Command_Buffer_full := true;
         else
            Command_Queue.Enqueue(Q, C);
            Has_Room := not Command_Queue.Full(Q);
            Has_Item := True;
         end if;
      end insert;

      entry Remove(C : out command_Dec.Command_Type; Error: out Boolean) when Has_Item
      is
      begin --Remove
         Error:=Command_Buffer_full;
         Command_Buffer_full := false;
         Command_Queue.Dequeue(Q, C);
         Has_Room := True;
         Has_Item := not Command_Queue.Empty(Q);
      end Remove;

   end Command_Buffer;




    protected  body Block_Buffer is --blocks train owns
      procedure insert(B : in raildefs.Block_Id; Error: out Boolean)
      is
      begin --Add
         if(not Has_Room) then
            Error:=true;
         else
            Block_Queue.Enqueue(Q, B);
            Has_Room := not Block_Queue.Full(Q);
            Has_Item := True;
            Error:=false;
         end if;

      end insert;

      entry Remove(B : out raildefs.Block_Id) when Has_Item
      is
      begin --Remove

         Block_Queue.Dequeue(Q,B);
         Has_Room := True;
         Has_Item := not Block_Queue.Empty(Q);
      end Remove;

      function get_Last return raildefs.Block_Id is
      begin
         return Block_Queue.get_Last(Q);
         end get_Last;
      function get_Front return raildefs.Block_Id is
      begin
          return Block_Queue.get_Front(Q);
         end get_Front;

      function get_count return natural is
      begin
         return Block_Queue.get_count(Q);
end get_count;
   end Block_Buffer;


    protected  body Block_Buffer_stack is --blocks train owns
      procedure insert(B : in raildefs.Block_Id; Error: out Boolean)
      is
      begin --Add
         if( not Has_room) then
            Error := true;
         else
            Block_Stack.Enstack(S, B);
            Has_Room := not Block_Stack.Full(S);
            Has_Item := True;
            Error:= false;
         end if;

      end insert;

      entry Remove(B : out raildefs.Block_Id) when Has_Item
      is
      begin --Remove

         Block_Stack.Destack(S, B);
         Has_Room := True;
         Has_Item := not Block_Stack.Empty(S);
      end Remove;

      function get_count return natural is
      begin
         return Block_Stack.get_count(S);
end get_count;
   end Block_Buffer_stack;






end Buffers;
