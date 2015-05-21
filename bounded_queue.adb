package body Bounded_Queue is

   procedure Enqueue (Queue : in out Queue_Type;
                      Item  : in     Element_Type) is
   begin --Enqueue
      if Queue.Count = Queue.Max_Size then
         raise Overflow;
      else

          Queue.to_in := Queue.to_in  rem  Queue.Max_Size + 1;
         Queue.Items(Queue.to_in) := Item;
         Queue.Count := Queue.Count + 1;
      end if;
   end Enqueue;

   procedure Dequeue (Queue : in out Queue_Type;
                      Item  :    out Element_Type) is
   begin --Dequeue

      if Queue.Count = 0 then
         raise Underflow;
      else
         Queue.to_out :=  Queue.to_out  rem  Queue.Max_Size + 1;
         Item := Queue.Items(Queue.to_out);
         Queue.Count := Queue.Count - 1;
      end if;
   end Dequeue;

   function get_Last(Queue : in Queue_Type) return Element_Type is
      begin
         return Queue.Items(Queue.to_out  rem  Queue.Max_Size + 1);
         end get_Last;
      function get_Front(Queue : in Queue_Type) return Element_Type is
      begin
         return Queue.Items(Queue.to_in);
         end get_Front;

   function Full (Queue : in Queue_Type) return Boolean is
   begin--Full
      return Queue.Count = Queue.Max_Size;
   end Full;


   function Empty (Queue : in Queue_Type) return Boolean is
   begin --Empty
      return Queue.Count = 0;
   end Empty;

   function get_count(Queue : in Queue_Type) return Natural is
   begin
      return  Queue.Count;
      end get_count;

end Bounded_Queue;
