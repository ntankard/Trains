generic
   type Element_Type is private;
package Bounded_Queue is

   type Queue_Type(Max_Size : Positive) is limited private;

   Underflow: exception;
   Overflow: exception;


   procedure Enqueue(Queue: in out Queue_Type;
                     Item : in Element_Type);

   procedure Dequeue (Queue : in out Queue_Type;
                      Item  :    out Element_Type);

   function Full (Queue : in Queue_Type) return Boolean;
   function Empty (Queue : in Queue_Type) return Boolean;

   function get_Last(Queue : in Queue_Type) return Element_Type;
   function get_Front(Queue : in Queue_Type) return Element_Type;
   function get_Count(Queue : in Queue_Type) return Natural;

private
   type Queue_Array is array (Positive range <>) of Element_Type;
   type Queue_Type(Max_Size : Positive) is
      record
         Count: Natural :=0;
         to_in: Positive  := 1;
         to_out: Positive  := 1; --cahnged from max to 1, this why it reads the newest thing, not the second newest
         Items : Queue_Array(1..Max_Size);
      end record;



end Bounded_Queue;
