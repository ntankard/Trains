generic
   type Element_Type is private;
package Bounded_Stack is

   type Stack_Type(Max_Size : Positive) is limited private;

   Underflow: exception;
   Overflow: exception;


   procedure EnStack(Stack: in out Stack_Type;
                     Item : in Element_Type);

   procedure DeStack (Stack : in out Stack_Type;
                      Item  :    out Element_Type);

   function Full (Stack : in Stack_Type) return Boolean;
   function Empty (Stack : in Stack_Type) return Boolean;
   function get_Count(Stack : in Stack_Type) return Natural;

private
   type Stack_Array is array (Positive range <>) of Element_Type;
   type Stack_Type(Max_Size : Positive) is
      record
         top: Positive  := 1;
         Items : Stack_Array(1..Max_Size);
      end record;



end Bounded_Stack;
