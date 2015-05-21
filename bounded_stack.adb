package body Bounded_Stack is

   procedure EnStack (Stack : in out Stack_Type;
                      Item  : in     Element_Type) is
   begin --EnStack
      if Stack.Top > Stack.Max_Size then
         raise Overflow;
      else
         Stack.Items(Stack.Top) := Item;
         Stack.top := Stack.top   + 1;
      end if;
   end EnStack;

   procedure DeStack (Stack : in out Stack_Type;
                      Item  :    out Element_Type) is
   begin --DeStack
      if Stack.top = 1 then
         raise Underflow;
      else
          Stack.top := Stack.top   - 1;
         Item := Stack.Items(Stack.Top);
      end if;
   end DeStack;

   function Full (Stack : in Stack_Type) return Boolean is
   begin--Full
      return Stack.Top = Stack.Max_Size;
   end Full;


   function Empty (Stack : in Stack_Type) return Boolean is
   begin --Empty
      return Stack.Top = 1;
   end Empty;

   function get_count(Stack : in Stack_Type) return Natural is
   begin
      return  Stack.Top-1;
   end get_count;

end Bounded_Stack;
