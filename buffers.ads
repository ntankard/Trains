with Bounded_Queue;
with Bounded_Stack;
with Raildefs;
with command_Dec;

package Buffers is
   package Command_Queue is new Bounded_Queue(Element_Type => command_Dec.Command_Type);
   package Block_Queue is new Bounded_Queue(Element_Type => raildefs.Block_Id);
   package Block_Stack is new Bounded_Stack(Element_Type => raildefs.Block_Id);


    protected type Command_Buffer is
     procedure insert(C : in command_Dec.Command_Type) ;
      entry Remove(C : out command_Dec.Command_Type; Error: out Boolean);

   private
      Q : Command_Queue.Queue_Type (Max_Size => 25);
      Has_Room : Boolean := True;
      Has_Item : Boolean := False;
      Command_Buffer_full:Boolean := false;
   end Command_Buffer;

   protected type Block_Buffer is
      procedure insert(B : in raildefs.Block_Id; Error: out Boolean) ;
      entry Remove(B : out raildefs.Block_Id);
 function get_Last return raildefs.Block_Id;
      function get_Front return raildefs.Block_Id;
      function get_count return natural;
   private
     Q : Block_Queue.Queue_Type (Max_Size => raildefs.Num_Blocks);
      Has_Room : Boolean := True;
      Has_Item : Boolean := False;
   end Block_Buffer;

   protected type Block_Buffer_stack is
      procedure insert(B : in raildefs.Block_Id; Error: out Boolean) ;
      entry Remove(B : out raildefs.Block_Id);
 	function get_count return natural;
   private
     S : Block_Stack.Stack_Type (Max_Size => raildefs.Num_Blocks);
      Has_Room : Boolean := True;
      Has_Item : Boolean := False;
   end Block_Buffer_stack;

end Buffers;
