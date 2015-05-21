-- circular array code for RTP lecture 6 2007
-- (procedure wrapper to make it compile)
-- This is the form for a producer-consumer buffer.
-- Rob Allen 1-Apr-07
procedure Circular_Array is
   subtype Item_Type is Float;  -- whatever

   N : constant := 8;
   type Index_Type is mod N;
   type Item_Array is array (Index_Type) of Item_Type;

   protected  Pr_Buffer is
      entry Add (
            C : in     Item_Type );
      entry Remove (
            C :    out Item_Type );
   private
      Items : Item_Array;
      Iin, Jout : Index_Type := 0;
      Count : Integer := 0;
   end Pr_Buffer;

   protected  body Pr_Buffer is

      entry Add (
            C : in     Item_Type ) when Count < N is
      begin
         Items (Iin) := C;
         Iin := Iin + 1;
         Count := Count + 1;
      end Add;

      entry Remove (
            C :    out Item_Type ) when Count > 0 is
      begin
         C := Items(Jout);
         Jout := Jout + 1;
         Count := Count - 1;
      end Remove;

   end Pr_Buffer;

begin
   null;
end;
