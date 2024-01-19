import Prim "mo:prim";

actor {
   Prim.debugPrint("Version 2");

   let allocationSize = 40_000_000;

   func largeAllocation(name: Text): [var Nat] {
      Prim.trap("Should not be called");
   };

   stable var firstVariable : Any = largeAllocation("first variable");
   stable var secondVariable : Any = largeAllocation("second variable");

   public func check(): async() {
      // Extra GC increments.
      await async {};
      await async {};
      // Check that both variables have been cleared and both arrays have been reclaimed.
      assert(Prim.rts_heap_size() < allocationSize);
   }
};
