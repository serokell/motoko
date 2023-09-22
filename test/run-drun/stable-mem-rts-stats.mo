import P "mo:⛔";
import StableMemory "stable-mem/StableMemory";
actor {
  let s1 = P.rts_stable_memory_size();
  let l1 = P.rts_logical_stable_memory_size();
  P.debugPrint (debug_show({s1;l1}));
  let 0 = StableMemory.grow(16);
  stable var v = StableMemory.loadBlob(0, 65536);
  let s2 = P.rts_stable_memory_size();
  ();
  let l2 = P.rts_logical_stable_memory_size();
  P.debugPrint (debug_show({s2;l2}));
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref
//CALL upgrade ""
//CALL upgrade ""
//CALL upgrade ""