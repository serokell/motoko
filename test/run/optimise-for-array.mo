//MOC-FLAG -fshared-code
import Prim "mo:⛔";

// Differences between incremental and non-incremental compilation (additional forwarding header field).

// CHECK: (local $check0 i32)

// CHECK-NOT:  call $@immut_array_size
// DON'TCHECK: i32.load offset=(5 or 9) 
// CHECK:      i32.load offset= 
// CHECK:      i32.const 2
// CHECK:      i32.shl
// CHECK:      i32.lt_u
// CHECK:      i32.add
// DON'TCHECK: i32.load offset=(9 or 13)
// CHECK:      local.tee $check0
// CHECK-NEXT: call $print_text
// CHECK:      i32.const 4
// CHECK-NEXT: i32.add
for (check0 in ["hello", "world"].vals()) { Prim.debugPrint check0 };


// CHECK-NOT:  call $@mut_array_size
// DON'TCHECK: i32.load offset=(5 or 9)
// CHECK:      i32.load offset=
// CHECK:      i32.const 2
// CHECK-NEXT: i32.shl
// CHECK:      i32.lt_u
// CHECK:      i32.add
// DON'TCHECK: i32.load offset=(9 or 13)
// CHECK:      i32.load offset=
// CHECK-NEXT: local.tee $check1
// CHECK-NEXT: call $print_text
for (check1 in [var "hello", "mutable", "world"].vals()) { Prim.debugPrint check1 };

let array = [var "hello", "remutable", "world"];
array[1] := "mutable";
// CHECK-NOT:   call $@immut_array_size
// DON'TCHECK:  i32.load offset=(5 or 9)
// CHECK:       i32.load offset=
// CHECK:       i32.const 2
// CHECK:       i32.shl
// DON'T-CHECK: i32.lt_u
// DON'T-CHECK: local.get $array
// DON'T-CHECK: local.set $check2
// `arr` being a `VarE` already (but we rebind anyway, otherwise we open a can of worms)
// later when we have path compression for variables in the backend, we can bring this back
for (check2 in array.vals()) { Prim.debugPrint check2 };

// CHECK-NOT:  call $@immut_array_size
// DON'TCHECK: i32.load offset=(5 or 9)
// CHECK:      i32.load offset=
// CHECK:      i32.const 2
// CHECK:      i32.shl
// CHECK:      i32.lt_u
// CHECK:      i32.add
// DON'TCHECK: i32.load offset=(9 or 13)
// CHECK:      i32.load offset=
// CHECK-NEXT: local.tee $check3
// interfering parentheses don't disturb us
for (check3 in (((["hello", "immutable", "world"].vals())))) { Prim.debugPrint check3 };


// CHECK:      i32.const 84
// CHECK:      call $B_add
// CHECK-NEXT: call $B_eq
// CHECK-NEXT: if
// CHECK-NEXT: loop
// CHECK-NEXT: br 0
// CHECK-NEXT: end
// CHECK-NEXT: unreachable
// CHECK-NEXT: else
// bottom iteration expression is treated fairly
var c = 42;
if (c == c + 1) {
    for (check4 in (loop {}).vals()) { Prim.debugPrint check4 }
};

// CHECK:      call $B_add
// CHECK-NEXT: call $B_eq
// CHECK-NEXT: if
// CHECK-NEXT: loop
// CHECK-NEXT: br 0
// CHECK-NEXT: end
// CHECK-NEXT: unreachable
// CHECK-NEXT: else
// typed bottom iteration expression is treated fairly
if (c == c + 1) {
    for (check5 in ((loop {}) : [Text]).vals()) { Prim.debugPrint check5 }
};

let check6 = [var "hello", "immutable", "world"];
check6[1] := "mutable";
// `check6` being a `VarE` already and iteration variable is named identically
// this passes the IR type check, which demonstrates that no name capture happens
for (check6 in check6.vals()) { ignore check6 };

// DON'TCHECK: i32.load offset=(5 or 9)
// CHECK:      i32.load offset=
// CHECK:      i32.const 2
// CHECK:      i32.shl
// argument to vals can have an effect too, expect it
for (check7 in [].vals(Prim.debugPrint "want to see you")) { };

// CHECK:      local.set $num8
// CHECK-NOT:  call $@immut_array_size
// DON'TCHECK: i32.load offset=(5 or 9)
// CHECK:      i32.load offset=
// CHECK:      i32.const 1
// CHECK:      i32.shl
// CHECK:      i32.lt_u
// CHECK-NOT:  i32.add
// CHECK:      local.tee $check8
// CHECK-NEXT: local.get $num8
// CHECK-NEXT: call $B_add
var num8 = 42;
num8 := 25;
// `keys` is even easier to rewrite, as the "indexing expression" is just the
// indexing variable itself
for (check8 in ["hello", "keyed", "world"].keys()) { ignore (check8 + num8) };

// polymorphic arrays should still work
func f9<A>(array : [A]) {
  for (check9 in array.keys()) { }
};

// make sure that one-byte-sized elements still work
var sum10 : Nat8 = 0;
for (check10 in ([3, 5, 7, 11] : [Nat8]).vals()) { sum10 += check10 };
assert sum10 == 26
