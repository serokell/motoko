import Prim "mo:⛔";
import StableMemory "stable-mem/StableMemory";

func ensure(offset : Nat64) {
  let pages = (offset + 65536) >> 16;
  if (pages > StableMemory.size()) {
    let oldsize = StableMemory.grow(pages - StableMemory.size());
    assert (oldsize != 0xFFFF_FFFF_FFFF_FFFF);
  };
};

var base : Nat64 = 0;

func log(t : Text) {
  let blob = Prim.encodeUtf8(t);
  let size = Prim.natToNat64(blob.size());
  ensure(base + size + 4);
  StableMemory.storeBlob(base, blob);
  base += size;
  StableMemory.storeNat32(base, Prim.natToNat32(blob.size()));
  base += 4;
};

func readLast(count : Nat) : [Text] {
  let a = Prim.Array_init<Text>(count, "");
  var offset = base;
  var k = 0;
  while (k < count and offset > 0) {
    offset -= 4;
    let size = StableMemory.loadNat32(offset);
    offset -= Prim.natToNat64(Prim.nat32ToNat(size));
    let blob = StableMemory.loadBlob(offset, Prim.nat32ToNat(size));
    switch (Prim.decodeUtf8(blob)) {
      case (?t) { a[k] := t };
      case null { assert false };
    };
    k += 1;
  };
  return Prim.Array_tabulate<Text>(k, func i { a[i] });
};


// testing
var count = 0;

func populate() : () {
  // populate
  let limit = count + 10;
  while (count < limit) {
    log(debug_show(count));
    count += 1;
  };
};

func readAll() : () {
  let ts = readLast(count);
  for (t in ts.vals()) {
    Prim.debugPrint(t);
  };
};

func readExtra() : () {
  let ts = readLast(2*count);
  for (t in ts.vals()) {
    Prim.debugPrint(t);
  }
};

populate();
readAll();
populate();
readAll();
readExtra();

//SKIP run
//SKIP run-low
//SKIP run-ir
