//MOC-FLAG --force-gc --stable-regions
import {
   performanceCounter;
   rts_heap_size;
   debugPrint;
   regionNew = new;
   regionSize = size;
   regionGrow = grow;
   regionLoadNat16 = loadNat16;
   regionStoreNat16 = storeNat16;
} = "mo:⛔";

actor stablemem {

    func counters() : (Int, Nat64) = (rts_heap_size(), performanceCounter(0));

    let block_pages : Nat64 = 128;
    let blocks : Nat64 = 3;
    let pages : Nat64 = blocks * block_pages;
    let r = new();
    let _ = grow(r, pages);
    assert size(r) == pages;

    // write and read 3 blocks of nat16
    public func go() : async () {
        let (m0, n0) = counters();
        var o : Nat64 = 0;
        var n16 : Nat16 = 0;
        while (o < pages * 65536) {
           storeNat16(r, o, n16);
           o += 2;
           n16 +%= 1;
        };
        o := 0;
        n16 := 0;
        while (o < pages * 65536) {
           let m16 = loadNat16(r, o);
           assert m16 == n16;
           o += 2;
           n16 +%= 1;
        };
        let (m1, n1) = counters();
        debugPrint(debug_show {heap_diff = m1 - m0; instr_diff = n1 - n0});
    }
}
//SKIP run-low
//SKIP run
//SKIP run-ir
//SKIP ic-ref-run
//CALL ingress go 0x4449444C0000

