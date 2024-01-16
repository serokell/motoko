import Prim "mo:⛔";

actor class UpgradeTarget() {
  stable var stableArray = Prim.Array_tabulate<Nat>(10_000, func(index) { index });

  system func preupgrade() {
    Prim.debugPrint("PRE-UPGRADE HOOK!");
  };

  system func postupgrade() {
    Prim.debugPrint("POST-UPGRADE HOOK!");
  };
};
