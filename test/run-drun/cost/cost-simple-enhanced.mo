//MOC-FLAG --enhanced-orthogonal-persistence
import Cost "cost-simple-class";

persistent actor {
  public func test() : async () {
    Cost.test();
  };
};
