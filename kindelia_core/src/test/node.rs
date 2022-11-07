use proptest::collection::vec;
use proptest::proptest;

use crate::bits::ProtoSerialize;
use crate::node;
use crate::test::strategies::statement;
use crate::util;

proptest! {
  #[test]
  fn serialize_deserialize_transaction_into_statement(statements in statement()) {
    let s1 = format!("{:?}", statements);
    println!("{}", statements);
    let bytes = util::bitvec_to_bytes(&statements.proto_serialized());
    // TODO filter this using proptest
    // ignore statements which oversize transaction
    if let Ok(transaction) = node::Transaction::new(bytes) {
      let statements = transaction.to_statement().unwrap();
      let s2 = format!("{:?}", statements);
      assert_eq!(s1, s2);
    }
  }
}
