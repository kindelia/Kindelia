use crate::{
  bits::{deserialized_func, serialized_func},
  hvm::{init_map, init_runtime, name_to_u128, read_statements, u128_to_name, view_statements},
  test::{
    strategies::{func, heap, name, statement},
    util::{
      advance, rollback, rollback_path, rollback_simple, temp_dir, test_heap_checksum,
      view_rollback_ticks, RuntimeStateTest, TempDir,
    },
  },
};
use proptest::collection::vec;
use proptest::proptest;
use rstest::rstest;

#[rstest]
pub fn simple_rollback(temp_dir: TempDir) {
  let fn_names = ["Count", "Store", "Sub", "Add"];
  assert!(rollback_simple(PRE_COUNTER, COUNTER, &fn_names, 1000, 1, &temp_dir.path));
}

#[rstest]
pub fn advanced_rollback_in_random_state(temp_dir: TempDir) {
  let fn_names = ["Count", "Store", "Sub", "Add"];
  let path = [1000, 12, 1000, 24, 1000, 36];
  assert!(rollback_path(PRE_COUNTER, COUNTER, &fn_names, &path, &temp_dir.path));
}

#[rstest]
pub fn advanced_rollback_in_saved_state(temp_dir: TempDir) {
  let fn_names = ["Count", "Store", "Sub", "Add"];
  let mut rt = init_runtime(Some(&temp_dir.path));
  rt.run_statements_from_code(PRE_COUNTER, true);
  advance(&mut rt, 1000, Some(COUNTER));
  rt.rollback(900);
  println!(" - tick: {}", rt.get_tick());
  let s1 = RuntimeStateTest::new(&fn_names, &mut rt);

  advance(&mut rt, 1000, Some(COUNTER));
  rt.rollback(900);
  println!(" - tick: {}", rt.get_tick());
  let s2 = RuntimeStateTest::new(&fn_names, &mut rt);

  advance(&mut rt, 1000, Some(COUNTER));
  rt.rollback(900);
  println!(" - tick: {}", rt.get_tick());
  let s3 = RuntimeStateTest::new(&fn_names, &mut rt);

  assert_eq!(s1, s2);
  assert_eq!(s2, s3);
}

#[rstest]
pub fn advanced_rollback_run_fail(temp_dir: TempDir) {
  let fn_names = ["Count", "Store", "Sub", "Add"];
  let path = [2, 1, 2, 1, 2, 1];
  assert!(rollback_path(PRE_COUNTER, COUNTER, &fn_names, &path, &temp_dir.path));
}

#[rstest]
pub fn stack_overflow(temp_dir: TempDir) {
  // caused by compute_at function
  let mut rt = init_runtime(Some(&temp_dir.path));
  rt.run_statements_from_code(PRE_COUNTER, true);
  advance(&mut rt, 1000, Some(COUNTER));
}

#[rstest]
#[ignore = "fix not done"]
// TODO: fix drop stack overflow
pub fn stack_overflow2(temp_dir: TempDir) {
  // caused by drop of term
  let mut rt = init_runtime(Some(&temp_dir.path));
  rt.run_statements_from_code(PRE_COUNTER, false);
  rt.run_statements_from_code(COUNTER_STACKOVERFLOW, false);
}

#[rstest]
pub fn persistence1(temp_dir: TempDir) {
  println!("{}", temp_dir.path.as_path().display());

  let fn_names = ["Count", "Store", "Sub", "Add"];
  let mut rt = init_runtime(Some(&temp_dir.path));
  rt.run_statements_from_code(PRE_COUNTER, true);

  advance(&mut rt, 1000, Some(COUNTER));
  let s1 = RuntimeStateTest::new(&fn_names, &mut rt);

  rt.rollback(999); // rollback for the latest rollback saved
  let s2 = RuntimeStateTest::new(&fn_names, &mut rt);

  advance(&mut rt, 1000, Some(COUNTER));
  let s3 = RuntimeStateTest::new(&fn_names, &mut rt);

  rt.restore_state().expect("Could not restore state"); // restore last rollback, must be equal to s2
  let s4 = RuntimeStateTest::new(&fn_names, &mut rt);

  advance(&mut rt, 1000, Some(COUNTER));
  let s5 = RuntimeStateTest::new(&fn_names, &mut rt);

  assert_eq!(s1, s3);
  assert_eq!(s2, s4);
  assert_eq!(s3, s5);
}

#[rstest]
fn one_hundred_snapshots(temp_dir: TempDir) {
  // run this with rollback in each 4th snapshot
  // note: this test has no state
  let mut rt = init_runtime(Some(&temp_dir.path));
  for i in 0..100000 {
    rt.tick();
    println!(" - tick: {}, - rollback: {}", rt.get_tick(), view_rollback_ticks(&rt));
  }
}

#[test]
#[should_panic]
fn parse_ask_fail1() {
  read_statements(ASK_FAIL_1).unwrap();
}

#[test]
#[should_panic]
fn parse_ask_fail2() {
  read_statements(ASK_FAIL_2).unwrap();
}

#[test]
#[should_panic]
fn parse_ask_fail3() {
  read_statements(ASK_FAIL_3).unwrap();
}

proptest! {
  #[test]
  fn name_conversion(name in name()) {
    let a = u128_to_name(name);
    let b = name_to_u128(&a);
    let c = u128_to_name(b);
    assert_eq!(name, b);
    assert_eq!(a, c);
  }

  #[test]
  fn parser(statements in vec(statement(), 0..10)) {
    let str = view_statements(&statements);
    let (.., s1) = read_statements(&str).unwrap();
    assert_eq!(statements, s1);
  }

  #[test]
  #[ignore = "slow"]
  fn serialize_deserialize_heap(heap in heap()) {
    let mut h1 = heap;
    let s1 = format!("{:?}", h1);
    let a = h1.serialize();
    h1.deserialize(&a);
    let s2 = format!("{:?}", h1);
    assert_eq!(s1, s2);
  }
}

// ===========================================================
// Codes
pub const PRE_COUNTER: &'static str = "
  ctr {Succ p}
  ctr {Zero}

  fun (ToSucc n) {
    (ToSucc #0) = {Zero}
    (ToSucc n) = {Succ (ToSucc (- n #1))}
  }

  fun (Add n) {
    (Add n) = {Succ n}
  }

  fun (Sub n) {
    (Sub {Succ p}) = p
    (Sub {Zero}) = {Zero}
  }

  ctr {StoreAdd}
  ctr {StoreSub}
  ctr {StoreGet}

  fun (Store action) {
    (Store {StoreAdd}) =
      ask l = (Take);
      ask (Save (Add l));
      (Done #0)
    (Store {StoreSub}) =
      ask l = (Take);
      ask (Save (Sub l));
      (Done #0)
    (Store {StoreGet}) = 
      ask l = (Load);
      (Done l)
  } with { {Zero} }
";

pub const COUNTER: &'static str = "
  run {
    ask (Call 'Store' [{StoreAdd}]);
    ask count = (Call 'Store' [{StoreGet}]);
    (Done count)
  }

  run {
    ask (Call 'Count' [{Inc}]);
    ask count = (Call 'Count' [{Get}]);
    (Done count)
  }
";

pub const SIMPLE_COUNT: &'static str = "
  run {
    ask (Call 'Count' [{Inc}]);
    ask count = (Call 'Count' [{Get}]);
    (Done count)
  }
";

pub const COUNTER_STACKOVERFLOW: &'static str = "
  run {
    (Done (ToSucc #8000))
  }
";

pub const ASK_FAIL_1: &'static str = "
fun (Test c) {
  (Test {Aa ask aa}) = dup x y = #2; (+ x y)
} with {
  (S84_dKIY_)
} sign {
  a0389bb267d0cebd9190b74c65
  33acf1c57b4cdb5166f202edfd
  d52f06c3f4e560d01e3ced971a
  54f3b3b47133daa1befe226a77
  48afa13c8b2d3182382ee2fde8
}
";

pub const ASK_FAIL_2: &'static str = "
fun (Test) {
  (Test) = dup ask y = #2; (+ ask y)
}
";

pub const ASK_FAIL_3: &'static str = "
fun (Test ask) {
  (Test ask) = dup x y = ask; (+ x y)
}
";
