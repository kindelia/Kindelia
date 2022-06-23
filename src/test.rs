use crate::crypto;
use crate::hvm::{init_runtime, name_to_u128, show_term, Runtime, view_rollback, u128_to_name, Term, Rule, Statement, view_statements, read_statements};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use im::HashMap;
use proptest::{proptest, prop_oneof, collection::vec, arbitrary::any, option};
use proptest::strategy::Strategy;

// Struct used to store interesting parts of runtime state
#[derive(Eq, PartialEq, Debug, Clone)]
struct RuntimeStateTest {
  checksum: u64,
  mana: u128,
  size: i128,
}
impl RuntimeStateTest {
  fn new(checksum: u64, mana: u128, size: i128) -> RuntimeStateTest {
    RuntimeStateTest { checksum, mana, size }
  }
}

// ===========================================================
// Aux functions
pub fn are_all_elemenets_equal<E: PartialEq>(vec: &[E]) -> bool {
  if vec.len() == 0 {
    return true;
  }
  let last_value = &vec[0];
  for value in vec.iter().skip(1) {
    if *value != vec[0] {
      return false;
    }
  }
  true
}

// Generate a checksum for a runtime state (for testing)
pub fn test_heap_checksum(fn_names: &[&str], rt: &mut Runtime) -> u64 {
  let fn_ids = fn_names.iter().map(|x| name_to_u128(x)).collect::<Vec<u128>>();
  let mut hasher = DefaultHasher::new();
  for fn_id in fn_ids {
    let term_lnk = rt.read_disk(fn_id);
    if let Some(term_lnk) = term_lnk {
      let term_lnk = show_term(rt, term_lnk, None);

      // dbg!(term_lnk.clone());
      term_lnk.hash(&mut hasher);
    }
  }
  let res = hasher.finish();
  // dbg!(res);
  res
}

pub fn rollback(rt: &mut Runtime, tick: u128, pre_code: Option<&str>, code: Option<&str>) {
  debug_assert!(tick < rt.get_tick());
  rt.rollback(tick);
  if rt.get_tick() == 0 {
    if let Some(pre_code) = pre_code {
      rt.run_statements_from_code(pre_code, true);
    }
  }
  let tick_diff = tick - rt.get_tick();
  for _ in 0..tick_diff {
    if let Some(code) = code {
      rt.run_statements_from_code(code, true);
    }
    rt.tick();
  }
  // println!("- final rollback tick {}", rt.get_tick());
}

pub fn advance(rt: &mut Runtime, tick: u128, code: Option<&str>) {
  debug_assert!(tick >= rt.get_tick());
  // println!("- advancing from {} to {}", rt.get_tick(), tick);
  let actual_tick = rt.get_tick();
  for _ in actual_tick..tick {
    if let Some(code) = code {
      rt.run_statements_from_code(code, true);
    }
    rt.tick();
  }
}

/// Tests the rollback of states in the kindelia runtime
///
/// # Arguments
///
/// * `pre_code` - Like a genesis block, use this to deploy functions and contracts
/// * `code` - The code that will be executed `total_tick` times, use this to test the states
/// * `fn_names` - The names of the functions which states will be tested
/// * `total_tick` - The number of times the code will be executed
/// * `rollback_tick` - The tick to rollback to
///
pub fn rollback_simple(
  pre_code: &str,
  code: &str,
  fn_names: &[&str],
  total_tick: u128,
  rollback_tick: u128,
) -> bool {
  let mut rt = init_runtime();

  // Calculate all total_tick states and saves old checksum
  let mut old_state = RuntimeStateTest::new(0, 0, 0);
  rt.run_statements_from_code(pre_code, true);
  for _ in 0..total_tick {
    rt.run_statements_from_code(code, true);
    rt.tick();
    // dbg!(test_heap_checksum(&fn_names, &mut rt));
    if rt.get_tick() == rollback_tick {
      old_state =
        RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());
    }
  }
  // Does rollback to nearest rollback_tick saved state
  rt.rollback(rollback_tick);
  if rt.get_tick() == 0 {
    rt.run_statements_from_code(pre_code, true);
  }
  // Run until rollback_tick
  let tick_diff = rollback_tick - rt.get_tick();
  for _ in 0..tick_diff {
    rt.run_statements_from_code(code, true);
    rt.tick();
  }
  // Calculates new checksum, after rollback
  let new_state =
    RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());
  // dbg!(old_state.clone(), new_state.clone());
  // Returns if checksums are equal
  old_state == new_state
}

// Does basically the same of rollback_simple, but with a path
// This path tells kindelia where to go
pub fn rollback_path(pre_code: &str, code: &str, fn_names: &[&str], path: &[u128]) -> bool {
  let mut states_store: HashMap<u128, Vec<RuntimeStateTest>> = HashMap::new();
  let mut insert_state = |rt: &mut Runtime| {
    let state =
      RuntimeStateTest::new(test_heap_checksum(&fn_names, rt), rt.get_mana(), rt.get_size());
    let vec = states_store.get_mut(&rt.get_tick());
    if let Some(vec) = vec {
      vec.push(state);
    } else {
      states_store.insert(rt.get_tick(), vec![state]);
    }
  };

  let mut rt = init_runtime();
  rt.run_statements_from_code(pre_code, true);

  for tick in path {
    let tick = *tick;
    if tick < rt.get_tick() {
      rollback(&mut rt, tick, Some(pre_code), Some(code));
    } else {
      advance(&mut rt, tick, Some(code));
    }
    insert_state(&mut rt);
  }

  // dbg!(states_store.clone());
  // Verify if all values from all vectors from all ticks of interest are equal
  states_store.values().all(|vec| are_all_elemenets_equal(vec))
}

// ===========================================================
// Tests
#[test]
pub fn simple_rollback() {
  let fn_names = ["Count", "IO.load", "Store", "Sub", "Add"];
  assert!(rollback_simple(PRE_COUNTER, COUNTER, &fn_names, 1000, 1));
}

#[test]
pub fn advanced_rollback_in_random_state() {
  let fn_names = ["Count", "IO.load", "Store", "Sub", "Add"];
  let path = [1000, 12, 1000, 24, 1000, 36];
  assert!(rollback_path(PRE_COUNTER, COUNTER, &fn_names, &path));
}

#[test]
pub fn advanced_rollback_in_saved_state() {
  let fn_names = ["Count", "IO.load", "Store", "Sub", "Add"];
  let mut rt = init_runtime();
  rt.run_statements_from_code(PRE_COUNTER, true);
  advance(&mut rt, 1000, Some(COUNTER));
  rt.rollback(900);
  println!(" - tick: {}", rt.get_tick());
  let s1 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());

  advance(&mut rt, 1000, Some(COUNTER));
  rt.rollback(900);
  println!(" - tick: {}", rt.get_tick());
  let s2 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());

  advance(&mut rt, 1000, Some(COUNTER));
  rt.rollback(900);
  println!(" - tick: {}", rt.get_tick());
  let s3 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());

  assert_eq!(s1, s2);
  assert_eq!(s2, s3);
}

#[test]
pub fn advanced_rollback_run_fail() {
  let fn_names = ["Count", "IO.load", "Store", "Sub", "Add"];
  let path = [2, 1, 2, 1, 2, 1];
  assert!(rollback_path(PRE_COUNTER, COUNTER, &fn_names, &path));
}

#[test]
pub fn stack_overflow() { // caused by compute_at function
  let mut rt = init_runtime();
  rt.run_statements_from_code(PRE_COUNTER, true);
  advance(&mut rt, 1, Some(COUNTER));
}

#[test]
#[ignore]
pub fn persistence1() {
  let fn_names = ["Count", "IO.load", "Store", "Sub", "Add"];
  let mut rt = init_runtime();
  rt.run_statements_from_code(PRE_COUNTER, true);
  advance(&mut rt, 50, Some(COUNTER));

  rt.clear_current_heap();
  let s1 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());
  
  rt.snapshot();
  rt.persist_state().expect("Could not persist state");

  advance(&mut rt, 55, Some(COUNTER));
  let s2 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());
  
  rt.restore_state().expect("Could not restore state");
  let s3 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());

  advance(&mut rt, 55, Some(COUNTER));
  let s4 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());
  
  assert_eq!(s1, s3);
  assert_eq!(s2, s4);
}

#[test]
#[ignore]
pub fn persistence2() {
  let fn_names = ["Count", "IO.load", "Store", "Sub", "Add"];
  let mut rt = init_runtime();
  rt.run_statements_from_code(PRE_COUNTER, true);
  advance(&mut rt, 1000, Some(COUNTER));
  rollback(&mut rt, 900, Some(PRE_COUNTER), Some(COUNTER));
  let s1 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());

  rt.persist_state().expect("Could not persist state");
  rt.clear_current_heap();
  let s2 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());

  advance(&mut rt, 1000, Some(COUNTER));
  rt.restore_state().expect("Could not restore state");
  let s3 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());

  advance(&mut rt, 1000, Some(COUNTER));
  rollback(&mut rt, 900, Some(PRE_COUNTER), Some(COUNTER));
  let s4 = RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());
  
  assert_eq!(s1, s4);
  assert_eq!(s2, s3);
}

// ===========================================================
// Codes
pub const PRE_COUNTER: &'static str = "
  ctr {Succ p}
  ctr {Zero}

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
      !take l
      !save (Add l)
      !done #0
    (Store {StoreSub}) =
      !take l
      !save (Sub l)
      !done #0
    (Store {StoreGet}) = 
      !load l
      !done l
  } with { {Zero} }
";

pub const COUNTER: &'static str = "
  run {
    !call ~ 'Store' [{StoreAdd}]
    !call x 'Store' [{StoreGet}]
    !done x
  }

  run {
    !call ~ 'Count' [{Count.Inc}]
    !call x 'Count' [{Count.Get}]
    !done x
  }
";

// ===========================================================
// Proptest

pub fn name() -> impl Strategy<Value = u128> {
  "[a-zA-Z0-9_][a-zA-Z0-9_.]{1,19}".prop_map(|s| name_to_u128(&s))
}

pub fn term() -> impl Strategy<Value = Term> {
    let leaf = prop_oneof![
        name().prop_map(|n| Term::Var{name: n}),
        name().prop_map(|n| Term::Num{numb: n}),
    ];

    leaf.prop_recursive(
      16, // 16 levels deep
      256, // Shoot for maximum size of 256 nodes
      10, // We put up to 10 items per collection
      |inner| {
        prop_oneof![
          (name(), name(), inner.clone(), inner.clone()).prop_map(|(n0, n1, e, b)| {
            Term::Dup { nam0: n0, nam1: n1, expr: Box::new(e), body: Box::new(b) }
          }),
          (name(), inner.clone()).prop_map(|(n, e)| {
            Term::Lam { name: n, body: Box::new(e) }
          }),
          (inner.clone(), inner.clone()).prop_map(|(f, a)| {
            Term::App { func: Box::new(f), argm: Box::new(a) }
          }),
          (name(), vec(inner.clone(), 0..10)).prop_map(|(n, v)| {
            Term::Ctr { name: n, args: v }
          }),
          (name(), vec(inner.clone(), 0..10)).prop_map(|(n, v)| {
            Term::Fun { name: n, args: v }
          }),
          (0..15_u128, inner.clone(), inner).prop_map(|(o, v0, v1)| {
            Term::Op2 { oper: o, val0: Box::new(v0), val1: Box::new(v1) }
          }),
        ]
      }
    )
}

pub fn rule() -> impl Strategy<Value = Rule> {
  (term(), term()).prop_map(|(lhs, rhs)| Rule{lhs, rhs})
}

pub fn sign() -> impl Strategy<Value = crypto::Signature> {
  (vec(any::<u8>(), 65)).prop_map(|s| {
    crypto::Signature(s.try_into().unwrap())
  })
}

pub fn statement() -> impl Strategy<Value = Statement> {
  prop_oneof![
    (name(), vec(name(), 0..10), vec(rule(), 0..10), term(), option::of(sign())).prop_map(|(n, a, r, i, s)| {
      Statement::Fun { name: n, args: a, func: r, init: i, sign: s }
    }),
    (name(), vec(name(), 0..10), option::of(sign())).prop_map(|(n, a, s)| {
      Statement::Ctr { name: n, args: a, sign: s }
    }),
    (term(), option::of(sign())).prop_map(|(t, s)| {
      Statement::Run { expr: t, sign: s }
    }),
    (name(), name(), option::of(sign())).prop_map(|(n, o, s)| {
      Statement::Reg { name: n, ownr: o, sign: s }
    }),
  ]
}

proptest!{
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
    let (.., s1) = read_statements(&str);
    assert_eq!(statements, s1);
  }
}
