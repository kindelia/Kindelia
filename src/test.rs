use crate::hvm::{init_runtime, name_to_u128, show_term, Runtime, view_rollback, u128_to_name, Rollback, Heap, U128_NONE};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use im::HashMap;
use proptest::proptest;

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

pub fn view_rollback_ticks(rt: &Runtime) -> String {
  fn view_rollback_ticks_go(rt: &Runtime, back: &Arc<Rollback>) -> Vec<Option<u128>> {
    match &**back {
      Rollback::Nil => {
        return Vec::new()
      }
      Rollback::Cons { keep, head, tail, life } => {
        let mut vec = view_rollback_ticks_go(&rt, tail);
        let tick = rt.get_heap(*head).tick;
        vec.push(Some(tick));
        return vec;
      }
    }
  }


  let back = rt.get_back();
  let ticks = view_rollback_ticks_go(rt, &back);
  let elems = 
    ticks
      .iter()
      .rev()
      .map(|x| 
        if let Some(x) = x {
          format!("{}", if *x != U128_NONE { *x } else { 0 }) 
        } else { 
          "___________".to_string()
        }
      )
      .collect::<Vec<String>>()
      .join(", ");
  return format!("[{}]", elems);
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
  advance(&mut rt, 1000, Some(COUNTER));
}

#[test]
#[ignore]
// TODO: fix drop stack overflow
pub fn stack_overflow2() { // caused by drop of term
  let mut rt = init_runtime();
  rt.run_statements_from_code(PRE_COUNTER, false);
  rt.run_statements_from_code(COUNTER_STACKOVERFLOW , false);
}

#[test]
#[ignore]
// TODO: fix runtime persistence
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
// TODO: fix runtime persistence
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
    !call ~ 'Count' [{Count_Inc}]
    !call x 'Count' [{Count_Get}]
    !done x
  }
";

pub const SIMPLE_COUNT: &'static str = "
  run {
    !call ~ 'Count' [{Count_Inc}]
    !call x 'Count' [{Count_Get}]
    !done x
  }
";

pub const COUNTER_STACKOVERFLOW: &'static str = "
  run {
    !done (ToSucc #8000)
  }
";

#[test]
#[ignore]
fn one_hundred_snapshots() {
  // run this with rollback in each 4th snapshot
  // note: this test has no state
  let mut rt = init_runtime();
  for i in 0..100000 {
    rt.tick();
    println!(" - tick: {}, - rollback: {}", rt.get_tick(), view_rollback_ticks(&rt));
  }
}
