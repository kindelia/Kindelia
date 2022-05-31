use crate::hvm::{init_runtime, name_to_u128, show_term, Runtime};
use im::HashMap;
use proptest::proptest;
use std::{
  collections::hash_map::DefaultHasher,
  hash::{Hash, Hasher},
};

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
fn are_all_elemenets_equal<E: PartialEq>(vec: &[E]) -> bool {
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
fn test_heap_checksum(fn_names: &[&str], rt: &mut Runtime) -> u64 {
  let fn_ids = fn_names.iter().map(|x| name_to_u128(x)).collect::<Vec<u128>>();
  let mut hasher = DefaultHasher::new();
  for fn_id in fn_ids {
    let term_lnk = rt.read_disk(fn_id);
    if let Some(term_lnk) = term_lnk {
      let term_lnk = show_term(rt, term_lnk);

      // dbg!(term_lnk.clone());
      term_lnk.hash(&mut hasher);
    }
  }
  let res = hasher.finish();
  // dbg!(res);
  res
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
fn rollback_simple(
  pre_code: &str,
  code: &str,
  fn_names: &[&str],
  total_tick: u128,
  rollback_tick: u128,
) -> bool {
  let mut rt = init_runtime();

  // Calculate all total_tick states and saves old checksum
  let mut old_state = RuntimeStateTest::new(0, 0, 0);
  rt.run_statements_from_code(pre_code);
  for _ in 0..total_tick {
    rt.run_statements_from_code(code);
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
    rt.run_statements_from_code(pre_code);
  }
  // Run until rollback_tick
  let tick_diff = rollback_tick - rt.get_tick();
  for _ in 0..tick_diff {
    rt.run_statements_from_code(code);
    rt.tick();
  }
  // Calculates new checksum, after rollback
  let new_state =
    RuntimeStateTest::new(test_heap_checksum(&fn_names, &mut rt), rt.get_mana(), rt.get_size());
  dbg!(old_state.clone(), new_state.clone());
  // Returns if checksums are equal
  old_state == new_state
}

// Does basically the same of rollback_simple, but with a path
// This path tells kindelia where to go
// The points_to_test are the points where the state will be saved and compared
fn rollback_path(
  pre_code: &str,
  code: &str,
  fn_names: &[&str],
  path: &[u128],
  points_to_test: &[u128],
) -> bool {
  let mut rt = init_runtime();

  // Calculate all total_tick states and saves old checksum
  rt.run_statements_from_code(pre_code);

  let mut states_store: HashMap<u128, Vec<RuntimeStateTest>> = HashMap::new();
  let mut insert_state_interest = |rt: &mut Runtime| {
    if points_to_test.contains(&rt.get_tick()) {
      let state =
        RuntimeStateTest::new(test_heap_checksum(&fn_names, rt), rt.get_mana(), rt.get_size());
      let vec = states_store.get_mut(&rt.get_tick());
      if let Some(vec) = vec {
        vec.push(state);
      } else {
        states_store.insert(rt.get_tick(), vec![state]);
      }
    }
  };

  for tick in path {
    let tick = *tick;
    if tick < rt.get_tick() {
      rt.rollback(tick);
      if rt.get_tick() == 0 {
        rt.run_statements_from_code(pre_code);
      }
      insert_state_interest(&mut rt);
    }
    if tick >= rt.get_tick() {
      let actual_tick = rt.get_tick();
      for _ in actual_tick..tick {
        rt.run_statements_from_code(code);
        rt.tick();
        // dbg!(test_heap_checksum(&fn_names, &mut rt));
        insert_state_interest(&mut rt);
      }
    } else {
    }
  }

  dbg!(states_store.clone());
  states_store.values().all(|vec| are_all_elemenets_equal(vec))
}


// ===========================================================
// Tests
#[test]
fn simple_rollback() {
  let fn_names = ["Count", "IO.load", "Store", "Sub", "Add"];
  assert!(rollback_simple(PRE_COUNTER, COUNTER, &fn_names, 10000, 1));
}

#[test]
fn advanced_rollback() {
  let fn_names = ["Count", "IO.load", "Store", "Sub", "Add"];
  let path =
    [1000, 500, 12, 500, 12, 500, 12, 500, 12, 500, 12, 500, 12, 500, 12, 500, 12, 500, 12, 500];
  let points_to_test = [1, 12, 37, 500, 950];
  assert!(rollback_path(PRE_COUNTER, COUNTER, &fn_names, &path, &points_to_test));
}

// ===========================================================
// Codes
const PRE_COUNTER: &'static str = "
  $(Succ p)
  $(Zero)
  !(Add n) {
    !(Add n) = $(Succ n)
  } = #0
  
  !(Sub n) {
    !(Sub $(Succ p)) = p
    !(Sub $(Zero)) = $(Zero)
  } = #0
  
  !(Store action) {
    !(Store $(Add)) =
      $(IO.take @l 
      $(IO.save !(Add l) @~
      $(IO.done #0)))
    !(Store $(Sub)) =
      $(IO.take @l 
      $(IO.save !(Sub l) @~
      $(IO.done #0)))
    !(Store $(Get)) = !(IO.load @l $(IO.done l))
  } = $(Zero)
";

const COUNTER: &'static str = "
  {
    $(IO.call 'Count' $(Tuple1 $(Inc #1)) @~
    $(IO.call 'Count' $(Tuple1 $(Get)) @x
    $(IO.done x)))
  }
  {
    $(IO.call 'Store' $(Tuple1 $(Add)) @~
    $(IO.call 'Store' $(Tuple1 $(Get)) @x
    $(IO.done x)))
  }
";

// ===========================================================
// TODO
// fazer funcao de teste (ou modificar a atual) para testar ir e voltar mais de uma vez com o rollback
// colocar testes em outro arquivo DONE
// criar funcao iterativa para substituir a show_term (usando XOR sum) DONE (fiz sem XOR SUM)
// estudar proptest?
// criar contratos que usam: dups de construtores, dups de lambdas,
// salvar lambda em um estado e usá-la, criar árvores, tuplas, qlqr coisa mais complicada