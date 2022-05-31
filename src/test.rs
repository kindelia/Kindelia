use std::{collections::hash_map::DefaultHasher, hash::{Hash, Hasher}};
use proptest::proptest;
use crate::hvm::{Runtime, init_runtime, name_to_u128, show_term};

#[derive(Eq, PartialEq, Debug, Clone)]
struct RuntimeStateTest {
  checksum: u64,
  mana: u128,
  size: i128,
}

impl RuntimeStateTest {
  fn new(checksum: u64, mana: u128, size: i128) -> RuntimeStateTest {
    RuntimeStateTest {
      checksum,
      mana,
      size,
    }
  }
}

// ===========================================================
// Aux functions

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
fn test_runtime_rollback(
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
      old_state = RuntimeStateTest::new(
        test_heap_checksum(&fn_names, &mut rt),
        rt.get_mana(),
        rt.get_size()
      );
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
  let new_state = RuntimeStateTest::new(
    test_heap_checksum(&fn_names, &mut rt),
    rt.get_mana(),
    rt.get_size()
  );
  dbg!(old_state.clone(), new_state.clone());
  // Returns if checksums are equal
  old_state == new_state
}


// ===========================================================
// Tests

// proptest! {
  #[test]
  fn simple_rollback() {
    let pre_code = "
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
    let code = "
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
    let fn_names = ["Count", "IO.load", "Store", "Sub", "Add"];
    // if a > b {
      assert!(test_runtime_rollback(pre_code, code, &fn_names, 10000, 1));
    // } else {
    //   assert!(test_runtime_rollback(pre_code, code, &fn_names, b, a));
    // }
  }
// }

// TODO
// fazer funcao de teste (ou modificar a atual) para testar ir e voltar mais de uma vez com o rollback
// colocar testes em outro arquivo DONE
// criar funcao iterativa para substituir a show_term (usando XOR sum) DONE (fiz sem XOR SUM)
// estudar proptest?
// criar contratos que usam: dups de construtores, dups de lambdas,
//    salvar lambda em um estado e usá-la, criar árvores, tuplas, qlqr coisa mais complicada
