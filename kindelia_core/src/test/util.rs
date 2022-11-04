use std::collections::{hash_map::DefaultHasher, HashMap};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::sync::Arc;

use rstest::fixture;

use crate::constants;
use crate::common::{Name, U120};
use crate::hvm::{
  self, read_term, show_term, Rollback, Runtime, Statement, StatementInfo,
  Term, U128_NONE, U64_NONE,
};
use crate::node;

pub fn init_runtime(path: &PathBuf) -> hvm::Runtime {
  let genesis_stmts =
    hvm::parse_code(constants::GENESIS_CODE).expect("Genesis code parses.");
  hvm::init_runtime(path.clone(), &genesis_stmts)
}

// ===========================================================
// Aux types

pub type Validator =
  (&'static str, fn(u64, &hvm::Term, &mut hvm::Runtime) -> bool);

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
  fn view_rollback_ticks_go(
    rt: &Runtime,
    back: &Arc<Rollback>,
  ) -> Vec<Option<u64>> {
    match &**back {
      Rollback::Nil => return Vec::new(),
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
  let elems = ticks
    .iter()
    .rev()
    .map(|x| {
      if let Some(x) = x {
        format!("{}", if *x != U64_NONE { *x } else { 0 })
      } else {
        "___________".to_string()
      }
    })
    .collect::<Vec<String>>()
    .join(", ");
  return format!("[{}]", elems);
}

// ===========================================================
// HEAP STATE

// Struct used to store interesting parts of runtime state
#[derive(Eq, PartialEq, Debug, Clone)]
pub struct RuntimeStateTest {
  checksum: u64,
  mana: u64,
  size: u64,
}
impl RuntimeStateTest {
  pub fn new(fn_names: &[&str], rt: &mut Runtime) -> RuntimeStateTest {
    RuntimeStateTest {
      checksum: test_heap_checksum(&fn_names, rt),
      mana: rt.get_mana(),
      size: rt.get_size(),
    }
  }
}

// Generate a checksum for a runtime state (for testing)
pub fn test_heap_checksum(fn_names: &[&str], rt: &mut Runtime) -> u64 {
  let fn_ids =
    fn_names.iter().map(|x| Name::from_str(x).unwrap()).collect::<Vec<Name>>();
  let mut hasher = DefaultHasher::new();
  for fn_id in fn_ids {
    let term_lnk = rt.read_disk(fn_id.into());
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

// ===========================================================
// RUNTIME ROLLBACK
pub fn rollback(
  rt: &mut Runtime,
  tick: u64,
  pre_code: Option<&str>,
  code: Option<&str>,
  validators: &[Validator],
) {
  debug_assert!(tick < rt.get_tick());
  rt.rollback(tick);
  if rt.get_tick() == 0 {
    if let Some(pre_code) = pre_code {
      rt.run_statements_from_code(pre_code, true, true);
    }
  }
  let tick_diff = tick - rt.get_tick();
  for _ in 0..tick_diff {
    if let Some(code) = code {
      rt.run_statements_from_code(code, true, true);
    }
    validate_and_tick(rt, validators);
  }
  // println!("- final rollback tick {}", rt.get_tick());
}

pub fn advance(
  rt: &mut Runtime,
  tick: u64,
  code: Option<&str>,
  validators: &[Validator],
) {
  debug_assert!(tick >= rt.get_tick());
  // eprintln!("- advancing from {} to {}", rt.get_tick(), tick);
  let current_tick = rt.get_tick();
  for _ in current_tick..tick {
    if let Some(code) = code {
      rt.run_statements_from_code(code, true, true);
    }
    validate_and_tick(rt, validators);
  }
}

/// Tests the rollback of states in the Kindelia runtime
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
  rollback_tick: u64,
  validators: &[Validator],
  dir_path: &PathBuf,
) -> bool {
  let mut rt = init_runtime(dir_path);

  // Calculate all total_tick states and saves old checksum
  let mut old_state = RuntimeStateTest::new(fn_names, &mut rt);
  rt.run_statements_from_code(pre_code, true, true);
  for _ in 0..total_tick {
    rt.run_statements_from_code(code, true, true);
    validate_and_tick(&mut rt, validators);
    // dbg!(test_heap_checksum(&fn_names, &mut rt));
    if rt.get_tick() == rollback_tick {
      old_state = RuntimeStateTest::new(fn_names, &mut rt);
    }
  }
  // Does rollback to nearest rollback_tick saved state
  rt.rollback(rollback_tick);
  if rt.get_tick() == 0 {
    rt.run_statements_from_code(pre_code, true, true);
  }
  // Run until rollback_tick
  let tick_diff = rollback_tick - rt.get_tick();
  for _ in 0..tick_diff {
    rt.run_statements_from_code(code, true, true);
    validate_and_tick(&mut rt, validators);
  }
  // Calculates new checksum, after rollback
  let new_state = RuntimeStateTest::new(fn_names, &mut rt);
  // dbg!(old_state.clone(), new_state.clone());
  // Returns if checksums are equal
  old_state == new_state
}

// Does basically the same of rollback_simple, but with a path
// This path tells Kindelia where to go
pub fn rollback_path(
  pre_code: &str,
  code: &str,
  fn_names: &[&str],
  path: &[u64],
  validators: &[Validator],
  dir_path: &PathBuf,
) -> bool {
  let mut states_store: HashMap<u64, Vec<RuntimeStateTest>> = HashMap::new();
  let mut insert_state = |rt: &mut Runtime| {
    let state = RuntimeStateTest::new(fn_names, rt);
    let vec = states_store.get_mut(&rt.get_tick());
    if let Some(vec) = vec {
      vec.push(state);
    } else {
      states_store.insert(rt.get_tick(), vec![state]);
    }
  };

  let mut rt = init_runtime(dir_path);
  rt.run_statements_from_code(pre_code, true, true);

  for tick in path {
    let tick = *tick;
    if tick < rt.get_tick() {
      rollback(&mut rt, tick, Some(pre_code), Some(code), validators);
    } else {
      advance(&mut rt, tick, Some(code), validators);
    }
    insert_state(&mut rt);
  }

  // dbg!(states_store.clone());
  // Verify if all values from all vectors from all ticks of interest are equal
  states_store.values().all(|vec| are_all_elemenets_equal(vec))
}

pub fn run_term_and<A>(term: &Term, action: A)
where
  A: Fn(&Term),
{
  let temp_dir = temp_dir();
  let mut rt = init_runtime(&temp_dir.path);

  let term = Term::Fun {
    name: "Done".try_into().unwrap(),
    args: [term.clone()].to_vec(),
  };
  let stmt = Statement::Run { expr: term, sign: None };
  let result = rt.run_statement(&stmt, false, true, None).unwrap();

  if let StatementInfo::Run { done_term, .. } = result {
    action(&done_term)
  }
}

pub fn run_term_from_code_and<A>(code: &str, action: A)
where
  A: Fn(&Term),
{
  let (_, term) = read_term(code).unwrap();
  run_term_and(&term, action)
}

// ===========
// validations

fn validate<P: Fn(u64, &hvm::Term, &mut hvm::Runtime) -> bool>(
  rt: &mut Runtime,
  name: &str,
  predicate: P,
) {
  let tick = rt.get_tick();
  let name = Name::from_str(name).unwrap();
  let name = U120::from(name);
  let state = rt.read_disk(name).unwrap();
  let state = hvm::readback_term(rt, state, None).unwrap();
  assert!(predicate(tick, &state, rt))
}

fn validate_and_tick(rt: &mut Runtime, validators: &[Validator]) {
  rt.open();
  eprintln!("tick: {}", rt.get_tick());
  for (fn_name, predicate) in validators {
    validate(rt, fn_name, predicate)
  }
  rt.commit();
}

// ===========================================================
// BEFORE EACH

// This struct is created just to wrap Pathbuf and
// be able to remove the dir when it is dropped
pub struct TempPath {
  pub path: PathBuf,
}

impl Drop for TempPath {
  fn drop(&mut self) {
    if self.path.is_file() {
      if let Err(e) = std::fs::remove_file(&self.path) {
        eprintln!("Error removing temp file {}: {}", self.path.display(), e);
      };
      if let Some(parent) = self.path.parent() {
        if let Err(e) = std::fs::remove_dir_all(&parent) {
          eprintln!(
            "Error removing temp dir of file {}: {}",
            self.path.display(),
            e
          );
        }
      }
    } else {
      if let Err(e) = std::fs::remove_dir_all(&self.path) {
        eprintln!("Error removing temp dir {}: {}", self.path.display(), e);
      }
    }
  }
}

// fixture from rstest library
/// Creates a temporary dir and returns a TempDir struct
/// before each test that uses it as a parameter
#[fixture]
pub fn temp_dir() -> TempPath {
  let path =
    std::env::temp_dir().join(format!("kindelia.{:x}", fastrand::u128(..)));
  let temp_dir = TempPath { path };
  temp_dir
}

// fixture from rstest library
/// Creates a temporary `.txt` file and returns a TempDir struct
/// before each test that uses it as a parameter
#[fixture]
pub fn temp_file() -> TempPath {
  let path = std::env::temp_dir()
    .join(format!("kindelia.{:x}", fastrand::u128(..)))
    .join(format!("kindelia.{:x}.txt", fastrand::u128(..)));
  std::fs::create_dir_all(&path.parent().unwrap()).unwrap();
  std::fs::write(&path, "").unwrap();
  let temp_file = TempPath { path };
  temp_file
}
