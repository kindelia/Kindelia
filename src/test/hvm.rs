use std::convert::TryInto;
use std::path::PathBuf;

use proptest::prelude::ProptestConfig;
use proptest::proptest;
use proptest::{collection::vec, strategy::Strategy};
use rstest::rstest;
use rstest_reuse::{apply, template};

use crate::common::{Name, U120};
use crate::hvm::{
  self, init_map, read_statements, readback_term, show_term, view_statements,
  view_term, Rollback, Runtime, StatementInfo, Term, Heap
};
use crate::node;
use crate::test::strategies::{func, heap, name, op2, statement, term};
use crate::test::util::{
  self, advance, init_runtime, rollback, rollback_path, rollback_simple,
  run_term_and, run_term_from_code_and, temp_dir, temp_file, test_heap_checksum,
  view_rollback_ticks, RuntimeStateTest, TempPath,
};

#[template]
#[rstest]
#[case(&["Count", "Store", "Sub", "Add"], PRE_COUNTER, COUNTER, &counter_validators())]
#[case(&["Bank", "Random", "AddAcc", "AddEq", "AddChild"], PRE_BANK, BANK, &bank_validators())]
#[case(&["End", "B0", "B1", "IncBit", "ToNum", "CountBit"], PRE_BIT_COUNTER, BIT_COUNTER, &[])]
fn hvm_cases(
  #[case] fn_names: &[&str],
  #[case] pre_code: &str,
  #[case] code: &str,
  #[case] validators: &[util::Validator],
) {
}

#[apply(hvm_cases)]
pub fn simple_rollback(
  fn_names: &[&str],
  pre_code: &str,
  code: &str,
  validators: &[util::Validator],
  temp_dir: TempPath,
) {
  assert!(rollback_simple(
    pre_code,
    code,
    fn_names,
    1000,
    1,
    validators,
    &temp_dir.path
  ));
}


#[apply(hvm_cases)]
pub fn advanced_rollback_in_random_state(
  fn_names: &[&str],
  pre_code: &str,
  code: &str,
  validators: &[util::Validator],
  temp_dir: TempPath,
) {
  let path = [1000, 12, 1000, 24, 1000, 36];
  assert!(rollback_path(
    pre_code,
    code,
    fn_names,
    &path,
    validators,
    &temp_dir.path
  ));
}

#[apply(hvm_cases)]
pub fn advanced_rollback_in_saved_state(
  fn_names: &[&str],
  pre_code: &str,
  code: &str,
  validators: &[util::Validator],
  temp_dir: TempPath,
) {
  let mut rt = init_runtime(&temp_dir.path);
  rt.run_statements_from_code(pre_code, true, true);
  advance(&mut rt, 1000, Some(code), validators);
  rt.rollback(900);
  println!(" - tick: {}", rt.get_tick());
  let s1 = RuntimeStateTest::new(&fn_names, &mut rt);

  advance(&mut rt, 1000, Some(code), validators);
  rt.rollback(900);
  println!(" - tick: {}", rt.get_tick());
  let s2 = RuntimeStateTest::new(&fn_names, &mut rt);

  advance(&mut rt, 1000, Some(code), validators);
  rt.rollback(900);
  println!(" - tick: {}", rt.get_tick());
  let s3 = RuntimeStateTest::new(&fn_names, &mut rt);

  assert_eq!(s1, s2);
  assert_eq!(s2, s3);
}

#[apply(hvm_cases)]
pub fn advanced_rollback_run_fail(
  fn_names: &[&str],
  pre_code: &str,
  code: &str,
  validators: &[util::Validator],
  temp_dir: TempPath,
) {
  let path = [2, 1, 2, 1, 2, 1];
  assert!(rollback_path(
    pre_code,
    code,
    &fn_names,
    &path,
    validators,
    &temp_dir.path
  ));
}

#[apply(hvm_cases)]
pub fn stack_overflow(
  _fn_names: &[&str],
  pre_code: &str,
  code: &str,
  validators: &[util::Validator],
  temp_dir: TempPath,
) {
  // caused by compute_at function
  let mut rt = init_runtime(&temp_dir.path);
  rt.run_statements_from_code(pre_code, true, true);
  advance(&mut rt, 1000, Some(code), validators);
}

#[rstest]
#[ignore = "fix not done"]
// TODO: fix drop stack overflow
pub fn stack_overflow2(temp_dir: TempPath) {
  // caused by drop of term
  let mut rt = init_runtime(&temp_dir.path);
  rt.run_statements_from_code(PRE_COUNTER, false, true);
  rt.run_statements_from_code(COUNTER_STACKOVERFLOW, false, true);
}

#[apply(hvm_cases)]
pub fn persistence1(
  fn_names: &[&str],
  pre_code: &str,
  code: &str,
  validators: &[util::Validator],
  #[values(1000, 1500, 2000)] tick: u128,
  temp_dir: TempPath,
) {
  let mut rt = init_runtime(&temp_dir.path);
  rt.run_statements_from_code(pre_code, true, true);

  advance(&mut rt, tick, Some(code), validators);
  let s1 = RuntimeStateTest::new(&fn_names, &mut rt);

  let last = {
    if let Rollback::Cons { head, .. } = *rt.get_back() {
      rt.get_heap(head).tick
    } else {
      0
    }
  };

  rt.rollback(last); // rollback for the latest rollback saved
  let s2 = RuntimeStateTest::new(&fn_names, &mut rt);

  advance(&mut rt, tick, Some(code), validators);
  let s3 = RuntimeStateTest::new(&fn_names, &mut rt);

  rt.restore_state().expect("Could not restore state"); // restore last rollback, must be equal to s2
  let s4 = RuntimeStateTest::new(&fn_names, &mut rt);

  advance(&mut rt, tick, Some(code), validators);
  let s5 = RuntimeStateTest::new(&fn_names, &mut rt);

  assert_eq!(s1, s3);
  assert_eq!(s2, s4);
  assert_eq!(s3, s5);
}

#[rstest]
fn one_hundred_snapshots(temp_dir: TempPath) {
  // run this with rollback in each 4th snapshot
  // note: this test has no state
  let mut rt = init_runtime(&temp_dir.path);
  for _ in 0..100000 {
    rt.open();
    println!(
      " - tick: {}, - rollback: {}",
      rt.get_tick(),
      view_rollback_ticks(&rt)
    );
    rt.commit();
  }
}

// Statement Indexes
#[rstest]
fn test_simple_idx(temp_dir: TempPath){
  let mut rt = init_runtime(&temp_dir.path.clone());
  rt.open();
  rt.commit();
  rt.open();
  rt.commit();
  rt.open();
  rt.commit();
  rt.open();
  let code = "
    fun (Add a b) {
      (Add #256 #256) = #512
      (Add {True} {True}) = {T2 {True} {True}}
      (Add a b) = {T2 a b}
    }
    fun (B test) {
      (B ~) = #0
    }
    run {
      ask B_idx = (GetIdx 'B');
      (Done B_idx)
    }
  ";
  let results = rt.run_statements_from_code(code, false, true);
  let result_term = results.last().unwrap().clone().unwrap();
  let name = Name::from_str("B").unwrap();
  let idx = rt.get_index(&name).unwrap();
  if let StatementInfo::Run { done_term, .. } = result_term {
    assert_eq!(format!("#{}", idx), view_term(&done_term));
              // (3 << 60) |  1
  } else {
    panic!("Wrong result");
  }
}
#[rstest]
fn test_genesis_idx(temp_dir: TempPath){
  let mut rt = init_runtime(&temp_dir.path.clone());
  let code = "
   run {
     ask T2_idx = (GetIdx 'T2');
     (Done T2_idx)
   }
   ";
  let results = rt.run_statements_from_code(code, false, true);
  let result_term = results.last().unwrap().clone().unwrap();
  if let StatementInfo::Run { done_term, .. } = result_term {
    assert_eq!("#2", view_term(&done_term));
              // (1 << 60) |  1
  } else {
    panic!("Wrong result");
  }

}
#[rstest]
fn test_thousand_idx(temp_dir: TempPath) {
  let mut rt = init_runtime(&temp_dir.path.clone());
  for i in 0..1000 {
    rt.open();
    rt.commit();
  }
  let code = "
   fun (Test x) {
     (Test ~) = #0
   }
   run {
     ask idx = (GetIdx 'Test');
     (Done idx)
   }
   ";
  let results = rt.run_statements_from_code(code, false, true);
  let result_term = results.last().unwrap().clone().unwrap();
  if let StatementInfo::Run { done_term, .. } = result_term {
    assert_eq!("#1152921504606846976000", view_term(&done_term));
              // (1000 << 60) |  1
  } else {
    panic!("Wrong result");
  }
}
  
#[rstest]
fn test_stmt_hash(temp_dir: TempPath){
  let mut rt = init_runtime(&temp_dir.path.clone());
  rt.open();
  let code = "
   fun (Test x) {
     (Test ~) = #0
   }
   run {
     ask idx = (GetIdx 'Test');
     dup id0 id1 = idx;
     ask h0  = (GetStmHash0 id0);
     ask h1  = (GetStmHash1 id1);
     (Done (T2 h0 h1))
   }
   ";
  let results = rt.run_statements_from_code(code, false, true);
  let name = Name::from_str("Test").unwrap();
  let indx = rt.get_index(&name).unwrap();
  let sth0 = rt.get_sth0(indx).unwrap();
  let sth1 = rt.get_sth1(indx).unwrap();
  let result_term = results.last().unwrap().clone().unwrap();
  if let StatementInfo::Run { done_term, .. } = result_term {
    assert_eq!(format!("(T2 #{} #{})", sth0, sth1), view_term(&done_term));
  } else {
    panic!("Wrong result");
  } 
}
#[rstest]
fn test_two_stmt_hash(temp_dir: TempPath){
  let mut rt = init_runtime(&temp_dir.path.clone());
  let code = "
   fun (Test1 x) {
     (Test1 ~) = #0
   }
   fun (Test2 x) {
     (Test2 ~) = #1
   }
   run {
     ask idx_1 = (GetIdx 'Test1');
     dup idx_11 idx_12 = idx_1;
     ask idx_2 = (GetIdx 'Test2');
     dup idx_21 idx_22 = idx_2;
     ask h0 = (GetStmHash0 idx_11);
     ask h1 = (GetStmHash1 idx_12);
     ask h2 = (GetStmHash0 idx_21);
     ask h3 = (GetStmHash1 idx_22);
     (Done (T4 h0 h1 h2 h3))
   }
   ";
  let results = rt.run_statements_from_code(code, false, true);
  let result_term = results.last().unwrap().clone().unwrap();
  let name1 = Name::from_str("Test1").unwrap();
  let indx1 = rt.get_index(&name1).unwrap();
  let name2 = Name::from_str("Test2").unwrap();
  let indx2 = rt.get_index(&name2).unwrap();
  let sth0 = rt.get_sth0(indx1).unwrap();
  let sth1 = rt.get_sth1(indx1).unwrap();
  let sth2 = rt.get_sth0(indx2).unwrap();
  let sth3 = rt.get_sth1(indx2).unwrap();
  if let StatementInfo::Run { done_term, .. } = result_term {
    assert_eq!(format!("(T4 #{} #{} #{} #{})", sth0, sth1, sth2, sth3), view_term(&done_term));
  } else {
    panic!("Wrong result");
  } 
}

#[rstest]
fn test_stmt_hash_after_commit(temp_dir: TempPath){
  let mut rt = init_runtime(&temp_dir.path.clone());
  let code = "
   fun (Test x) {
     (Test ~) = #0
   }
   ";
  let results = rt.run_statements_from_code(code, false, true);
  rt.open();
  rt.commit();
  rt.open();
  rt.commit(); //two ticks just to be safe
  let code = "
    run {
     ask idx = (GetIdx 'Test');
     dup id0 id1 = idx; 
     ask h0  = (GetStmHash0 id0);
     ask h1  = (GetStmHash1 id1);
     (Done (T2 h0 h1))
   }
  ";
  let results = rt.run_statements_from_code(code, false, true);
  let result_term = results.last().unwrap().clone().unwrap();
  let name = Name::from_str("Test").unwrap();
  let indx = rt.get_index(&name).unwrap();
  let sth0 = rt.get_sth0(indx).unwrap();
  let sth1 = rt.get_sth1(indx).unwrap();
  if let StatementInfo::Run { done_term, .. } = result_term {
    assert_eq!(format!("(T2 #{} #{})", sth0, sth1), view_term(&done_term));
  } else {
    panic!("Wrong result");
  } 
}

#[rstest]
fn test_name_sanitizing(temp_dir: TempPath) {
  let mut rt = init_runtime(&temp_dir.path.clone());
  rt.open();
  let code = "
   fun (Test x) {
     (Test ~) = (Done #5)
   }

   run {
     dup n1 n2 = 'Test';
     ask z1 = (Call n1 {T0});
     ask z2 = (Call n2 {T0});
     (Done (T2 z1 z2))
   }
   ";
  let results = rt.run_statements_from_code(code, false, true);
  rt.commit();
  let result_term = results.last().unwrap().clone().unwrap();
  if let StatementInfo::Run { done_term, .. } = result_term {
    assert_eq!(format!("(T2 #5 #5)"), view_term(&done_term));
  } else {
    panic!("Wrong result");
  }
}

#[rstest]
#[case(keyword_fail_1)]
#[case(keyword_fail_2)]
#[case(keyword_fail_3)]
#[should_panic]
fn parse_ask_fail1(
  #[case] template_fn: fn(&str) -> String,
  #[values("ask", "dup", "let")] keyword: &str,
) {
  let code = template_fn(keyword);
  read_statements(&code).unwrap();
}

#[rstest]
fn compute_at_funs(temp_dir: TempPath) {
  let code = "
    fun (Add a b) {
      (Add #256 #256) = #512
      (Add {True} {True}) = {T2 {True} {True}}
      (Add a b) = {T2 a b}
    }
    run {
      (Done dup ~ b = @x @y (Add x y); b)
    }
  ";
  let mut rt = init_runtime(&temp_dir.path);
  let results = rt.run_statements_from_code(code, false, true);
  let result_term = results.last().unwrap().clone().unwrap();
  if let StatementInfo::Run { done_term, .. } = result_term {
    assert_eq!("@x0 @x1 (Add x0 x1)", view_term(&done_term));
  } else {
    panic!("Wrong result");
  }
}

#[rstest]
fn dupped_state_test(temp_dir: TempPath) {
  fn print_and_assert_states(
    rt: &mut Runtime,
    expected_original_readback: &str,
    expected_other_readback: &str,
  ) {
    let original_state =
      rt.read_disk(Name::try_from("Original").unwrap().into()).unwrap();
    let other_state =
      rt.read_disk(Name::try_from("Other").unwrap().into()).unwrap();
    println!();
    println!("original ptr: {}", original_state);
    println!("original: {}", show_term(&rt, original_state, None));
    println!(
      "original readback: {}",
      view_term(&readback_term(&rt, original_state, None).unwrap())
    );
    assert_eq!(
      expected_original_readback,
      view_term(&readback_term(&rt, original_state, None).unwrap())
    );
    println!();
    println!("other ptr: {}", other_state);
    println!("other: {}", show_term(&rt, other_state, None));
    println!(
      "other readback: {}",
      view_term(&readback_term(&rt, other_state, None).unwrap())
    );
    assert_eq!(
      expected_other_readback,
      view_term(&readback_term(&rt, other_state, None).unwrap())
    );
    println!();
  }

  let mut rt = init_runtime(&temp_dir.path);
  rt.run_statements_from_code(&PRE_DUPPED_STATE, false, true);
  rt.run_statements_from_code(&DUPPED_STATE, false, true);
  print_and_assert_states(&mut rt, "@x0 @x1 #7", "@x0 @x1 #7");
  rt.run_statements_from_code(&CHANGE_DUPPED_STATE, false, true);
  print_and_assert_states(&mut rt, "@x0 @x1 #8", "@x0 @x1 #7");
}

#[rstest]
fn shadowing(temp_dir: TempPath) {
  let code = "
    fun (Test state) {
      (Test state) = 
        dup state2 state1 = state;
        let state = state2;
        let got = state1;
        let state = (+ state #1);
        let state = (+ state #1);
        let got = (+ got #1);
        (+ state got)
    }
    run {
      (Done (Test #2))
    }
  ";
  let mut rt = init_runtime(&temp_dir.path);
  let results = rt.run_statements_from_code(code, false, true);
  let result_term = results.last().unwrap().clone().unwrap();
  if let StatementInfo::Run { done_term, .. } = result_term {
    assert_eq!("#7", view_term(&done_term));
  } else {
    panic!("Wrong result");
  }
}

#[rstest]
#[case("@~ dup a ~ = #2; a", "@x0 #2")]
#[case("@~ {Cons #4 {Nil}}", "@x0 {Cons #4 {Nil}}")]
#[case("dup a ~ = (! @x @y {Pair (+ x #1) y} #2); (!a #10)", "{Pair #3 #10}")]
#[case(
  "@~ dup x ~ = {Cons (+ #1 #1) {Cons (+ #2 #2) {Cons (+ #3 #3) {Nil}}}}; x",
  "@x0 {Cons (+ #1 #1) {Cons (+ #2 #2) {Cons (+ #3 #3) {Nil}}}}"
)]
#[case(
  "dup x ~ = {Cons (+ #1 #1) {Cons (+ #2 #2) {Cons (+ #3 #3) {Nil}}}}; x",
  "{Cons #2 {Cons #4 {Cons #6 {Nil}}}}"
)]
#[case(
  "dup a b = @x @y {Pair x y}; {Pair a b}",
  "{Pair @x1 @x2 {Pair x1 x2} @x1 @x2 {Pair x1 x2}}"
)]
#[case(
  "dup a b = (! @x @y {Pair (+ x #1) y} #2); {Pair (!a #10) (!b #20)}",
  "{Pair ((@x1 @x2 {Pair (+ x1 #1) x2} #2) #10) ((@x1 @x2 {Pair (+ x1 #1) x2} #2) #20)}"
)]
#[case("dup a ~ = @~ #2; a", "@x0 #2")]
#[case("dup a ~ = @x (!x #4); a", "@x0 (x0 #4)")]
#[case("dup a ~ = @x dup b ~ = x; b; a", "@x0 x0")]
#[case("dup a ~ = @x dup ~ b = x; b; a", "@x0 x0")]
#[case("dup a ~ = dup b ~ = @x (+ x #2); b; a", "@x0 (+ x0 #2)")]
#[case("dup a ~ = dup b ~ = @x (+ #2 x); b; a", "@x0 (+ #2 x0)")]
#[case("let state = #2; let state = (+ state #1); state", "#3")]
fn readback(
  #[case] code: &str,
  #[case] expected_readback: &str,
  temp_dir: TempPath,
) {
  // initialize runtime
  let mut rt = init_runtime(&temp_dir.path);
  // declare used constructors
  let pre_code = "ctr {Cons x xs} ctr {Nil} ctr {Pair x y}";
  rt.run_statements_from_code(&pre_code, false, true);
  // run code
  let code = format!("run{{ (Done {}) }}", code); // always run one statement
  let result = rt.run_statements_from_code(&code, false, true); // get vector of results
  let result = result[0].clone(); // get first and only result
  let result = result.unwrap(); // expect result not to be an error (fails test if it is)

  // verify readback
  if let StatementInfo::Run { done_term, .. } = result {
    let readback = view_term(&done_term);
    assert_eq!(expected_readback, readback);
  } else {
    panic!("Expected Run statement, got {:?}", result);
  }
}

proptest! {
  #[test]
  fn name_conversion(name in name()) {
    let a = name.to_string();
    let b = Name::from_str(&a).unwrap();
    let c = b.to_string();
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
    let h1 = heap;
    let path = temp_dir();
    h1.serialize(&path.path, false).unwrap();
    if let Ok(h2) = Heap::deserialize(h1.uuid, &path.path) {
        assert_eq!(h1, h2);
    }
    else {
        panic!("Could not deserialize")
    }
  }
}

// ===========================================================
// Operations

proptest! {
  #![proptest_config(ProptestConfig::with_cases(1000))]
  #[test]
  fn operators_dont_panic(op in op2(0..16)) {
    // this test also ensures that all op2 dont panic
    // and that their results are numbers
    run_term_and(&op, |res| {
      match res {
        Term::Num {..} => (),
        _ => panic!("Not a number: {}", view_term(res))
      }
    });
  }

  #[test]
  fn boolean_operators(op in op2(10..16)) {
    // verifies if boolean operators always returns
    // 0 or 1
    run_term_and(&op, |res| {
      match res {
        Term::Num {numb} => assert!(**numb == 0 || **numb == 1),
        _ => panic!("Not a number: {}", view_term(res))
      }
    });
  }

  #[test]
  #[should_panic]
  fn invalid_operation(op in op2(16_u128..u128::MAX)) {
    run_term_and(&op, |res| {});
  }
}

#[rstest]
#[case("(+ #1 #2)", 3)]
#[case("(- #2 #1)", 1)]
#[case("(- #100 #1)", 99)]
#[case("(* #1 #2)", 2)]
#[case("(/ #5 #3)", 1)]
#[case("(% #5 #3)", 2)]
#[case("(<< #1 #120)", 1)]
#[case("(- (<< #1 #120) #1)", 0)]
#[case(&format!("(> #{} #0)", U120::MAX), 1)]
#[case("(> (- #0 #1) #0)", 1)]
#[case("(- #0 #1)", *U120::MAX)]
#[case("(<< #1 #12000)", 1)]
#[case("(<< (<< #1 #60) #60)", 0)]
#[case("(<< #1 #120)", 1)]
#[case("(* (<< #1 #60) #7)", 1u128.wrapping_shl(60) * 7)]
#[case("(+ (<< #1 #119) (<< #1 #119))", 0)]
#[case("(* (<< #1 #119) #2)", 0)]
#[case("(% (<< #1 #119) #27)", 23)]
#[case("(/ (<< #1 #119) #4)", 1u128.wrapping_shl(117))]
#[case(&format!("(* #{} #{})", U120::MAX, U120::MAX), 1)]
#[case(&format!("(< (+ #{} #1) #1)", U120::MAX), 1)]
#[case(&format!("(* #{} #2)", U120::MAX), *U120::MAX - 1)]
#[case(&format!("(| #{} #1)", U120::MAX), *U120::MAX)]
#[case(&format!("(& #{} #1)", U120::MAX), 1)]
#[case(&format!("(^ #{} #1)", U120::MAX), *U120::MAX - 1)]
#[case(&format!("(>> #{} #119)", U120::MAX), 1)]
#[case(&format!("(>> #{} #118)", U120::MAX), 3)]
#[case(&format!("(+ #{} #1)", U120::MAX), *U120::ZERO)]
#[case(&format!("(+ #{} #123456)", U120::MAX), 123455)]
fn operators_cases(#[case] code: &str, #[case] expected: u128) {
  run_term_from_code_and(code, |res| match res {
    Term::Num { numb } => assert_eq!(**numb, expected),
    _ => panic!("Not a number as result: {}", view_term(res)),
  })
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
    ask (Call 'Store' {StoreAdd});
    ask count = (Call 'Store' {StoreGet});
    (Done count)
  }

  run {
    ask (Call 'Count' {Inc});
    ask count = (Call 'Count' {Get});
    (Done count)
  }
";

fn counter_validators() -> [util::Validator; 2] {
  fn count_validator(tick: u128, term: &Term, _: &mut Runtime) -> bool {
    let counter = tick;
    assert_eq!(view_term(term), format!("#{}", counter));
    view_term(term) == format!("#{}", counter)
  }
  fn store_validator(tick: u128, term: &Term, _: &mut Runtime) -> bool {
    let counter = tick;
    view_term(term).matches("Succ").count() as u128 == counter
  }
  [("Count", count_validator), ("Store", store_validator)]
}

pub const SIMPLE_COUNT: &'static str = "
  run {
    ask (Call 'Count' {Inc});
    ask count = (Call 'Count' {Get});
    (Done count)
  }
";

pub const COUNTER_STACKOVERFLOW: &'static str = "
  run {
    (Done (ToSucc #8000))
  }
";

pub const PRE_BANK: &'static str = "
ctr {Node k v l r}
ctr {Leaf}

fun (AddEq cond key t) {
  (AddEq #1 ~ {Node k v l r}) = {Node k (+ v #1) l r}
  (AddEq #0 key {Node k v l r}) = 
    dup k.0 k.1 = k;
    dup key.0 key.1 = key;
    (AddChild (> key.0 k.0) key.1 {Node k.1 v l r})
} 

fun (AddChild cond key t) {
  (AddChild #1 key {Node k v l r}) = {Node k v l (AddAcc key r)}
  (AddChild #0 key {Node k v l r}) = {Node k v (AddAcc key l) r}
} 

fun (AddAcc key t) {
  (AddAcc key {Leaf}) = {Node key #1 {Leaf} {Leaf}}
  (AddAcc key {Node k v lft rgt}) =
    dup k.0 k.1 = k;
    dup key.0 key.1 = key;
    (AddEq (== k.0 key.0) key.1 {Node k.1 v lft rgt})
}

ctr {Random_Inc}
ctr {Random_Get}

fun (Random action) {
  (Random {Random_Inc}) = 
    ask x = (Take);
    ask (Save (% (+ (* #25214903917 x) #11) #281474976710656));
    (Done #0)
  (Random {Random_Get}) = 
    ask x = (Load);
    (Done x)
} with {
  #1
}

ctr {Bank_Add acc}
ctr {Bank_Get}

fun (Bank action) {
  (Bank {Bank_Add acc}) = 
    ask t = (Take);
    ask (Save (AddAcc acc t));
    (Done #0)
  (Bank {Bank_Get}) = 
    ask x = (Load);
    (Done x)
} with {
  {Leaf}
}
";

pub const BANK: &'static str = "
  run {
    ask (Call 'Random' {Random_Inc});
    ask acc = (Call 'Random' {Random_Get});
    ask (Call 'Bank' {Bank_Add acc});
    ask b = (Call 'Bank' {Bank_Get});
    (Done b)
    // !done (AddAcc #1 {Leaf})
  }
";

fn bank_validators() -> [util::Validator; 1] {
  fn tree_validator(tick: u128, term: &Term, _: &mut Runtime) -> bool {
    let counter = tick;
    view_term(term).matches("Node").count() as u128 == counter
  }
  [("Bank", tree_validator)]
}

pub const PRE_BIT_COUNTER: &'static str = "
// The Scott-Encoded Bits type
fun (End) {
  (End) = @e @~ @~ e 
}

fun (B0 p) {
  (B0 p) = @~ @o @~ (!o p)
}

fun (B1 p) {
  (B1 p) = @~ @~ @i (!i p)
}

fun (IncBit xs) {
  (IncBit xs) = @ex @ox @ix
    let e = ex;
    let o = ix;
    let i = @p (!ox (IncBit p));
    (!(!(!xs e) o) i)
}

fun (ToNum ys) {
  (ToNum ys) =
    let e = #0;
    let o = @p (+ #0 (* #2 (ToNum p)));
    let i = @p (+ #1 (* #2 (ToNum p)));
    (!(!(!ys e) o) i)
}

fun (FromNum s i) {
  (FromNum #0 ~) = (End)
  (FromNum s i) = dup i0 i1 = i; (FromNumPut (- s #1) (% i0 #2) (/ i1 #2))
}

fun (FromNumPut s b i) {
  (FromNumPut s #0 i) = (B0 (FromNum s i))
  (FromNumPut s #1 i) = (B1 (FromNum s i))
}

fun (CountBit action) {
  (CountBit {Inc}) =  
    ask x = (Take);
    ask (Save (IncBit x));
    (Done #0)
  (CountBit {Get}) = 
    ask x = (Load);
    (Done x)
} with {
  (FromNum #32 #1)
}
";

pub const BIT_COUNTER: &'static str = "
run {
  ask (Call 'CountBit' {Inc});
  ask x = (Call 'CountBit' {Get});
  (Done (ToNum x))
}
";

// TODO: this code generate stack overflow on the parser
// fn bit_validators() -> [util::Validator; 1] {
//   fn bit_validator(tick: u128, term: &Term, rt: &mut Runtime) -> bool {
//     let term = view_term(term);
//     let code = format!(" run {{ (Done (ToNum {})) }} ", term);
//     let res = rt
//       .run_statements_from_code(&code, true, true)
//       .first()
//       .unwrap()
//       .clone()
//       .unwrap();
//     if let hvm::StatementInfo::Run { done_term, .. } = res {
//       view_term(&done_term) == format!("#{}", tick)
//     } else {
//       panic!();
//     }
//   }
//   [("CountBit", bit_validator)]
// }

pub fn keyword_fail_1(keyword: &str) -> String {
  format!(
    "
    fun (Test c) {{
      (Test {{Aa {} aa}}) = dup x y = #2; (+ x y)
    }} with {{
      (S84_dKIY_)
    }} sign {{
      a0389bb267d0cebd9190b74c65
      33acf1c57b4cdb5166f202edfd
      d52f06c3f4e560d01e3ced971a
      54f3b3b47133daa1befe226a77
      48afa13c8b2d3182382ee2fde8
    }}
  ",
    keyword
  )
}

pub fn keyword_fail_2(keyword: &str) -> String {
  format!(
    "
    fun (Test) {{
      (Test) = dup {} y = #2; (+ {} y)
    }}
  ",
    keyword, keyword
  )
}

pub fn keyword_fail_3(keyword: &str) -> String {
  format!(
    "
    fun (Test {}) {{
      (Test {}) = dup x y = {}; (+ x y)
    }}
  ",
    keyword, keyword, keyword
  )
}

const PRE_DUPPED_STATE: &'static str = "
ctr {Copy}
ctr {Change}

fun (Original action) {
  (Original {Copy}) =
    ask x = (Take);
    dup a b = x;
    ask (Save a);
    (Done b)
  (Original {Change}) = 
    ask (Take);
    ask (Save @~ @~ #8);
    (Done #0)
} with {
  @~ @~ #7
}

fun (Other) {
  (Other) =
    ask x = (Call 'Original' {Copy});
    ask (Save x);
    (Done #0)
}
";

const DUPPED_STATE: &'static str = "
run {
  ask (Call 'Other' []);
  (Done #0)
}
";

const CHANGE_DUPPED_STATE: &'static str = "
run {
  ask (Call 'Original' {Change});
  (Done #1)
}
";
