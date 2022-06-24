use crate::{
  crypto,
  hvm::{name_to_u128, Rule, Statement, Term},
};
use proptest::{arbitrary::any, collection::vec, option, prop_oneof, strategy::Strategy};

// generate valid names
pub fn name() -> impl Strategy<Value = u128> {
  "[a-zA-Z0-9_][a-zA-Z0-9_]{1,19}".prop_map(|s| name_to_u128(&s))
}

// generate valid terms
pub fn term() -> impl Strategy<Value = Term> {
  let leaf = prop_oneof![
    name().prop_map(|n| Term::Var { name: n }),
    name().prop_map(|n| Term::Num { numb: n }),
  ];

  leaf.prop_recursive(
    16,  // 16 levels deep
    256, // Shoot for maximum size of 256 nodes
    10,  // We put up to 10 items per collection
    |inner| {
      prop_oneof![
        (name(), name(), inner.clone(), inner.clone()).prop_map(|(n0, n1, e, b)| {
          Term::Dup { nam0: n0, nam1: n1, expr: Box::new(e), body: Box::new(b) }
        }),
        (name(), inner.clone()).prop_map(|(n, e)| { Term::Lam { name: n, body: Box::new(e) } }),
        (inner.clone(), inner.clone())
          .prop_map(|(f, a)| { Term::App { func: Box::new(f), argm: Box::new(a) } }),
        (name(), vec(inner.clone(), 0..10)).prop_map(|(n, v)| { Term::Ctr { name: n, args: v } }),
        (name(), vec(inner.clone(), 0..10)).prop_map(|(n, v)| { Term::Fun { name: n, args: v } }),
        (0..15_u128, inner.clone(), inner).prop_map(|(o, v0, v1)| {
          Term::Op2 { oper: o, val0: Box::new(v0), val1: Box::new(v1) }
        }),
      ]
    },
  )
}

// generate rules
pub fn rule() -> impl Strategy<Value = Rule> {
  (term(), term()).prop_map(|(lhs, rhs)| Rule { lhs, rhs })
}

// generate signatures
pub fn sign() -> impl Strategy<Value = crypto::Signature> {
  (vec(any::<u8>(), 65)).prop_map(|s| crypto::Signature(s.try_into().unwrap()))
}

// generate statements
pub fn statement() -> impl Strategy<Value = Statement> {
  prop_oneof![
    (name(), vec(name(), 0..10), vec(rule(), 0..10), term(), option::of(sign())).prop_map(
      |(n, a, r, i, s)| { Statement::Fun { name: n, args: a, func: r, init: i, sign: s } }
    ),
    (name(), vec(name(), 0..10), option::of(sign()))
      .prop_map(|(n, a, s)| { Statement::Ctr { name: n, args: a, sign: s } }),
    (term(), option::of(sign())).prop_map(|(t, s)| { Statement::Run { expr: t, sign: s } }),
    (name(), name(), option::of(sign()))
      .prop_map(|(n, o, s)| { Statement::Reg { name: n, ownr: o, sign: s } }),
  ]
}
