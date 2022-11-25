#![warn(dead_code)]
#![warn(non_snake_case)]
#![warn(unused_variables)]
#![warn(clippy::style)]
#![warn(clippy::identity_op)]

use std::{collections::{HashMap, hash_map}, fmt::Write};

use kindelia_common::Name;
use kindelia_lang::ast::Oper;

use crate::runtime::{Loc, CellTag, ask_arg, ask_lnk};

use super::{Runtime, RawCell};

pub fn show_rt(rt: &Runtime) -> String {
  let mut s: String = String::new();
  for i in 0..32 {
    write!(s, "{:x} | ", i).unwrap();
    let cell_str = &rt.read(Loc::from_u64_unchecked(i)).to_string();
    s.push_str(cell_str);
    s.push('\n');
  }
  s
}

pub fn show_memo(rt: &Runtime) -> String {
  let mut txt = String::new();
  for i in 0..rt.get_mcap() {
    txt.push(if rt.read(Loc::from_u64_unchecked(i)) == RawCell::ZERO { '_' } else { 'X' });
  }
  txt
}

pub fn show_term(rt: &Runtime, term: RawCell, focus: Option<RawCell>) -> String {
  enum StackItem {
    Term(RawCell),
    Str(String),
  }
  let mut names: HashMap<Loc, String> = HashMap::new();
  fn find_lets(
    rt: &Runtime,
    term: RawCell,
    names: &mut HashMap<Loc, String>,
    focus: Option<RawCell>,
  ) -> String {
    let mut lets: HashMap<Loc, Loc> = HashMap::new();
    let mut kinds: HashMap<Loc, u128> = HashMap::new();
    let mut count: u128 = 0;
    let mut stack = vec![term];
    let mut text = String::new();
    while !stack.is_empty() {
      let term = stack.pop().unwrap();
      match term.get_tag() {
        CellTag::LAM => {
          names.insert(term.get_loc(0), format!("{}", count));
          count += 1;
          stack.push(ask_arg(rt, term, 1));
        }
        CellTag::APP => {
          stack.push(ask_arg(rt, term, 1));
          stack.push(ask_arg(rt, term, 0));
        }
        CellTag::SUP => {
          stack.push(ask_arg(rt, term, 1));
          stack.push(ask_arg(rt, term, 0));
        }
        CellTag::DP0 => {
          if let hash_map::Entry::Vacant(e) = lets.entry(term.get_loc(0)) {
            names.insert(term.get_loc(0), format!("{}", count));
            count += 1;
            kinds.insert(term.get_loc(0), term.get_ext());
            e.insert(term.get_loc(0));
            stack.push(ask_arg(rt, term, 2));
          }
        }
        CellTag::DP1 => {
          if let hash_map::Entry::Vacant(e) = lets.entry(term.get_loc(0)) {
            names.insert(term.get_loc(0), format!("{}", count));
            count += 1;
            kinds.insert(term.get_loc(0), term.get_ext());
            e.insert(term.get_loc(0));
            stack.push(ask_arg(rt, term, 2));
          }
        }
        CellTag::OP2 => {
          stack.push(ask_arg(rt, term, 1));
          stack.push(ask_arg(rt, term, 0));
        }
        CellTag::CTR | CellTag::FUN => {
          let name = term.get_name_from_ext();
          let arity = rt.get_arity(&name).unwrap();
          // NOTE: arity should never be None (read from memory), should panic
          // TODO: remove unwrap?
          for i in (0..arity).rev() {
            stack.push(ask_arg(rt, term, i));
          }
        }
        _ => {}
      }
    }

    for (_key, pos) in lets {
      // todo: reverse
      let what = String::from("?h");
      //let kind = kinds.get(&key).unwrap_or(&0);
      let name = names.get(&pos).unwrap_or(&what);
      let nam0 = if ask_lnk(rt, pos + 0) == RawCell::era() { String::from("*") } else { format!("a{}", name) };
      let nam1 = if ask_lnk(rt, pos + 1) == RawCell::era() { String::from("*") } else { format!("b{}", name) };
      write!(text, "dup {} {} = {}; ", nam0, nam1, go(rt, ask_lnk(rt, pos + 2), names, focus)).unwrap();
    }
    text
  }

  fn go(rt: &Runtime, term: RawCell, names: &HashMap<Loc, String>, focus: Option<RawCell>) -> String {
    let mut stack = vec![StackItem::Term(term)];
    let mut output = Vec::new();
    while !stack.is_empty() {
      let item = stack.pop().unwrap();
      match item {
        StackItem::Str(txt) => {
          output.push(txt);
        },
        StackItem::Term(term) => {
          if let Some(focus) = focus {
            if focus == term {
              output.push("$".to_string());
            }
          }
          match term.get_tag() {
            CellTag::DP0 => {
              output.push(format!("a{}", names.get(&term.get_loc(0)).unwrap_or(&String::from("?a"))));
            }
            CellTag::DP1 => {
              output.push(format!("b{}", names.get(&term.get_loc(0)).unwrap_or(&String::from("?b"))));
            }
            CellTag::VAR => {
              output.push(format!("x{}", names.get(&term.get_loc(0)).unwrap_or(&String::from("?c"))));
            }
            CellTag::LAM => {
              let name = format!("x{}", names.get(&term.get_loc(0)).unwrap_or(&String::from("?")));
              output.push(format!("@{} ", name));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
            }
            CellTag::APP => {
              output.push("(!".to_string());
              stack.push(StackItem::Str(")".to_string()));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
              stack.push(StackItem::Str(" ".to_string()));
              stack.push(StackItem::Term(ask_arg(rt, term, 0)));
            }
            CellTag::SUP => {
              output.push("{".to_string());
              stack.push(StackItem::Str("}".to_string()));
              //let kind = term.get_ext();
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
              stack.push(StackItem::Str(" ".to_string()));
              stack.push(StackItem::Term(ask_arg(rt, term, 0)));
            }
            CellTag::OP2 => {
              let oper = term.get_ext().try_into().unwrap();
              let symb = match oper {
                Oper::Add => "+",
                Oper::Sub => "-",
                Oper::Mul => "*",
                Oper::Div => "/",
                Oper::Mod => "%",
                Oper::And => "&",
                Oper::Or  => "|",
                Oper::Xor => "^",
                Oper::Shl => "<<",
                Oper::Shr => ">>",
                Oper::Ltn => "<",
                Oper::Lte => "<=",
                Oper::Eql => "=",
                Oper::Gte => ">=",
                Oper::Gtn => ">",
                Oper::Neq => "!=",
              };
              output.push(format!("({} ", symb));
              stack.push(StackItem::Str(")".to_string()));
              stack.push(StackItem::Term(ask_arg(rt, term, 1)));
              stack.push(StackItem::Str(" ".to_string()));
              stack.push(StackItem::Term(ask_arg(rt, term, 0)));
            }
            CellTag::NUM => {
              let numb = term.get_num();
              output.push(format!("#{}", numb));
            }
            CellTag::CTR => {
              let name = term.get_name_from_ext();
              let mut arit = rt.get_arity(&name).unwrap();
              // NOTE: arity should never be zero (read from memory)
              // TODO: remove unwrap
              let mut name = name.to_string();
              // Pretty print names
              if name == "Name" && arit == 1 {
                let arg = ask_arg(rt, term, 0);
                if arg.get_tag() == CellTag::NUM {
                  let sugar: Name = arg.get_num().into();
                  name = format!("Name '{}'", sugar);
                  arit = 0; // erase arit to avoid for
                }
              }
              output.push(format!("{{{}", name));
              stack.push(StackItem::Str("}".to_string()));

              for i in (0..arit).rev() {
                stack.push(StackItem::Term(ask_arg(rt, term, i)));
                stack.push(StackItem::Str(" ".to_string()));

              }
            }
            CellTag::FUN => {
              let name = term.get_name_from_ext();
              output.push(format!("({}", name));
              stack.push(StackItem::Str(")".to_string()));
              let arit = rt.get_arity(&name).unwrap();
              for i in (0..arit).rev() {
                stack.push(StackItem::Term(ask_arg(rt, term, i)));
                stack.push(StackItem::Str(" ".to_string()));
              }
            }
            CellTag::ERA => {
              output.push(String::from("*"));
            }
            _ => {
              // println!("{}", show_ptr(term));
              // println!("{}", show_term(rt,  ask_lnk(rt, term), None));
              output.push(format!("?g({})", term.get_tag() as u128))
            }
          }
        }
      }
    }
    output.join("")
  }

  let mut text = find_lets(rt, term, &mut names, focus);
  text.push_str(&go(rt, term, &names, focus));
  text
}
