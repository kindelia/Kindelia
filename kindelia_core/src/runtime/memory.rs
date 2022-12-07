//! Kindelia-HVM's memory model
//! ---------------------------
//!
//! The runtime memory consists of just a vector of u128 pointers. That is:
//!
//! ```text
//! Mem ::= Vec<Ptr>
//! ```
//!
//! A pointer has 3 parts:
//!
//! ```text
//! Ptr ::= TT AAAAAAAAAAAAAAAAAA BBBBBBBBBBBB
//! ```
//!
//! Where:
//!
//! ```text
//! T : u8  is the pointer tag
//! A : u72 is the 1st value
//! B : u48 is the 2nd value
//! ```
//!
//! There are 12 possible tags:
//!
//!   Tag | Val | Meaning
//!   ----| --- | ------------------------------------------------------
//!   DP0 |   0 | a variable, bound to the 1st argument of a duplication
//!   DP1 |   1 | a variable, bound to the 2nd argument of a duplication
//!   VAR |   2 | a variable, bound to the one argument of a lambda
//!   ARG |   3 | an used argument of a lambda or duplication
//!   ERA |   4 | an erased argument of a lambda or duplication
//!   LAM |   5 | a lambda
//!   APP |   6 | an application
//!   SUP |   7 | a superposition
//!   CTR |   8 | a constructor
//!   FUN |   9 | a function
//!   OP2 |  10 | a numeric operation
//!   NUM |  11 | a 120-bit number
//!
//! The semantics of the 1st and 2nd values depend on the pointer tag.
//!
//!   Tag | 1st ptr value                | 2nd ptr value
//!   --- | ---------------------------- | ---------------------------------
//!   DP0 | the duplication label        | points to the duplication node
//!   DP1 | the duplication label        | points to the duplication node
//!   VAR | not used                     | points to the lambda node
//!   ARG | not used                     | points to the variable occurrence
//!   ERA | not used                     | not used
//!   LAM | not used                     | points to the lambda node
//!   APP | not used                     | points to the application node
//!   SUP | the duplication label        | points to the superposition node
//!   CTR | the constructor name         | points to the constructor node
//!   FUN | the function name            | points to the function node
//!   OP2 | the operation name           | points to the operation node
//!   NUM | the most significant 72 bits | the least significant 48 bits
//!
//! Notes:
//!
//! 1. The duplication label is an internal value used on the DUP-SUP rule.
//! 2. The operation name only uses 4 of the 72 bits, as there are only 16 ops.
//! 3. NUM pointers don't point anywhere, they just store the number directly.
//!
//! A node is a tuple of N pointers stored on sequential memory indices.
//! The meaning of each index depends on the node. There are 7 types:
//!
//! Duplication Node:
//!
//! ```text
//! [0] => either an ERA or an ARG pointing to the 1st variable location
//! [1] => either an ERA or an ARG pointing to the 2nd variable location.
//! [2] => pointer to the duplicated expression
//! ```
//!
//! Lambda Node:
//!
//! ```text
//! [0] => either and ERA or an ERA pointing to the variable location
//! [1] => pointer to the lambda's body
//! ```
//!
//! Application Node:
//!
//! ```text
//! [0] => pointer to the lambda
//! [1] => pointer to the argument
//! ```
//!
//! Superposition Node:
//!
//! ```text
//! [0] => pointer to the 1st superposed value
//! [1] => pointer to the 2sd superposed value
//! ```
//!
//! Constructor Node:
//!
//! ```text
//! [0] => pointer to the 1st field
//! [1] => pointer to the 2nd field
//! ... => ...
//! [N] => pointer to the Nth field
//! ```
//!
//! Function Node:
//!
//! ```text
//! [0] => pointer to the 1st argument
//! [1] => pointer to the 2nd argument
//! ... => ...
//! [N] => pointer to the Nth argument
//! ```
//!
//! Operation Node:
//!
//! ```text
//! [0] => pointer to the 1st operand
//! [1] => pointer to the 2nd operand
//! ```
//!
//! Notes:
//!
//! 1. Duplication nodes DON'T have a body. They "float" on the global scope.
//! 2. Lambdas and Duplications point to their variables, and vice-versa.
//! 3. ARG pointers can only show up inside Lambdas and Duplications.
//! 4. Nums and vars don't require a node type, because they're unboxed.
//! 5. Function and Constructor arities depends on the user-provided definition.
//!
//! ## Examples
//! 
//! ### Example 0
//!
//! Term: `{T2 #7 #8}`
//!
//! Memory:
//!
//! ```text
//! Root : Ptr(CTR, 0x0000000007b9d30a43, 0x000000000000)
//! 0x00 | Ptr(NUM, 0x000000000000000000, 0x000000000007) // the tuple's 1st field
//! 0x01 | Ptr(NUM, 0x000000000000000000, 0x000000000008) // the tuple's 2nd field
//! ```
//!
//! Notes:
//!
//! 1. This is just a pair with two numbers.
//! 2. The root pointer is not stored on memory.
//! 3. The '0x0000000007b9d30a43' constant encodes the 'T2' name.
//! 4. Since nums are unboxed, a 2-tuple uses 2 memory slots, or 32 bytes.
//!
//! ### Example 1
//!
//! Term: `λ~ λb b`
//!
//! Memory:
//!
//! ```text
//! Root : Ptr(LAM, 0x000000000000000000, 0x000000000000)
//! 0x00 | Ptr(ERA, 0x000000000000000000, 0x000000000000) // 1st lambda's argument
//! 0x01 | Ptr(LAM, 0x000000000000000000, 0x000000000002) // 1st lambda's body
//! 0x02 | Ptr(ARG, 0x000000000000000000, 0x000000000003) // 2nd lambda's argument
//! 0x03 | Ptr(VAR, 0x000000000000000000, 0x000000000002) // 2nd lambda's body
//! ```
//!
//! Notes:
//!
//! 1. This is a λ-term that discards the 1st argument and returns the 2nd.
//! 2. The 1st lambda's argument not used, thus, an ERA pointer.
//! 3. The 2nd lambda's argument points to its variable, and vice-versa.
//! 4. Each lambda uses 2 memory slots. This term uses 64 bytes in total.
//!
//! ### Example 2
//!
//! Term: `λx dup x0 x1 = x; (* x0 x1)`
//!
//! Memory:
//!
//! ```text
//! Root : Ptr(LAM, 0x000000000000000000, 0x000000000000)
//! 0x00 | Ptr(ARG, 0x000000000000000000, 0x000000000004) // the lambda's argument
//! 0x01 | Ptr(OP2, 0x000000000000000002, 0x000000000005) // the lambda's body
//! 0x02 | Ptr(ARG, 0x000000000000000000, 0x000000000005) // the duplication's 1st argument
//! 0x03 | Ptr(ARG, 0x000000000000000000, 0x000000000006) // the duplication's 2nd argument
//! 0x04 | Ptr(VAR, 0x000000000000000000, 0x000000000000) // the duplicated expression
//! 0x05 | Ptr(DP0, 0x7b93e8d2b9ba31fb21, 0x000000000002) // the operator's 1st operand
//! 0x06 | Ptr(DP1, 0x7b93e8d2b9ba31fb21, 0x000000000002) // the operator's 2st operand
//! ```
//!
//! Notes:
//!
//! 1. This is a lambda function that squares a number.
//! 2. Notice how every ARGs point to a VAR/DP0/DP1, that points back its source node.
//! 3. DP1 does not point to its ARG. It points to the duplication node, which is at 0x02.
//! 4. The lambda's body does not point to the dup node, but to the operator. Dup nodes float.
//! 5. 0x7b93e8d2b9ba31fb21 is a globally unique random label assigned to the duplication node.
//! 6. That duplication label is stored on the DP0/DP1 that point to the node, not on the node.
//! 7. A lambda uses 2 memory slots, a duplication uses 3, an operator uses 2. Total: 112 bytes.
//! 8. In-memory size is different to, and larger than, serialization size.

#![warn(dead_code)]
#![warn(non_snake_case)]
#![warn(unused_variables)]
#![warn(clippy::style)]
#![warn(clippy::identity_op)]

use std::fmt::{self, Display};

use kindelia_common::nohash_hasher;
use kindelia_common::{Name, U120};

use crate::util::mask;

use super::U128_NONE;

// RawCell
// =======

/// An HVM memory cell/word. It can be a variable ocurrence, point to an HVM
/// node or store an unboxed U120.
///
/// These are the basic possible layouts for a HVM u128 memory cell:
///
///  | ----------------------------------------------------------- |
///  | Tag (8 bits)    | 1st field (72 bits) | 2nd field (48 bits) |
///  | --------------- | ------------------- | ------------------- |
///  | DP0/DP1/SUP     | duplication label   | pointer             |
///  | CTR/FUN         | Name                | pointer             |
///  | other           | not used            | pointer             |
///  | ERA             | not used            | not used            |
///  | OP2             | opcode              | pointer             |
///  | --------------- | ----------------------------------------- |
///  | NUM             | U120 number                               |
///  | ----------------------------------------------------------- |
#[derive(Debug, Eq, PartialEq, Clone, Hash, Copy)]
#[repr(transparent)]
pub struct RawCell(u128);

// Constants
impl RawCell {
  pub const ZERO: RawCell = RawCell(0u128);
  pub const NONE: RawCell = RawCell(U128_NONE);

  // Size of each RawCell field in bits
  pub const VAL_SIZE: usize = 48;
  pub const EXT_SIZE: usize = Name::MAX_BITS;
  pub const TAG_SIZE: usize = 8;
  pub const NUM_SIZE: usize = Self::EXT_SIZE + Self::VAL_SIZE;

  // Position of each RawCell field
  pub const VAL_POS: usize = 0;
  pub const EXT_POS: usize = Self::VAL_POS + Self::VAL_SIZE;
  pub const TAG_POS: usize = Self::EXT_POS + Self::EXT_SIZE;
  pub const NUM_POS: usize = 0;

  // First bit of each field
  pub const VAL_SHL: u128 = 1 << Self::VAL_POS;
  pub const EXT_SHL: u128 = 1 << Self::EXT_POS;
  pub const TAG_SHL: u128 = 1 << Self::TAG_POS;
  pub const NUM_SHL: u128 = 1 << Self::NUM_POS;

  // Bit mask for each field
  pub const VAL_MASK: u128 = mask(Self::VAL_SIZE, Self::VAL_POS);
  pub const EXT_MASK: u128 = mask(Self::EXT_SIZE, Self::EXT_POS);
  pub const TAG_MASK: u128 = mask(Self::TAG_SIZE, Self::TAG_POS);
  pub const NUM_MASK: u128 = mask(Self::NUM_SIZE, Self::NUM_POS);
}

// Constructors
impl RawCell {
  pub fn var(pos: Loc) -> RawCell {
    RawCell((CellTag::VAR as u128 * Self::TAG_SHL) | *pos as u128)
  }

  pub fn dp0(col: u128, pos: Loc) -> RawCell {
    RawCell(
      (CellTag::DP0 as u128 * Self::TAG_SHL)
        | (col * Self::EXT_SHL)
        | *pos as u128,
    )
  }

  pub fn dp1(col: u128, pos: Loc) -> RawCell {
    RawCell(
      (CellTag::DP1 as u128 * Self::TAG_SHL)
        | (col * Self::EXT_SHL)
        | *pos as u128,
    )
  }

  pub fn arg(pos: Loc) -> RawCell {
    RawCell((CellTag::ARG as u128 * Self::TAG_SHL) | *pos as u128)
  }

  pub fn era() -> RawCell {
    RawCell(CellTag::ERA as u128 * Self::TAG_SHL)
  }

  pub fn lam(pos: Loc) -> RawCell {
    RawCell((CellTag::LAM as u128 * Self::TAG_SHL) | *pos as u128)
  }

  pub fn app(pos: Loc) -> RawCell {
    RawCell((CellTag::APP as u128 * Self::TAG_SHL) | *pos as u128)
  }

  pub fn par(col: u128, pos: Loc) -> RawCell {
    RawCell(
      (CellTag::SUP as u128 * Self::TAG_SHL)
        | (col * Self::EXT_SHL)
        | *pos as u128,
    )
  }

  pub fn op2(ope: u128, pos: Loc) -> RawCell {
    RawCell(
      (CellTag::OP2 as u128 * Self::TAG_SHL)
        | (ope * Self::EXT_SHL)
        | *pos as u128,
    )
  }

  pub fn num(val: u128) -> RawCell {
    debug_assert!((!Self::NUM_MASK & val) == 0, "Num overflow: `{}`.", val);
    RawCell((CellTag::NUM as u128 * Self::TAG_SHL) | (val & Self::NUM_MASK))
  }

  pub fn ctr(fun: Name, pos: Loc) -> RawCell {
    RawCell(
      (CellTag::CTR as u128 * Self::TAG_SHL)
        | (*fun * Self::EXT_SHL)
        | *pos as u128,
    )
  }

  pub fn fun(fun: Name, pos: Loc) -> RawCell {
    RawCell(
      (CellTag::FUN as u128 * Self::TAG_SHL)
        | (*fun * Self::EXT_SHL)
        | *pos as u128,
    )
  }

  pub fn nil() -> RawCell {
    RawCell(CellTag::NIL as u128 * RawCell::TAG_SHL)
  }
}

impl std::ops::Deref for RawCell {
  type Target = u128;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl RawCell {
  pub fn new(value: u128) -> Option<Self> {
    let tag = value >> (Self::TAG_POS);
    CellTag::try_from(tag as u8).ok().map(|_| RawCell(value))
  }

  /// For testing purposes only. TODO: remove.
  pub const fn new_unchecked(value: u128) -> Self {
    RawCell(value)
  }

  pub fn get_tag(&self) -> CellTag {
    let tag = (**self / Self::TAG_SHL) as u8;
    let tag = CellTag::try_from(tag);
    tag.expect("Unknown cell tag")
  }

  pub fn get_ext(&self) -> u128 {
    (**self / Self::EXT_SHL) & 0xFF_FFFF_FFFF_FFFF_FFFF
  }

  pub fn get_ptr(&self) -> Loc {
    Loc((**self & 0xFFFF_FFFF_FFFF) as u64)
  }

  pub fn get_loc(&self, arg: u64) -> Loc {
    self.get_ptr() + arg
  }

  pub fn get_num(&self) -> U120 {
    U120::from_u128_unchecked(**self & Self::NUM_MASK)
  }

  pub fn get_name_from_ext(&self) -> Name {
    Name::new_unsafe(self.get_ext())
  }
}

// CellTag
// =======

/// A memory cell tag. It's stored in it's first byte to determine the cell's
/// type. Doesn't start at `0x00` to prevent a completely zeroed cell, which
/// would be considered a "cleared" position. Starts as `0x02` because a DP0 vs
/// DP1 check is done on the first bit of the tag on some parts of the code, but
/// that could be different.
#[derive(PartialEq, Eq)]
#[repr(u8)]
pub enum CellTag {
  DP0 = 0x02,
  DP1 = 0x03,
  VAR = 0x04,
  ARG = 0x05,
  ERA = 0x06,
  LAM = 0x07,
  APP = 0x08,
  SUP = 0x09,
  CTR = 0x0A,
  FUN = 0x0B,
  OP2 = 0x0C,
  NUM = 0x0D,
  NIL = 0x0F,
}

impl TryFrom<u8> for CellTag {
  type Error = ();
  fn try_from(tag: u8) -> Result<Self, ()> {
    match tag {
      tag if tag == (CellTag::DP0 as u8) => Ok(CellTag::DP0),
      tag if tag == (CellTag::DP1 as u8) => Ok(CellTag::DP1),
      tag if tag == (CellTag::VAR as u8) => Ok(CellTag::VAR),
      tag if tag == (CellTag::ARG as u8) => Ok(CellTag::ARG),
      tag if tag == (CellTag::ERA as u8) => Ok(CellTag::ERA),
      tag if tag == (CellTag::LAM as u8) => Ok(CellTag::LAM),
      tag if tag == (CellTag::APP as u8) => Ok(CellTag::APP),
      tag if tag == (CellTag::SUP as u8) => Ok(CellTag::SUP),
      tag if tag == (CellTag::CTR as u8) => Ok(CellTag::CTR),
      tag if tag == (CellTag::FUN as u8) => Ok(CellTag::FUN),
      tag if tag == (CellTag::OP2 as u8) => Ok(CellTag::OP2),
      tag if tag == (CellTag::NUM as u8) => Ok(CellTag::NUM),
      tag if tag == (CellTag::NIL as u8) => Ok(CellTag::NIL),
      _ => Err(()),
    }
  }
}

impl Display for RawCell {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if *self == RawCell::ZERO {
      f.write_str("?")
    } else {
      let tag = self.get_tag();
      let ptr = self.get_ptr();
      let tag_str = match tag {
        CellTag::DP0 => "DP0",
        CellTag::DP1 => "DP1",
        CellTag::VAR => "VAR",
        CellTag::ARG => "ARG",
        CellTag::ERA => "ERA",
        CellTag::LAM => "LAM",
        CellTag::APP => "APP",
        CellTag::SUP => "SUP",
        CellTag::CTR => "CTR",
        CellTag::FUN => "FUN",
        CellTag::OP2 => "OP2",
        CellTag::NUM => "NUM",
        _ => "?",
      };
      let name = self.get_name_from_ext();
      f.write_fmt(format_args!("{}:{}:{:x}", tag_str, name, *ptr))
    }
  }
}

// Loc
// ===

/// HVM memory location, or "pointer".
#[derive(Debug, Eq, PartialEq, Clone, Hash, Copy)]
#[repr(transparent)]
pub struct Loc(u64);

impl Loc {
  pub const ZERO: Loc = Loc(0);

  pub const _MAX: u64 = (1 << RawCell::VAL_SIZE) - 1;
  pub const MAX: Loc = Loc(Loc::_MAX);

  pub fn new(value: u64) -> Option<Self> {
    if value >> RawCell::VAL_SIZE == 0 {
      Some(Loc(value))
    } else {
      None
    }
  }

  pub fn from_u64_unchecked(value: u64) -> Self {
    Loc(value)
  }
}

impl std::ops::Deref for Loc {
  type Target = u64;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl std::ops::Add<u64> for Loc {
  type Output = Self;
  fn add(self, other: u64) -> Self::Output {
    Loc(self.0 + other)
  }
}

impl std::ops::Add for Loc {
  type Output = Self;
  fn add(self, other: Self) -> Self::Output {
    Loc(self.0 + other.0)
  }
}

impl nohash_hasher::IsEnabled for Loc {}
