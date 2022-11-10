use std::fmt;
use std::str::FromStr;
use std::string::ToString;

use serde::{Deserialize, Serialize};

use crate::hvm::EXT_SIZE;

// U120
// ====

/// A unsigned 120 bit integer: the native unboxed integer type
/// of the Kindelia's HVM.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[serde(into = "String", try_from = "&str")]
#[repr(transparent)]
pub struct U120(u128);

impl U120 {
  pub const ZERO: U120 = U120(0);
  pub const MAX: U120 = U120((1_u128 << 120) - 1);

  pub fn new(numb: u128) -> Option<Self> {
    if numb >> 120 == 0 {
      Some(U120(numb))
    }
    else {
      None
    }
  }

  pub fn from_u128_unchecked(numb: u128) -> Self { 
    debug_assert_eq!(numb >> 120, 0_u128);
    U120(numb)
  }

  pub fn wrapping_add(self, other:U120) -> U120 {
    let res = self.0 + other.0;
    U120(res & U120::MAX.0)
  }

  pub fn wrapping_sub(self, other: U120) -> U120 {
    let other_complement = U120::wrapping_add(U120(other.0 ^ U120::MAX.0), U120(1));
    U120::wrapping_add(self, other_complement)
  }

  // based off of this answer https://stackoverflow.com/a/1815371
  // maybe this is too much work for an easy function?
  // idk, maybe there's a better way to do this
  pub fn wrapping_mul(self, other: U120) -> U120 {
    const LO_MASK : u128  =  (1 << 60) - 1;
    let a = self.0;
    let b = other.0;
    let a_lo = a & LO_MASK;
    let a_hi = a >> 60;
    let b_lo = b & LO_MASK;
    let b_hi = b >> 60;
    let s0 = a_lo * b_lo;
    let s1 = ((a_hi * b_lo) & LO_MASK) << 60;
    let s2 = ((b_hi * a_lo) & LO_MASK) << 60;
    U120(s0).wrapping_add(U120(s1)).wrapping_add(U120(s2))
  }

  // Wrapping div is just normal division, since
  // self / other is always smaller than self.
  // warning: this will panic when other is 0.
  pub fn wrapping_div(self, other: U120) -> U120 {
    U120(self.0 / other.0)
  }

  // Wrapping remainder is just normal remainder
  // given that self % other is always smaller than other
  // by definition of the modulo operation.
  pub fn wrapping_rem(self, other: U120) -> U120 {
    U120(self.0 % other.0)
  }

  // Wrapping shift left is only defined for
  // values `other` between 0 and 120. For values bigger than
  // that, it will wrap the value module 120 before doing the shift.
  // Ex: (1u120 << 120) === (1u120 << 0) === 1u120 
  pub fn wrapping_shl(self, other: U120) -> U120 {
    U120((self.0 << (other.0 % 120)) & U120::MAX.0)
  }

  pub fn wrapping_shr(self, other: U120) -> U120 {
    U120(self.0 >> (other.0 % 120))
  }

  pub fn to_hex_literal(&self) -> String {
    format!("#x{:x}", self.0)
  }
}

impl std::ops::Deref for U120 {
  type Target = u128;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl TryFrom<u128> for U120 {
  type Error = String;
  fn try_from(numb: u128) -> Result<Self, Self::Error> {
    if numb >> 120 != 0 {
      Err(format!("Number {} does not fit in 120-bits.", numb))
    } else {
      Ok(U120(numb))
    }
  }
}

impl From<Name> for U120 {
  fn from(num: Name) -> Self {
    U120(*num)
  }
}

impl fmt::Display for U120 {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      write!(f, "{}", self.0)
  }
}

impl From<U120> for String {
  fn from(num: U120) -> Self {
      num.to_string()
  }
}


impl TryFrom<&str> for U120 {
  type Error = String;
  fn try_from(numb: &str) -> Result<Self, Self::Error> {
    fn err_msg<E: fmt::Debug>(e: E) -> String {
      format!("Invalid number string '{:?}'", e)
    }
    let (rest, result) = crate::parser::parse_numb(numb).map_err(err_msg)?;
    if !rest.is_empty() {
      Err(err_msg(numb))
    } else {
      Ok(result)
    }
  }
}

impl crate::NoHashHasher::IsEnabled for U120 {}

// Name
// ====

/// A Name inside the Kindelia Chain that constist of, at most, 12 6-bit
/// letters (72 bits).
///
/// Name strings are converted to numbers using the following table:
/// ```text
/// '.'       =>  0
/// '0' - '9' =>  1 to 10
/// 'A' - 'Z' => 11 to 36
/// 'a' - 'z' => 37 to 62
/// '_'       => 63
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[serde(into = "String", try_from = "&str")]
#[repr(transparent)]
pub struct Name(u128);

impl crate::NoHashHasher::IsEnabled for Name {}

pub fn char_to_code(chr: char) -> Result<u128, String> {
  let num = match chr {
    '.' => 0,
    '0'..='9' => 1 + chr as u128 - '0' as u128,
    'A'..='Z' => 11 + chr as u128 - 'A' as u128,
    'a'..='z' => 37 + chr as u128 - 'a' as u128,
    '_' => 63,
    _ => {
      return Err(format!("Invalid Kindelia Name letter '{}'.", chr));
    }
  };
  Ok(num)
}

impl Name {
  pub const MAX_BITS: usize = EXT_SIZE;
  pub const MAX_CHARS: usize = Self::MAX_BITS / 6;

  pub const _NONE: u128 = 0x3FFFF; // ?? '___'

  pub const EMPTY: Name = Name(0);
  pub const NONE: Name = Name(Self::_NONE);

  /// Creates a new name from a number.
  /// A name should fit in the EXT field (72-bits).
  pub const fn new(name: u128) -> Option<Self> {
    if name >> Self::MAX_BITS == 0 {
      Some(Name(name))
    } else {
      None
    }
  }

  // TODO: should be removed
  /// DEPRECATED
  pub const fn new_unsafe(name: u128) -> Self {
    debug_assert!(name >> Self::MAX_BITS == 0);
    Name(name)
  }

  pub fn is_empty(&self) -> bool {
    self.0 == 0
  }

  pub fn is_none(&self) -> bool {
    self.0 == Self::_NONE
  }

  pub const fn from_u128_unchecked(numb: u128) -> Self {
    Name(numb)
  }

  #[allow(clippy::should_implement_trait)]
  pub fn from_str(name_txt: &str) -> Result<Name, String> {
    if name_txt.len() > Self::MAX_CHARS {
      Err(format!("Name '{}' exceeds {} letters.", name_txt, Self::MAX_CHARS))
    } else {
      let mut num: u128 = 0;
      for chr in name_txt.chars() {
        num = (num << 6) + char_to_code(chr)?;
      }
      Ok(Name(num))
    }
  }

  /// Converts a name string to a Name. Same as `from_str`, but panics
  /// when name length > 12 or on invalid letter. **DEPRECATED**.
  // TODO: This should be removed.
  pub fn from_str_unsafe(name_txt: &str) -> Name {
    let mut num: u128 = 0;
    for (i, chr) in name_txt.chars().enumerate() {
      debug_assert!(i < Self::MAX_CHARS, "Name too big: `{}`.", name_txt);
      num = (num << 6) + char_to_code(chr).unwrap();
    }
    Name(num)
  }

  pub fn show_hex(&self) -> String {
    format!("#x{:0>30x}", **self)
  }
}

impl std::ops::Deref for Name {
  type Target = u128;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    let mut name = String::new();
    let mut num = self.0;
    while num > 0 {
      let chr = (num % 64) as u8;
      let chr = match chr {
        0 => '.',
        1..=10 => (chr - 1 + b'0') as char,
        11..=36 => (chr - 11 + b'A') as char,
        37..=62 => (chr - 37 + b'a') as char,
        63 => '_',
        64.. => panic!("Impossible letter value."),
      };
      name.push(chr);
      num /= 64;
    }
    let name: String = name.chars().rev().collect();
    write!(f, "{}", name)
  }
}

impl TryFrom<u128> for Name {
  type Error = String;
  fn try_from(name: u128) -> Result<Self, Self::Error> {
    if name >> Self::MAX_BITS != 0 {
      Err(format!("Name does not fit in {}-bits.", Self::MAX_BITS))
    } else {
      Ok(Name(name))
    }
  }
}

impl From<U120> for Name {
  // FIXME: checked conversion (TryFrom)
  fn from(num: U120) -> Self {
    assert!(*num >> Name::MAX_BITS == 0);
    Name(*num)
  }
}

// Necessary for serde `try_from` attr
impl TryFrom<&str> for Name {
  type Error = String;
  fn try_from(name: &str) -> Result<Self, Self::Error> {
    Name::from_str(name)
  }
}

// Necessary for serde `into` attr
impl From<Name> for String {
  fn from(name: Name) -> Self {
    name.to_string()
  }
}

// Necessary for `clap` parsing
impl FromStr for Name {
  type Err = String;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    s.try_into()
  }
}

// Persistence
// ===========

impl crate::persistence::DiskSer for U120 {
  fn disk_serialize<W: std::io::Write>(&self, sink: &mut W) -> std::io::Result<usize>{ 
    self.0.disk_serialize(sink)
  }
  fn disk_deserialize<R: std::io::Read>(source: &mut R) -> std::io::Result<Option<Self>> {
    let num = u128::disk_deserialize(source)?;
    match num {
      None => Ok(None),
      Some(num) => {
        if num >> 120 == 0 {
          Ok(Some(U120(num)))
        }
        else {
          Err(std::io::Error::from(std::io::ErrorKind::InvalidData))
        }
      }
    }
  }
}

impl crate::persistence::DiskSer for Name {
  fn disk_serialize<W: std::io::Write>(&self, sink: &mut W) -> std::io::Result<usize>{ 
    self.0.disk_serialize(sink)
  }
  fn disk_deserialize<R: std::io::Read>(source: &mut R) -> std::io::Result<Option<Self>> {
    let num = u128::disk_deserialize(source)?;
    match num {
      None => Ok(None),
      Some(num) => Ok(Name::new(num))
    }
  }
}
