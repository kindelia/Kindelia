use std::fmt;
use std::str::FromStr;
use std::string::ToString;

use serde::{Deserialize, Serialize};

use crate::hvm::{EXT_SIZE, U120};

// Name
// ====

/// A Name inside the Kindelia Chain that constist of, at most, 12 6-bit
/// letters (72 bits).
///
/// Name strings are converted to numbers using the following table:
/// ```
/// '.'       =>  0
/// '0' - '9' =>  1 to 10
/// 'A' - 'Z' => 11 to 36
/// 'a' - 'z' => 37 to 62
/// '_'       => 63
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(into = "String", try_from = "&str")]
pub struct Name(u128);

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

  pub const _NONE  : u128 = 0x3FFFF; // ?? '___'

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
  fn from(num: U120) -> Self {
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
