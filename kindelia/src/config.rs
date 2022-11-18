use std::{path::PathBuf, str::FromStr};

use crate::files::FileInput;

// ConfigSettings
// ==============

#[derive(derive_builder::Builder)]
#[builder(setter(strip_option))]
pub struct ConfigSettings<T, F>
where
  T: Clone + Sized,
  F: Fn() -> Result<T, String>,
{
  #[builder(default)]
  env: Option<&'static str>,
  #[builder(default)]
  prop: Option<String>,
  default_value: F,
}

impl<T, F> ConfigSettings<T, F>
where
  T: Clone + Sized,
  F: Fn() -> Result<T, String>,
{
  /// Resolve config value.
  ///
  /// Priority is:
  /// 1. CLI argument
  /// 2. Environment variable
  /// 3. Config file
  /// 4. Default value
  pub fn resolve(
    self,
    cli_value: Option<T>,
    config_values: Option<&toml::Value>,
  ) -> Result<T, String>
  where
    T: ArgumentFrom<String> + ArgumentFrom<toml::Value>,
  {
    if let Some(value) = cli_value {
      // Read from CLI argument
      return Ok(value);
    }
    if let Some(Ok(env_value)) = self.env.map(std::env::var) {
      // If env var is set, read from it
      return T::arg_from(env_value);
    }
    if let (Some(prop_path), Some(config_values)) = (self.prop, config_values) {
      // If config file and argument prop path are set, read from config file
      return Self::resolve_from_config_aux(config_values, &prop_path);
    }
    (self.default_value)()
  }

  // TODO: refactor

  pub fn resolve_from_file_only(
    self,
    config_values: Option<&toml::Value>,
  ) -> Result<T, String>
  where
    T: ArgumentFrom<toml::Value>,
  {
    if let Some(prop_path) = self.prop {
      if let Some(config_values) = config_values {
        Self::resolve_from_config_aux(config_values, &prop_path)
      } else {
        (self.default_value)()
      }
    } else {
      panic!("Cannot resolve from config file config without 'prop' field set")
    }
  }

  pub fn resolve_from_file_opt(
    self,
    config_values: Option<&toml::Value>,
  ) -> Result<Option<T>, String>
  where
    T: ArgumentFrom<toml::Value>,
  {
    if let Some(prop_path) = self.prop {
      if let Some(config_values) = config_values {
        let value = Self::get_prop(config_values, &prop_path);
        if let Some(value) = value {
          return T::arg_from(value).map(|v| Some(v)).map_err(|e| {
            format!(
              "Could not convert value of '{}' into desired type: {}",
              prop_path, e
            )
          });
        }
      }
      Ok(None)
    } else {
      panic!("Cannot resolve from config file config without 'prop' field set")
    }
  }

  fn resolve_from_config_aux(
    config_values: &toml::Value,
    prop_path: &str,
  ) -> Result<T, String>
  where
    T: ArgumentFrom<toml::Value>,
  {
    let value = Self::get_prop(config_values, prop_path)
      .ok_or(format!("Could not find prop '{}' in config file.", prop_path))?;
    T::arg_from(value).map_err(|e| {
      format!(
        "Could not convert value of '{}' into desired type: {}",
        prop_path, e
      )
    })
  }

  fn get_prop(mut value: &toml::Value, prop_path: &str) -> Option<toml::Value> {
    // Doing this way because of issue #469 toml-rs
    let props: Vec<_> = prop_path.split('.').collect();
    for prop in props {
      value = value.get(prop)?;
    }
    Some(value.clone())
  }
}

// ArgumentFrom
// ============

/// A trait to convert from anything to a type T.
/// It is equal to standard From trait, but
/// it has the From<String> for Vec<String> implementation.
/// As like From, the conversion must be perfect.
pub trait ArgumentFrom<T>: Sized {
  fn arg_from(value: T) -> Result<Self, String>;
}

impl ArgumentFrom<String> for String {
  fn arg_from(t: String) -> Result<Self, String> {
    Ok(t)
  }
}

impl ArgumentFrom<String> for u32 {
  fn arg_from(t: String) -> Result<Self, String> {
    t.parse().map_err(|e| format!("Invalid integer: `{}`", e))
  }
}

impl ArgumentFrom<String> for u64 {
  fn arg_from(t: String) -> Result<Self, String> {
    t.parse().map_err(|e| format!("Invalid integer: `{}`", e))
  }
}

impl ArgumentFrom<toml::Value> for u32 {
  fn arg_from(value: toml::Value) -> Result<Self, String> {
    match value {
      toml::Value::Integer(i) => Ok(i as Self),
      toml::Value::String(s) => {
        let s = s.trim_start_matches("0x");
        let num = u32::from_str_radix(s, 16)
          .map_err(|e| format!("Invalid hexadecimal '{}': {}", s, e))?;
        Ok(num)
      }
      _ => Err(format!("Invalid integer '{}'", value)),
    }
  }
}

impl ArgumentFrom<toml::Value> for u64 {
  fn arg_from(value: toml::Value) -> Result<Self, String> {
    match value {
      toml::Value::Integer(i) => Ok(i as u64),
      toml::Value::String(s) => {
        let s = s.trim_start_matches("0x");
        let num = u64::from_str_radix(s, 16)
          .map_err(|e| format!("Invalid hexadecimal '{}': {}", s, e))?;
        Ok(num)
      }
      _ => Err(format!("Invalid integer '{}'", value)),
    }
  }
}

impl ArgumentFrom<String> for Vec<String> {
  fn arg_from(t: String) -> Result<Self, String> {
    Ok(t.split(',').map(|x| x.to_string()).collect())
  }
}

impl ArgumentFrom<String> for bool {
  fn arg_from(t: String) -> Result<Self, String> {
    if t == "true" {
      Ok(true)
    } else if t == "false" {
      Ok(false)
    } else {
      Err(format!("Invalid boolean value: {}", t))
    }
  }
}

impl ArgumentFrom<String> for PathBuf {
  fn arg_from(t: String) -> Result<Self, String> {
    if let Some(path) = t.strip_prefix("~/") {
      let home_dir =
        dirs::home_dir().ok_or("Could not find $HOME directory.")?;
      Ok(home_dir.join(path))
    } else {
      PathBuf::from_str(&t).map_err(|_| format!("Invalid path: {}", t))
    }
  }
}

impl ArgumentFrom<toml::Value> for PathBuf {
  fn arg_from(value: toml::Value) -> Result<Self, String> {
    let t: String =
      value.try_into().map_err(|_| "Could not convert value to PahtBuf")?;
    PathBuf::arg_from(t)
  }
}

impl ArgumentFrom<toml::Value> for String {
  fn arg_from(t: toml::Value) -> Result<Self, String> {
    t.try_into().map_err(|_| "Could not convert value into String".to_string())
  }
}

impl ArgumentFrom<toml::Value> for Vec<String> {
  fn arg_from(t: toml::Value) -> Result<Self, String> {
    t.try_into().map_err(|_| "Could not convert value into array".to_string())
  }
}

impl ArgumentFrom<toml::Value> for bool {
  fn arg_from(t: toml::Value) -> Result<Self, String> {
    t.as_bool().ok_or(format!("Invalid boolean value: {}", t))
  }
}

impl ArgumentFrom<toml::Value> for kindelia_core::config::ApiConfig {
  fn arg_from(t: toml::Value) -> Result<Self, String> {
    t.try_into().map_err(|_| "Could not convert value into array".to_string())
  }
}

pub fn arg_from_file_or_stdin<T: ArgumentFrom<String>>(
  file: FileInput,
) -> Result<T, String> {
  match file {
    FileInput::Path { path } => {
      // read from file
      let content = std::fs::read_to_string(&path).map_err(|err| {
        format!("Cannot read from '{:?}' file: {}", path, err)
      })?;
      T::arg_from(content)
    }
    FileInput::Stdin => {
      // read from stdin
      let mut input = String::new();
      match std::io::stdin().read_line(&mut input) {
        Ok(_) => T::arg_from(input.trim().to_string()),
        Err(err) => Err(format!("Could not read from stdin: {}", err)),
      }
    }
  }
}
