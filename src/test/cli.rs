use proptest::collection::vec;
use proptest::proptest;
use rstest::rstest;
use std::convert::TryInto;
use std::io::Write;
use std::{fmt::Debug, process::Command, process::Stdio};

use crate::api;
use crate::bits;
use crate::common;
use crate::hvm;
use crate::node;
use crate::util;

use crate::test::strategies::statement;

use super::util::{temp_file, TempPath};

/// Runs a kindelia command.
///
/// Returns a `Command` that you can use
/// to check the stderr and stdout outputs.
macro_rules! kindelia {
  () => {
    Command::new("cargo")
      // .stdin(Stdio::piped())
      // .stdout(Stdio::piped())
      .arg("run")
      .arg("--profile=dev_fast")
      .arg("--")
  };
}

fn get_stdout(c: &std::process::Output) -> String {
  std::str::from_utf8(&c.stdout).unwrap().trim().to_string()
}

fn get_stderr(c: &std::process::Output) -> String {
  std::str::from_utf8(&c.stderr).unwrap().trim().to_string()
}

fn get_runs_result(output: &str) -> Vec<&str> {
  output
    .lines()
    .filter(|line| line.contains("[run]"))
    .map(|line| {
      let line = line.strip_prefix("0000000000 [run] ").unwrap();
      let idx = line.find('\u{1b}').unwrap();
      &line[0..idx - 1]
    })
    .collect()
}

#[rstest]
#[case("example/block_1.kdl")]
#[case("example/block_2.kdl")]
#[case("example/block_3.kdl")]
#[case("example/block_4.kdl")]
#[case("example/block_5.kdl")]
fn serialization(#[case] file: &str, temp_file: TempPath) {
  // read file and get statments
  let file_content = std::fs::read_to_string(file).unwrap();
  let (_, s1) = hvm::read_statements(&file_content).unwrap();

  // serializes file and saves it in a temp file
  let output = kindelia!().args(["serialize", file]).output().unwrap();
  let err = get_stderr(&output);
  let output = get_stdout(&output);

  eprintln!("OUT:\n{}", output);
  eprintln!("ERR:\n{}", err);

  std::fs::write(&temp_file.path, &output)
    .expect("Could not write in serialized file");

  // deserializes it and parses it into statements again
  let output = kindelia!()
    .args(["deserialize", &temp_file.path.to_str().unwrap()])
    .output()
    .unwrap();
  let output = get_stdout(&output);
  let (_, s2) = hvm::read_statements(&output).unwrap();

  eprintln!("OUT:\n{}", output);

  // checks if the statements are equal
  assert_eq!(s1, s2)
}

#[rstest]
#[case("example/block_1.kdl", &["#10", "#65536"])]
#[case("example/block_2.kdl", &["#3"])]
#[case("example/block_3.kdl", &["#656161725219724531611238334681629285"])]
#[case("example/block_4.kdl", &["#42"])]
#[case("example/block_5.kdl", &["{Entry #7 #100 {Entry #2 #200 {Empty}}}"])]
fn test_examples(#[case] file: &str, #[case] expected_results: &[&str]) {
  let output = kindelia!().args(["test", file]).output().unwrap();
  let err = get_stderr(&output);
  let output = get_stdout(&output);
  eprintln!("OUT:\n{}", output);
  eprintln!("ERR:\n{}", err);
  let results = get_runs_result(&output);
  assert_eq!(results, expected_results)
}

#[rstest]
#[case("example/private_key_1_namer", "#656161725219724531611238334681629285")]
#[case("example/private_key_2_alice", "#225111118185718227719509163399323998")]
#[case("example/private_key_3_bob", "#540402903301314077240655651075245048")]
fn signing_run(
  #[case] private_key: &str,
  #[case] expected_result: &str,
  temp_file: TempPath,
) {
  let output = kindelia!()
    .args(["sign", "example/block_3.unsig.kdl", "--secret-file", private_key])
    .output()
    .unwrap();
  let output = get_stdout(&output);
  std::fs::write(&temp_file.path, &output).unwrap();
  let output = kindelia!()
    .args(["test", temp_file.path.to_str().unwrap()])
    .output()
    .unwrap();
  let output = get_stdout(&output);
  let result = get_runs_result(&output);
  assert_eq!(result, [expected_result])
}

#[rstest]
#[case("example/private_key_1_namer", "755db39f7093d5969f3b88e6023f4bf7cca8c046e3cd00013cd7f9198fef2d0653daa42da9a3aa6bab1344b13dbb87355f25f2a0393ae4a1e836b0e50a763c227e21743712e4abaa15450db66c")]
#[case("example/private_key_2_alice", "755db39f7093d5969f3b88e6023771ae6a24b5b8c3be37068d8b0eb74d293c99396bc3f21a0a221aae3dfe8dee8b78680600b304724c4637d3729b982c24249a8dc47b9aa0eeb0eda6cef17490")]
#[case("example/private_key_3_bob", "755db39f7093d5969f3b88e600304bbff232fb35a58fc6c2f1b02a105514bdf9a05a400cfe684342ab9339f3bb628ba11b859e9cfda89730a347fb1194bd88e6d71d40aabc3a3be8799c3d9734")]
fn signing(#[case] private_key: &str, #[case] expected_result: &str) {
  let output = kindelia!()
    .args([
      "sign",
      "example/block_3.unsig.kdl",
      "--secret-file",
      private_key,
      "-E",
    ])
    .output()
    .unwrap();
  let output = get_stdout(&output);
  assert_eq!(output, expected_result)
}

#[rstest]
#[case("/constructor/*", Some("T3"), ctr_response_1(), "ctr arity", "3")]
#[case(
  "/constructor/*",
  Some("T3"),
  ctr_response_1(),
  "ctr code",
  "{T3 x0 x1 x2}"
)]
#[case("/reg/*", Some("Foo"), reg_response_1(), "reg owner", "400")]
#[case(
  "/reg/*",
  Some("Foo"),
  reg_response_1(),
  "reg list",
  "Foo\nFoo.Bar\nFoo.Bar.cats"
)]
#[case("/stats", None, stats_response_1(), "stats ctr-count", "3")]
#[case("/stats", None, stats_response_1(), "stats fun-count", "4")]
#[case("/stats", None, stats_response_1(), "stats reg-count", "5")]
#[case("/stats", None, stats_response_1(), "stats mana", "400")]
#[case("/stats", None, stats_response_1(), "stats space", "500")]
#[case("/stats", None, stats_response_1(), "stats tick", "700")]
// not working because the lack of u128 deserialzation support
// #[case("/peers/",None,peers_response_1(),"peers","0.0.0.1:42000\n0.0.0.1:42001")]
// not working because the `with { ~ }` syntax
// #[case("/functions/*", "Test", fun_response_1().0, "fun code", FUN_CODE)]
#[case("/functions/*/state", Some("Test"), fun_response_1().1, "fun state", "#42")]
fn test_get_mock<T: serde::Serialize>(
  #[case] path: &str,
  #[case] name: Option<&str>,
  #[case] response: T,
  #[case] command: &str,
  #[case] expected_result: &str,
) {
  // separate command and subcommand
  let command: Vec<_> = command.split(' ').collect();
  let subcommand = command.last();
  let command = command.first().unwrap();

  let mut path: String = path.into();
  // change path wildcard to name
  if let Some(name) = name {
    path = path.replace('*', name);
  }

  // create mock server
  let server = httpmock::MockServer::start();
  let get_mock = server.mock(|when, then| {
    when.method(httpmock::Method::GET).path(path);
    then.status(200).json_body_obj(&response);
  });
  let mock_url = format!("http://127.0.0.1:{}/", server.port());

  // execute a get
  let mut args = vec!["--api", &mock_url, "get", command];
  if let Some(name) = name {
    args.push(name);
  }
  if let Some(subcommand) = subcommand {
    args.push(subcommand);
  }
  let output = kindelia!().args(&args).output().unwrap();
  let output = get_stdout(&output);
  assert_eq!(output, expected_result)
}

fn ctr_response_1() -> api::CtrInfo {
  api::CtrInfo { arit: 3 }
}

fn reg_response_1() -> api::RegInfo {
  let names: Vec<common::Name> = vec!["Foo", "Foo.Bar", "Foo.Bar.cats"]
    .iter()
    .map(|s| (*s).try_into().unwrap())
    .collect();
  api::RegInfo { ownr: common::Name::from_u128_unchecked(1024), stmt: names }
}

// fn peers_response_1() -> Vec<node::Peer> {
//   vec![
//     node::Peer { address: read_address("0.0.0.1:42000"), seen_at: 1 },
//     node::Peer { address: read_address("0.0.0.1:42001"), seen_at: 2 },
//   ]
// }

pub const FUN_CODE: &str = "fun (Test ___) {
  (Test a #0) = #0
  (Test #0 b) = #0
  (Test a b) = #1
} with { #42 }";

fn fun_response_1() -> (api::FuncInfo, hvm::Term) {
  let code = FUN_CODE;
  let func = hvm::read_statement(code).unwrap().1;
  if let hvm::Statement::Fun { name, args, func, init, sign } = func {
    (api::FuncInfo { func }, init)
  } else {
    panic!("Not a function")
  }
}

fn stats_response_1() -> api::Stats {
  api::Stats {
    ctr_count: 3,
    fun_count: 4,
    reg_count: 5,
    mana: 400,
    space: 500,
    tick: 700,
  }
}

// #[test]
// fn count_check() {
//   let mut node = kindelia!().args(["node", "start", "--mine"]).spawn().unwrap();
//   std::thread::sleep(std::time::Duration::from_millis(10000));
//   let get =
//     kindelia!().args(["get", "fun", "Count", "state"]).output().unwrap();
//   let output = get_stdout(&get);
//   assert_eq!(output, "#0");
//   node.kill().unwrap();
// }
