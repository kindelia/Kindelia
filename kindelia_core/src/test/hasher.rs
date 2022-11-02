use crate::{
  util::{u128map_new, u256map_new, u64map_new, U128Map},
  NoHashHasher::NoHashHasher,
};
use primitive_types::U256;
use proptest::{arbitrary::any, array, proptest};
use std::{collections::HashMap, hash::{Hasher, Hash}};

fn get_insertion_elapsed<K, H>(map: &mut HashMap<K, u32, H>, n_items: u32) -> u128
where
  K: std::cmp::Eq,
  K: std::hash::Hash,
  K: std::convert::From<u32>,
  H: std::hash::BuildHasher,
{
  let init_time = std::time::Instant::now();
  for i in 0..n_items {
    map.insert(i.into(), i);
    let v = map.get(&(i.into()));
    assert_eq!(v, Some(&i));
  }
  let elapsed = init_time.elapsed();
  elapsed.as_millis()
}

fn insertion_bench<K, H>(map: &mut HashMap<K, u32, H>, n_items: u32, n_iters: u32)
where
  K: std::cmp::Eq,
  K: std::hash::Hash,
  K: std::convert::From<u32>,
  H: std::hash::BuildHasher,
{
  let mut nohs_sum = 0;
  let mut dflt_sum = 0;
  for _ in 0..n_iters {
    nohs_sum += get_insertion_elapsed(map, n_items);
    let mut dflt_map: HashMap<K, u32> = HashMap::new();
    dflt_sum += get_insertion_elapsed(&mut dflt_map, n_items);
  }
  let nohs_avg = nohs_sum as f64 / n_iters as f64;
  let dflt_avg = dflt_sum as f64 / n_iters as f64;
  println!(
    "u128map insertion benchmark: {} ms (nohs) vs {} ms (dflt) with {} iterations",
    nohs_avg, dflt_avg, n_iters
  );
}

#[test]
#[ignore = "benchmark"]
fn test_u128map_insertion_bench() {
  let mut map = u128map_new();
  insertion_bench(&mut map, 1_000_000, 10);
}

#[test]
#[ignore = "benchmark"]
fn test_u64map_insertion_bench() {
  let mut map = u64map_new();
  insertion_bench(&mut map, 1_000_000, 10);
}

#[test]
#[ignore = "benchmark"]
fn test_u256map_insertion_bench() {
  let mut map = u256map_new();
  insertion_bench(&mut map, 1_000_000, 10);
}

proptest! {
    // // useless
    // #[test]
    // fn test_map_insert_u128(key in any::<u128>(), value in any::<u32>()) {
    //   let mut map = u128map_new();
    //   map.insert(key, value);
    //   assert_eq!(map.get(&key), Some(&value));
    // }

    // // useless
    // #[test]
    // fn test_map_insert_u64(key in any::<u64>(), value in any::<u32>()) {
    //   let mut map = u64map_new();
    //   map.insert(key, value);
    //   assert_eq!(map.get(&key), Some(&value));
    // }

    // // useless
    // #[test]
    // fn test_map_insert_u256(key in array::uniform32(any::<u8>()), value in any::<u32>()) {
    //   let mut map = u256map_new();
    //   let key = key.into(); // from [u8; 32] to U256
    //   map.insert(key, value);
    //   assert_eq!(map.get(&key), Some(&value));
    // }

    #[test]
    fn test_hasher_write_256(key in array::uniform32(any::<u8>())) {
      let mut hasher = NoHashHasher::<U256>::default();
      hasher.write(&key);
      let hash = hasher.finish();

      let expected = &key[0..8];
      let expected = u64::from_le_bytes(expected.try_into().unwrap());

      assert_eq!(hash, expected);
    }

    #[test]
    fn test_hasher_write_128(key in any::<u128>()) {
      let mut hasher = NoHashHasher::<u128>::default();
      key.hash(&mut hasher);
      let hash = hasher.finish();

      let expected = key as u64; // TODO?
      assert_eq!(hash, expected);
    }
}
