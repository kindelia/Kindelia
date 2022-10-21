window.BENCHMARK_DATA = {
  "lastUpdate": 1666384134764,
  "repoUrl": "https://github.com/Kindelia/Kindelia",
  "entries": {
    "Rust Benchmark": {
      "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5": {
        "commit": {
          "author": {
            "email": "rheidner.achiles@gmail.com",
            "name": "rheidner",
            "username": "racs4"
          },
          "committer": {
            "email": "rheidner.achiles@gmail.com",
            "name": "rheidner",
            "username": "racs4"
          },
          "distinct": true,
          "id": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
          "message": "testing gh-pages",
          "timestamp": "2022-10-21T17:21:44-03:00",
          "tree_id": "ecd2477a02a24a421027af227647d7ba1ea047d4",
          "url": "https://github.com/Kindelia/Kindelia/commit/0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
          "original_ref": "continuous-benchmark",
          "parent": "35cbaa5b7d6acf4c67339f5111587043539388b1"
        },
        "date": 1666384134301,
        "tool": "cargo",
        "benches": [
          {
            "name": "max_message_serialize",
            "value": 32471,
            "range": "± 1562",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 39677,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 159280,
            "range": "± 2749",
            "unit": "ns/iter"
          }
        ]
      }
    }
  },
  "branches": {
    "continuous-benchmark": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5"
  }
}