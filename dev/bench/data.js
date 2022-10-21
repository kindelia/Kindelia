window.BENCHMARK_DATA = {
  "lastUpdate": 1666388039842,
  "repoUrl": "https://github.com/Kindelia/Kindelia",
  "entries": {
    "Rust Benchmark": {
      "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5": {
        "commit": {
          "author": {
            "name": "Kindelia",
            "username": "Kindelia"
          },
          "committer": {
            "name": "Kindelia",
            "username": "Kindelia"
          },
          "id": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
          "message": "Adds github action for continuous benchmark",
          "timestamp": "2022-10-20T10:53:24Z",
          "url": "https://github.com/Kindelia/Kindelia/pull/197/commits/0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
          "original_ref": "Kindelia:continuous-benchmark"
        },
        "date": 1666388038874,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2668832,
            "range": "± 81246",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 39388,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 47662,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 190660,
            "range": "± 846",
            "unit": "ns/iter"
          }
        ]
      }
    },
    "Simulation Benchmark": {
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
        "date": 1666384135606,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.38636363636363635,
            "unit": "Percent",
            "range": 0.16070608663330627
          },
          {
            "name": "Failed Mining",
            "value": 1108.5,
            "unit": "Logs",
            "range": 997.7276682542185
          }
        ]
      }
    }
  },
  "branches": {
    "continuous-benchmark": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
    "Kindelia:continuous-benchmark": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5"
  }
}