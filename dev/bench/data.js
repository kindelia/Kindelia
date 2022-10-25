window.BENCHMARK_DATA = {
  "lastUpdate": 1666705446611,
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
      },
      "7c8b1c8fb4224d098eaac3ec0f3c3a80329ba4e7": {
        "commit": {
          "author": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7c8b1c8fb4224d098eaac3ec0f3c3a80329ba4e7",
          "message": "Merge pull request #197 from Kindelia/continuous-benchmark\n\nAdds github action for continuous benchmark",
          "timestamp": "2022-10-25T10:29:06-03:00",
          "tree_id": "72c44257586252a64fdc8ba58b5a1d6f032f334e",
          "url": "https://github.com/Kindelia/Kindelia/commit/7c8b1c8fb4224d098eaac3ec0f3c3a80329ba4e7",
          "original_ref": "dev",
          "parent": "8f80194bf99bd8852d46c090683cb5fac98946a2"
        },
        "date": 1666705443745,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2812756,
            "range": "± 95721",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 43158,
            "range": "± 1419",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 45095,
            "range": "± 2688",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 193876,
            "range": "± 6493",
            "unit": "ns/iter"
          }
        ]
      }
    },
    "Simulation Benchmark": {
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
        "date": 1666388041003,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.14583333333333331,
            "unit": "Percent",
            "range": 0.029462782549439473
          },
          {
            "name": "Failed Mining",
            "value": 2046,
            "unit": "Logs",
            "range": 1132.7850634608492
          }
        ]
      },
      "7c8b1c8fb4224d098eaac3ec0f3c3a80329ba4e7": {
        "commit": {
          "author": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7c8b1c8fb4224d098eaac3ec0f3c3a80329ba4e7",
          "message": "Merge pull request #197 from Kindelia/continuous-benchmark\n\nAdds github action for continuous benchmark",
          "timestamp": "2022-10-25T10:29:06-03:00",
          "tree_id": "72c44257586252a64fdc8ba58b5a1d6f032f334e",
          "url": "https://github.com/Kindelia/Kindelia/commit/7c8b1c8fb4224d098eaac3ec0f3c3a80329ba4e7",
          "original_ref": "dev",
          "parent": "8f80194bf99bd8852d46c090683cb5fac98946a2"
        },
        "date": 1666705445901,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.17094017094017094,
            "unit": "Percent",
            "range": 0.08461106783428775
          },
          {
            "name": "Failed Mining",
            "value": 1835,
            "unit": "Logs",
            "range": 28.284271247461902
          }
        ]
      }
    }
  },
  "branches": {
    "continuous-benchmark": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
    "Kindelia:continuous-benchmark": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
    "dev": "7c8b1c8fb4224d098eaac3ec0f3c3a80329ba4e7"
  }
}