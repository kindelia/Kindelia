window.BENCHMARK_DATA = {
  "lastUpdate": 1666789229665,
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
      },
      "c1346b9e82e91e73c8023941d25f7744cc604e10": {
        "commit": {
          "author": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "committer": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "distinct": true,
          "id": "c1346b9e82e91e73c8023941d25f7744cc604e10",
          "message": "reverse TX_COUNT bits",
          "timestamp": "2022-10-25T15:45:11-03:00",
          "tree_id": "71b79069a15133b9173f931b4f450cee63e806fd",
          "url": "https://github.com/Kindelia/Kindelia/commit/c1346b9e82e91e73c8023941d25f7744cc604e10",
          "original_ref": "dev",
          "parent": "7c8b1c8fb4224d098eaac3ec0f3c3a80329ba4e7"
        },
        "date": 1666724433542,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2187940,
            "range": "± 23504",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 32898,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 39798,
            "range": "± 142",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 159164,
            "range": "± 226",
            "unit": "ns/iter"
          }
        ]
      },
      "830d3b30b6be9f720ba9e0b1d7426c63d5214298": {
        "commit": {
          "author": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "committer": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "distinct": true,
          "id": "830d3b30b6be9f720ba9e0b1d7426c63d5214298",
          "message": "fix spred.sh",
          "timestamp": "2022-10-25T16:25:13-03:00",
          "tree_id": "bf41c4ef055a7b0c4a439b43c7ea0bb95bf017eb",
          "url": "https://github.com/Kindelia/Kindelia/commit/830d3b30b6be9f720ba9e0b1d7426c63d5214298",
          "original_ref": "dev",
          "parent": "c1346b9e82e91e73c8023941d25f7744cc604e10"
        },
        "date": 1666726387180,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2820173,
            "range": "± 21021",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 41220,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 49857,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 200172,
            "range": "± 127",
            "unit": "ns/iter"
          }
        ]
      },
      "c1756cdaaf587bc4b08c477adf9518c3d45267d5": {
        "commit": {
          "author": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "committer": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "distinct": true,
          "id": "c1756cdaaf587bc4b08c477adf9518c3d45267d5",
          "message": "slow mining sleep done on `try_mine` elapsed time\n\ninstead of on successful mine event",
          "timestamp": "2022-10-26T09:11:59-03:00",
          "tree_id": "44d0e9dde93d9073720f6f0a73e53c9f794cfa26",
          "url": "https://github.com/Kindelia/Kindelia/commit/c1756cdaaf587bc4b08c477adf9518c3d45267d5",
          "original_ref": "dev",
          "parent": "830d3b30b6be9f720ba9e0b1d7426c63d5214298"
        },
        "date": 1666786788951,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2627493,
            "range": "± 46637",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 37479,
            "range": "± 278",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 32981,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 135359,
            "range": "± 124",
            "unit": "ns/iter"
          }
        ]
      },
      "91b715f837604970900cdbc276b7aa3faf34b934": {
        "commit": {
          "author": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "committer": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "distinct": true,
          "id": "91b715f837604970900cdbc276b7aa3faf34b934",
          "message": "increase block build frequency",
          "timestamp": "2022-10-26T09:35:04-03:00",
          "tree_id": "21425349f446f71d6b7cd47c192a8d48b6de9c9f",
          "url": "https://github.com/Kindelia/Kindelia/commit/91b715f837604970900cdbc276b7aa3faf34b934",
          "original_ref": "dev",
          "parent": "c1756cdaaf587bc4b08c477adf9518c3d45267d5"
        },
        "date": 1666789225914,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2625176,
            "range": "± 12887",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 37458,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 32971,
            "range": "± 145",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 134431,
            "range": "± 347",
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
      },
      "c1346b9e82e91e73c8023941d25f7744cc604e10": {
        "commit": {
          "author": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "committer": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "distinct": true,
          "id": "c1346b9e82e91e73c8023941d25f7744cc604e10",
          "message": "reverse TX_COUNT bits",
          "timestamp": "2022-10-25T15:45:11-03:00",
          "tree_id": "71b79069a15133b9173f931b4f450cee63e806fd",
          "url": "https://github.com/Kindelia/Kindelia/commit/c1346b9e82e91e73c8023941d25f7744cc604e10",
          "original_ref": "dev",
          "parent": "7c8b1c8fb4224d098eaac3ec0f3c3a80329ba4e7"
        },
        "date": 1666724435070,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.29166666666666663,
            "unit": "Percent",
            "range": 0.058925565098878946
          },
          {
            "name": "Failed Mining",
            "value": 1832.5,
            "unit": "Logs",
            "range": 383.9589821842953
          }
        ]
      },
      "830d3b30b6be9f720ba9e0b1d7426c63d5214298": {
        "commit": {
          "author": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "committer": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "distinct": true,
          "id": "830d3b30b6be9f720ba9e0b1d7426c63d5214298",
          "message": "fix spred.sh",
          "timestamp": "2022-10-25T16:25:13-03:00",
          "tree_id": "bf41c4ef055a7b0c4a439b43c7ea0bb95bf017eb",
          "url": "https://github.com/Kindelia/Kindelia/commit/830d3b30b6be9f720ba9e0b1d7426c63d5214298",
          "original_ref": "dev",
          "parent": "c1346b9e82e91e73c8023941d25f7744cc604e10"
        },
        "date": 1666726389283,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.18333333333333335,
            "unit": "Percent",
            "range": 0.023570226039551598
          },
          {
            "name": "Failed Mining",
            "value": 1808.5,
            "unit": "Logs",
            "range": 81.31727983645297
          }
        ]
      },
      "c1756cdaaf587bc4b08c477adf9518c3d45267d5": {
        "commit": {
          "author": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "committer": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "distinct": true,
          "id": "c1756cdaaf587bc4b08c477adf9518c3d45267d5",
          "message": "slow mining sleep done on `try_mine` elapsed time\n\ninstead of on successful mine event",
          "timestamp": "2022-10-26T09:11:59-03:00",
          "tree_id": "44d0e9dde93d9073720f6f0a73e53c9f794cfa26",
          "url": "https://github.com/Kindelia/Kindelia/commit/c1756cdaaf587bc4b08c477adf9518c3d45267d5",
          "original_ref": "dev",
          "parent": "830d3b30b6be9f720ba9e0b1d7426c63d5214298"
        },
        "date": 1666786790350,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.43246869409660105,
            "unit": "Percent",
            "range": 0.013281969950731222
          },
          {
            "name": "Failed Mining",
            "value": 2,
            "unit": "Logs",
            "range": 0
          }
        ]
      },
      "91b715f837604970900cdbc276b7aa3faf34b934": {
        "commit": {
          "author": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "committer": {
            "email": "me@steinerkelvin.dev",
            "name": "Kelvin Steiner",
            "username": "steinerkelvin"
          },
          "distinct": true,
          "id": "91b715f837604970900cdbc276b7aa3faf34b934",
          "message": "increase block build frequency",
          "timestamp": "2022-10-26T09:35:04-03:00",
          "tree_id": "21425349f446f71d6b7cd47c192a8d48b6de9c9f",
          "url": "https://github.com/Kindelia/Kindelia/commit/91b715f837604970900cdbc276b7aa3faf34b934",
          "original_ref": "dev",
          "parent": "c1756cdaaf587bc4b08c477adf9518c3d45267d5"
        },
        "date": 1666789227984,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.10518543956043956,
            "unit": "Percent",
            "range": 0.0035452469111688237
          },
          {
            "name": "Failed Mining",
            "value": 617.5,
            "unit": "Logs",
            "range": 26.16295090390226
          }
        ]
      }
    }
  },
  "branches": {
    "continuous-benchmark": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
    "Kindelia:continuous-benchmark": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
    "dev": "91b715f837604970900cdbc276b7aa3faf34b934"
  }
}