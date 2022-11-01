window.BENCHMARK_DATA = {
  "lastUpdate": 1667339291671,
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
      },
      "905abfb0927169829d6a1230945b89abdb019574": {
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
          "id": "905abfb0927169829d6a1230945b89abdb019574",
          "message": "remove txs from mempool only on tip update",
          "timestamp": "2022-10-27T08:53:51-03:00",
          "tree_id": "ca5ee6a29f31f41aad527dceb9b925f26ef75eef",
          "url": "https://github.com/Kindelia/Kindelia/commit/905abfb0927169829d6a1230945b89abdb019574",
          "original_ref": "dev",
          "parent": "91b715f837604970900cdbc276b7aa3faf34b934"
        },
        "date": 1666872093670,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2088736,
            "range": "± 21259",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 33563,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 23742,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 109629,
            "range": "± 107",
            "unit": "ns/iter"
          }
        ]
      },
      "4e39261db41ff7fdec52e1ac5a0609c52d2abc52": {
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
          "id": "4e39261db41ff7fdec52e1ac5a0609c52d2abc52",
          "message": "do not unwrap on result of node request answer",
          "timestamp": "2022-10-27T16:10:10-03:00",
          "tree_id": "3a7402756ea284845ce1b1732b2c6fce3523bb9a",
          "url": "https://github.com/Kindelia/Kindelia/commit/4e39261db41ff7fdec52e1ac5a0609c52d2abc52",
          "original_ref": "dev",
          "parent": "905abfb0927169829d6a1230945b89abdb019574"
        },
        "date": 1666898277170,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2125837,
            "range": "± 63486",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 33780,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 23941,
            "range": "± 204",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 109911,
            "range": "± 81",
            "unit": "ns/iter"
          }
        ]
      },
      "cc4efdfa274e16ea5b6ee18c07cbc9796c4f7de8": {
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
          "id": "cc4efdfa274e16ea5b6ee18c07cbc9796c4f7de8",
          "message": "add rollback measures in `bench_simulation.py`",
          "timestamp": "2022-10-28T09:52:12-03:00",
          "tree_id": "9c09f1e37ababf5851d53d144db07ecaa3a98239",
          "url": "https://github.com/Kindelia/Kindelia/commit/cc4efdfa274e16ea5b6ee18c07cbc9796c4f7de8",
          "original_ref": "rollback-bench",
          "parent": "95ae9491cdba7d1ba69f1ef8bef1967ed8e8c993"
        },
        "date": 1666962294009,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2160552,
            "range": "± 9740",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 31698,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 27351,
            "range": "± 207",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 113971,
            "range": "± 133",
            "unit": "ns/iter"
          }
        ]
      },
      "d9064b1de64a9b92b04042889ee341f542b6c9cb": {
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
          "id": "d9064b1de64a9b92b04042889ee341f542b6c9cb",
          "message": "move 'test/' to 'kdl/'",
          "timestamp": "2022-10-28T13:57:42-03:00",
          "tree_id": "7baa87bab56f84c73ccb04aba4d44307fccc339f",
          "url": "https://github.com/Kindelia/Kindelia/commit/d9064b1de64a9b92b04042889ee341f542b6c9cb",
          "original_ref": "dev",
          "parent": "4e39261db41ff7fdec52e1ac5a0609c52d2abc52"
        },
        "date": 1666976972798,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2199947,
            "range": "± 21312",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 31068,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 27232,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 114017,
            "range": "± 102",
            "unit": "ns/iter"
          }
        ]
      },
      "5106a6006970f8ddb8f8b9de7d17a2a8bac166fe": {
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
          "id": "5106a6006970f8ddb8f8b9de7d17a2a8bac166fe",
          "message": "network simulation with 5 nodes",
          "timestamp": "2022-10-28T14:33:39-03:00",
          "tree_id": "c068f40c6b3ce7a75ec914dce4768597f2b011a7",
          "url": "https://github.com/Kindelia/Kindelia/commit/5106a6006970f8ddb8f8b9de7d17a2a8bac166fe",
          "original_ref": "rollback-bench",
          "parent": "cc4efdfa274e16ea5b6ee18c07cbc9796c4f7de8"
        },
        "date": 1666978898968,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2451578,
            "range": "± 123902",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 35860,
            "range": "± 1859",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 26182,
            "range": "± 1583",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 118863,
            "range": "± 6036",
            "unit": "ns/iter"
          }
        ]
      },
      "0bd087ed715d543fc310eb64335698b54f5b56ae": {
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
          "id": "0bd087ed715d543fc310eb64335698b54f5b56ae",
          "message": "write block file in separate thread",
          "timestamp": "2022-10-28T14:58:17-03:00",
          "tree_id": "e56186ec91a025b661c439a622bd7ddcdc779fad",
          "url": "https://github.com/Kindelia/Kindelia/commit/0bd087ed715d543fc310eb64335698b54f5b56ae",
          "original_ref": "thread-file",
          "parent": "0000000000000000000000000000000000000000"
        },
        "date": 1666980630666,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 1874673,
            "range": "± 11834",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 27359,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 24280,
            "range": "± 344",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 100600,
            "range": "± 224",
            "unit": "ns/iter"
          }
        ]
      },
      "2c7da30bb2056db87d48901a753aaa76f2dff8fa": {
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
          "id": "2c7da30bb2056db87d48901a753aaa76f2dff8fa",
          "message": "Merge pull request #202 from Kindelia/rollback-bench\n\nRollback bench",
          "timestamp": "2022-10-28T15:03:39-03:00",
          "tree_id": "5f182182df911e590e1b0d32cf25e1dad031eb47",
          "url": "https://github.com/Kindelia/Kindelia/commit/2c7da30bb2056db87d48901a753aaa76f2dff8fa",
          "original_ref": "dev",
          "parent": "d9064b1de64a9b92b04042889ee341f542b6c9cb"
        },
        "date": 1666980661263,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2188249,
            "range": "± 14453",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 32177,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 27445,
            "range": "± 615",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 114730,
            "range": "± 162",
            "unit": "ns/iter"
          }
        ]
      },
      "cb92ca1e3ee30109eaed37dec98f80cf04813e7d": {
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
          "id": "cb92ca1e3ee30109eaed37dec98f80cf04813e7d",
          "message": "add todo",
          "timestamp": "2022-10-28T17:57:52-03:00",
          "tree_id": "4f63c4bb2f1efda8039ed5128a443be81d26dd58",
          "url": "https://github.com/Kindelia/Kindelia/commit/cb92ca1e3ee30109eaed37dec98f80cf04813e7d",
          "original_ref": "dev",
          "parent": "2c7da30bb2056db87d48901a753aaa76f2dff8fa"
        },
        "date": 1666991272986,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2197882,
            "range": "± 11748",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 32068,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 27361,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 114530,
            "range": "± 129",
            "unit": "ns/iter"
          }
        ]
      },
      "f6cd027297077e214d35c33e7c9b473b33f35bb6": {
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
          "id": "f6cd027297077e214d35c33e7c9b473b33f35bb6",
          "message": "change `chain` field of heartbeat to `tip_blocks`\n\n- now it only shows the last 10 blocks of the chain, starting with the tip",
          "timestamp": "2022-10-28T18:16:18-03:00",
          "tree_id": "9894fe1fed566a7eeb42c4a08d4f0c0fcd4d8f12",
          "url": "https://github.com/Kindelia/Kindelia/commit/f6cd027297077e214d35c33e7c9b473b33f35bb6",
          "original_ref": "dev",
          "parent": "cb92ca1e3ee30109eaed37dec98f80cf04813e7d"
        },
        "date": 1666992249234,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2190197,
            "range": "± 6319",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 41913,
            "range": "± 198",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 28399,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 118143,
            "range": "± 90",
            "unit": "ns/iter"
          }
        ]
      },
      "d51d3f6e493041641f46469fe125ab6a22f50f22": {
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
          "id": "d51d3f6e493041641f46469fe125ab6a22f50f22",
          "message": "change file writing in node\n\na trait was created in `persistence.rs` to decouple the node from the block file writting",
          "timestamp": "2022-10-31T15:58:39-03:00",
          "tree_id": "6cfc7aa7cbca7930c80ebfe4f59fa398860c0149",
          "url": "https://github.com/Kindelia/Kindelia/commit/d51d3f6e493041641f46469fe125ab6a22f50f22",
          "original_ref": "thread-file",
          "parent": "0bd087ed715d543fc310eb64335698b54f5b56ae"
        },
        "date": 1667243189606,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 2466359,
            "range": "± 72491",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 47306,
            "range": "± 2304",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 32595,
            "range": "± 1112",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 137552,
            "range": "± 4895",
            "unit": "ns/iter"
          }
        ]
      },
      "cc230f03466d02026ebe8bb0a7a1abdb45328ff3": {
        "commit": {
          "author": {
            "email": "leonardo.ribeiro.santiago@gmail.com",
            "name": "santi",
            "username": "o-santi"
          },
          "committer": {
            "email": "leonardo.ribeiro.santiago@gmail.com",
            "name": "santi",
            "username": "o-santi"
          },
          "distinct": true,
          "id": "cc230f03466d02026ebe8bb0a7a1abdb45328ff3",
          "message": "adding Loc and RawCell types",
          "timestamp": "2022-11-01T12:50:42-03:00",
          "tree_id": "2ad7958ba922776f1956253b0fdaf9a13d4f9290",
          "url": "https://github.com/Kindelia/Kindelia/commit/cc230f03466d02026ebe8bb0a7a1abdb45328ff3",
          "original_ref": "dev",
          "parent": "f6cd027297077e214d35c33e7c9b473b33f35bb6"
        },
        "date": 1667318304696,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 1720184,
            "range": "± 24586",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 31554,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 27568,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 113789,
            "range": "± 180",
            "unit": "ns/iter"
          }
        ]
      },
      "84d820a8b1cb7b36acb87489c35ce252669d5d71": {
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
          "id": "84d820a8b1cb7b36acb87489c35ce252669d5d71",
          "message": "bump version to 0.1.5",
          "timestamp": "2022-11-01T18:35:05-03:00",
          "tree_id": "4120f31b513f7828e92e207c7dc73a5860c28e49",
          "url": "https://github.com/Kindelia/Kindelia/commit/84d820a8b1cb7b36acb87489c35ce252669d5d71",
          "original_ref": "dev",
          "parent": "cc230f03466d02026ebe8bb0a7a1abdb45328ff3"
        },
        "date": 1667339290777,
        "tool": "cargo",
        "benches": [
          {
            "name": "kvm_tree_sum",
            "value": 1723890,
            "range": "± 36662",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_serialize",
            "value": 33845,
            "range": "± 195",
            "unit": "ns/iter"
          },
          {
            "name": "max_message_deserialize",
            "value": 24059,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "deserialize_block_with_txs",
            "value": 110819,
            "range": "± 135",
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
      },
      "905abfb0927169829d6a1230945b89abdb019574": {
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
          "id": "905abfb0927169829d6a1230945b89abdb019574",
          "message": "remove txs from mempool only on tip update",
          "timestamp": "2022-10-27T08:53:51-03:00",
          "tree_id": "ca5ee6a29f31f41aad527dceb9b925f26ef75eef",
          "url": "https://github.com/Kindelia/Kindelia/commit/905abfb0927169829d6a1230945b89abdb019574",
          "original_ref": "dev",
          "parent": "91b715f837604970900cdbc276b7aa3faf34b934"
        },
        "date": 1666872095256,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.09313725490196079,
            "unit": "Percent",
            "range": 0.03466209711698762
          },
          {
            "name": "Failed Mining",
            "value": 1001,
            "unit": "Logs",
            "range": 9.899494936611665
          }
        ]
      },
      "4e39261db41ff7fdec52e1ac5a0609c52d2abc52": {
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
          "id": "4e39261db41ff7fdec52e1ac5a0609c52d2abc52",
          "message": "do not unwrap on result of node request answer",
          "timestamp": "2022-10-27T16:10:10-03:00",
          "tree_id": "3a7402756ea284845ce1b1732b2c6fce3523bb9a",
          "url": "https://github.com/Kindelia/Kindelia/commit/4e39261db41ff7fdec52e1ac5a0609c52d2abc52",
          "original_ref": "dev",
          "parent": "905abfb0927169829d6a1230945b89abdb019574"
        },
        "date": 1666898278660,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.055712669683257915,
            "unit": "Percent",
            "range": 0.016397838251497987
          },
          {
            "name": "Failed Mining",
            "value": 945,
            "unit": "Logs",
            "range": 59.39696961966999
          }
        ]
      },
      "cc4efdfa274e16ea5b6ee18c07cbc9796c4f7de8": {
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
          "id": "cc4efdfa274e16ea5b6ee18c07cbc9796c4f7de8",
          "message": "add rollback measures in `bench_simulation.py`",
          "timestamp": "2022-10-28T09:52:12-03:00",
          "tree_id": "9c09f1e37ababf5851d53d144db07ecaa3a98239",
          "url": "https://github.com/Kindelia/Kindelia/commit/cc4efdfa274e16ea5b6ee18c07cbc9796c4f7de8",
          "original_ref": "rollback-bench",
          "parent": "95ae9491cdba7d1ba69f1ef8bef1967ed8e8c993"
        },
        "date": 1666962295605,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.08028743976600804,
            "unit": "Percent",
            "range": 0.003764451226568119
          },
          {
            "name": "Failed Mining",
            "value": 925.5,
            "unit": "Logs",
            "range": 2.1213203435596424
          },
          {
            "name": "Rollback count in node 0",
            "value": 5,
            "unit": "rollbacks",
            "range": 1.4142135623730951
          },
          {
            "name": "Rollback distance mean in node 0",
            "value": 1,
            "unit": "blocks",
            "range": 0
          },
          {
            "name": "Real rollback distance mean in node 0",
            "value": 7.083333333333333,
            "unit": "blocks",
            "range": 1.5320646925708525
          },
          {
            "name": "Blocks between rollbacks in node 0",
            "value": 12.5,
            "unit": "blocks",
            "range": 2.1213203435596424
          },
          {
            "name": "Computed blocks in node 0",
            "value": 131,
            "unit": "blocks",
            "range": 14.142135623730951
          },
          {
            "name": "Height of node 0",
            "value": 92.5,
            "unit": "blocks",
            "range": 0.7071067811865476
          },
          {
            "name": "Mean height",
            "value": 90.75,
            "unit": "blocks",
            "range": 3.181980515339464
          }
        ]
      },
      "d9064b1de64a9b92b04042889ee341f542b6c9cb": {
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
          "id": "d9064b1de64a9b92b04042889ee341f542b6c9cb",
          "message": "move 'test/' to 'kdl/'",
          "timestamp": "2022-10-28T13:57:42-03:00",
          "tree_id": "7baa87bab56f84c73ccb04aba4d44307fccc339f",
          "url": "https://github.com/Kindelia/Kindelia/commit/d9064b1de64a9b92b04042889ee341f542b6c9cb",
          "original_ref": "dev",
          "parent": "4e39261db41ff7fdec52e1ac5a0609c52d2abc52"
        },
        "date": 1666976974263,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.07950541087178632,
            "unit": "Percent",
            "range": 0.034265021361528314
          },
          {
            "name": "Failed Mining",
            "value": 881,
            "unit": "Logs",
            "range": 1.4142135623730951
          }
        ]
      },
      "5106a6006970f8ddb8f8b9de7d17a2a8bac166fe": {
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
          "id": "5106a6006970f8ddb8f8b9de7d17a2a8bac166fe",
          "message": "network simulation with 5 nodes",
          "timestamp": "2022-10-28T14:33:39-03:00",
          "tree_id": "c068f40c6b3ce7a75ec914dce4768597f2b011a7",
          "url": "https://github.com/Kindelia/Kindelia/commit/5106a6006970f8ddb8f8b9de7d17a2a8bac166fe",
          "original_ref": "rollback-bench",
          "parent": "cc4efdfa274e16ea5b6ee18c07cbc9796c4f7de8"
        },
        "date": 1666978902374,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.18220306906177552,
            "unit": "Percent",
            "range": 0.00953596619604488
          },
          {
            "name": "Failed Mining",
            "value": 2315.5,
            "unit": "Logs",
            "range": 14.849242404917497
          },
          {
            "name": "Rollback count in node 0",
            "value": 23.5,
            "unit": "rollbacks",
            "range": 4.949747468305833
          },
          {
            "name": "Rollback distance mean in node 0",
            "value": 2.0574074074074074,
            "unit": "blocks",
            "range": 0.3430777345756951
          },
          {
            "name": "Real rollback distance mean in node 0",
            "value": 9.630555555555556,
            "unit": "blocks",
            "range": 3.5630325029788814
          },
          {
            "name": "Blocks between rollbacks in node 0",
            "value": 4.408906882591093,
            "unit": "blocks",
            "range": 0.6870673177521108
          },
          {
            "name": "Computed blocks in node 0",
            "value": 320.5,
            "unit": "blocks",
            "range": 37.476659402887016
          },
          {
            "name": "Height of node 0",
            "value": 99,
            "unit": "blocks",
            "range": 2.8284271247461903
          },
          {
            "name": "Mean height",
            "value": 99,
            "unit": "blocks",
            "range": 2.8284271247461903
          }
        ]
      },
      "0bd087ed715d543fc310eb64335698b54f5b56ae": {
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
          "id": "0bd087ed715d543fc310eb64335698b54f5b56ae",
          "message": "write block file in separate thread",
          "timestamp": "2022-10-28T14:58:17-03:00",
          "tree_id": "e56186ec91a025b661c439a622bd7ddcdc779fad",
          "url": "https://github.com/Kindelia/Kindelia/commit/0bd087ed715d543fc310eb64335698b54f5b56ae",
          "original_ref": "thread-file",
          "parent": "0000000000000000000000000000000000000000"
        },
        "date": 1666980632145,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.07508116883116883,
            "unit": "Percent",
            "range": 0.030419366398447254
          },
          {
            "name": "Failed Mining",
            "value": 888.5,
            "unit": "Logs",
            "range": 4.949747468305833
          }
        ]
      },
      "2c7da30bb2056db87d48901a753aaa76f2dff8fa": {
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
          "id": "2c7da30bb2056db87d48901a753aaa76f2dff8fa",
          "message": "Merge pull request #202 from Kindelia/rollback-bench\n\nRollback bench",
          "timestamp": "2022-10-28T15:03:39-03:00",
          "tree_id": "5f182182df911e590e1b0d32cf25e1dad031eb47",
          "url": "https://github.com/Kindelia/Kindelia/commit/2c7da30bb2056db87d48901a753aaa76f2dff8fa",
          "original_ref": "dev",
          "parent": "d9064b1de64a9b92b04042889ee341f542b6c9cb"
        },
        "date": 1666980663307,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.17773744266114688,
            "unit": "Percent",
            "range": 0.0047587148548216395
          },
          {
            "name": "Failed Mining",
            "value": 2369,
            "unit": "Logs",
            "range": 1.4142135623730951
          },
          {
            "name": "Rollback count in node 0",
            "value": 17.5,
            "unit": "rollbacks",
            "range": 0.7071067811865476
          },
          {
            "name": "Rollback distance mean in node 0",
            "value": 1.7826797385620914,
            "unit": "blocks",
            "range": 0.5569043603462678
          },
          {
            "name": "Real rollback distance mean in node 0",
            "value": 13.782679738562091,
            "unit": "blocks",
            "range": 0.5569043603462671
          },
          {
            "name": "Blocks between rollbacks in node 0",
            "value": 6.325367647058823,
            "unit": "blocks",
            "range": 1.042462570793403
          },
          {
            "name": "Computed blocks in node 0",
            "value": 343,
            "unit": "blocks",
            "range": 0
          },
          {
            "name": "Height of node 0",
            "value": 98,
            "unit": "blocks",
            "range": 0
          },
          {
            "name": "Mean height",
            "value": 98,
            "unit": "blocks",
            "range": 0
          }
        ]
      },
      "cb92ca1e3ee30109eaed37dec98f80cf04813e7d": {
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
          "id": "cb92ca1e3ee30109eaed37dec98f80cf04813e7d",
          "message": "add todo",
          "timestamp": "2022-10-28T17:57:52-03:00",
          "tree_id": "4f63c4bb2f1efda8039ed5128a443be81d26dd58",
          "url": "https://github.com/Kindelia/Kindelia/commit/cb92ca1e3ee30109eaed37dec98f80cf04813e7d",
          "original_ref": "dev",
          "parent": "2c7da30bb2056db87d48901a753aaa76f2dff8fa"
        },
        "date": 1666991274326,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.2020438290849074,
            "unit": "Percent",
            "range": 0.02107315661298843
          },
          {
            "name": "Failed Mining",
            "value": 2514,
            "unit": "Logs",
            "range": 69.29646455628166
          },
          {
            "name": "Rollback count in node 0",
            "value": 19.5,
            "unit": "rollbacks",
            "range": 12.020815280171307
          },
          {
            "name": "Rollback distance mean in node 0",
            "value": 1.8571428571428572,
            "unit": "blocks",
            "range": 0.20203050891044222
          },
          {
            "name": "Real rollback distance mean in node 0",
            "value": 12.297077922077921,
            "unit": "blocks",
            "range": 4.208203668555006
          },
          {
            "name": "Blocks between rollbacks in node 0",
            "value": 5.462962962962963,
            "unit": "blocks",
            "range": 2.173698623647535
          },
          {
            "name": "Computed blocks in node 0",
            "value": 311,
            "unit": "blocks",
            "range": 70.71067811865476
          },
          {
            "name": "Height of node 0",
            "value": 90.5,
            "unit": "blocks",
            "range": 3.5355339059327378
          },
          {
            "name": "Mean height",
            "value": 90.5,
            "unit": "blocks",
            "range": 3.5355339059327378
          }
        ]
      },
      "f6cd027297077e214d35c33e7c9b473b33f35bb6": {
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
          "id": "f6cd027297077e214d35c33e7c9b473b33f35bb6",
          "message": "change `chain` field of heartbeat to `tip_blocks`\n\n- now it only shows the last 10 blocks of the chain, starting with the tip",
          "timestamp": "2022-10-28T18:16:18-03:00",
          "tree_id": "9894fe1fed566a7eeb42c4a08d4f0c0fcd4d8f12",
          "url": "https://github.com/Kindelia/Kindelia/commit/f6cd027297077e214d35c33e7c9b473b33f35bb6",
          "original_ref": "dev",
          "parent": "cb92ca1e3ee30109eaed37dec98f80cf04813e7d"
        },
        "date": 1666992251147,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.1505026633433054,
            "unit": "Percent",
            "range": 0.007107682829366826
          },
          {
            "name": "Failed Mining",
            "value": 2480,
            "unit": "Logs",
            "range": 36.76955262170047
          },
          {
            "name": "Rollback count in node 0",
            "value": 12.5,
            "unit": "rollbacks",
            "range": 0.7071067811865476
          },
          {
            "name": "Rollback distance mean in node 0",
            "value": 1.7756410256410255,
            "unit": "blocks",
            "range": 0.5529937647740948
          },
          {
            "name": "Real rollback distance mean in node 0",
            "value": 10.307692307692307,
            "unit": "blocks",
            "range": 3.8074980525429485
          },
          {
            "name": "Blocks between rollbacks in node 0",
            "value": 7.477272727272727,
            "unit": "blocks",
            "range": 0.03214121732666154
          },
          {
            "name": "Computed blocks in node 0",
            "value": 229,
            "unit": "blocks",
            "range": 45.254833995939045
          },
          {
            "name": "Height of node 0",
            "value": 97,
            "unit": "blocks",
            "range": 4.242640687119285
          },
          {
            "name": "Mean height",
            "value": 97,
            "unit": "blocks",
            "range": 4.242640687119285
          }
        ]
      },
      "d51d3f6e493041641f46469fe125ab6a22f50f22": {
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
          "id": "d51d3f6e493041641f46469fe125ab6a22f50f22",
          "message": "change file writing in node\n\na trait was created in `persistence.rs` to decouple the node from the block file writting",
          "timestamp": "2022-10-31T15:58:39-03:00",
          "tree_id": "6cfc7aa7cbca7930c80ebfe4f59fa398860c0149",
          "url": "https://github.com/Kindelia/Kindelia/commit/d51d3f6e493041641f46469fe125ab6a22f50f22",
          "original_ref": "thread-file",
          "parent": "0bd087ed715d543fc310eb64335698b54f5b56ae"
        },
        "date": 1667243191640,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.09468431513511227,
            "unit": "Percent",
            "range": 0.03791649674565663
          },
          {
            "name": "Failed Mining",
            "value": 570.5,
            "unit": "Logs",
            "range": 16.263455967290593
          }
        ]
      },
      "cc230f03466d02026ebe8bb0a7a1abdb45328ff3": {
        "commit": {
          "author": {
            "email": "leonardo.ribeiro.santiago@gmail.com",
            "name": "santi",
            "username": "o-santi"
          },
          "committer": {
            "email": "leonardo.ribeiro.santiago@gmail.com",
            "name": "santi",
            "username": "o-santi"
          },
          "distinct": true,
          "id": "cc230f03466d02026ebe8bb0a7a1abdb45328ff3",
          "message": "adding Loc and RawCell types",
          "timestamp": "2022-11-01T12:50:42-03:00",
          "tree_id": "2ad7958ba922776f1956253b0fdaf9a13d4f9290",
          "url": "https://github.com/Kindelia/Kindelia/commit/cc230f03466d02026ebe8bb0a7a1abdb45328ff3",
          "original_ref": "dev",
          "parent": "f6cd027297077e214d35c33e7c9b473b33f35bb6"
        },
        "date": 1667318306555,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Uncle Rate",
            "value": 0.15555073215818124,
            "unit": "Percent",
            "range": 0.04624105505424281
          },
          {
            "name": "Failed Mining",
            "value": 2509,
            "unit": "Logs",
            "range": 56.568542494923804
          },
          {
            "name": "Rollback count in node 0",
            "value": 18,
            "unit": "rollbacks",
            "range": 8.48528137423857
          },
          {
            "name": "Rollback distance mean in node 0",
            "value": 2.729166666666667,
            "unit": "blocks",
            "range": 0.7365695637359869
          },
          {
            "name": "Real rollback distance mean in node 0",
            "value": 8.25,
            "unit": "blocks",
            "range": 1.5320646925708532
          },
          {
            "name": "Blocks between rollbacks in node 0",
            "value": 5.3023715415019765,
            "unit": "blocks",
            "range": 3.6864578829448864
          },
          {
            "name": "Computed blocks in node 0",
            "value": 261.5,
            "unit": "blocks",
            "range": 103.94469683442249
          },
          {
            "name": "Height of node 0",
            "value": 100.5,
            "unit": "blocks",
            "range": 6.363961030678928
          },
          {
            "name": "Mean height",
            "value": 100.5,
            "unit": "blocks",
            "range": 6.363961030678928
          }
        ]
      }
    }
  },
  "branches": {
    "continuous-benchmark": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
    "Kindelia:continuous-benchmark": "0c88c996a5b3abded9f483b91ff1fbe21ac69ad5",
    "dev": "84d820a8b1cb7b36acb87489c35ce252669d5d71",
    "rollback-bench": "5106a6006970f8ddb8f8b9de7d17a2a8bac166fe",
    "thread-file": "d51d3f6e493041641f46469fe125ab6a22f50f22"
  }
}