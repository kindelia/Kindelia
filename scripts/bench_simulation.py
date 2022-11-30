#!/usr/bin/env python3

import argparse
from audioop import add
import json
import statistics
import sys
import os
import signal
from abc import ABC, abstractmethod
from dataclasses import dataclass
import subprocess
import time
from typing import List


@dataclass
class BencherInfo:
    name: str
    unit: str


class Bencher(ABC):
    def __init__(self):
        super().__init__()
        self.results = []

    @abstractmethod
    def info(self) -> BencherInfo:
        pass

    @abstractmethod
    def calculate(self, event, addr):
        pass

    # TODO: add generic type?
    @abstractmethod
    def get_result(self):
        pass

    @abstractmethod
    def clear(self):
        pass

    def store_result(self):
        self.results.append(self.get_result())

    def get_results(self):
        return self.results


class UncleRate(Bencher):
    def __init__(self):
        super().__init__()
        self.total: float = 0
        self.uncle: float = 0

    def info(self) -> BencherInfo:
        return BencherInfo(name="Uncle Rate", unit="Percent")

    def calculate(self, event, addr):
        if 'AddBlock' in event:
            data = event['AddBlock']
            if type(data['event']) is dict and 'Included' in data['event']:
                data = data['event']['Included']
                self.total += 1
                if len(data['siblings']) != 0:
                    self.uncle += 1

    def get_result(self):
        if self.total != 0:
            return self.uncle / self.total
        else:
            return self.total

    def clear(self):
        self.total = 0.
        self.uncle = 0.


class RollbackCount(Bencher):
    def __init__(self, addr):
        super().__init__()
        self.total: float = 0.
        self.addr = addr

    def info(self) -> BencherInfo:
        return BencherInfo(name=f"Rollback count in node {self.addr}", unit="rollbacks")

    def calculate(self, event, addr):
        if addr == self.addr:
            if 'AddBlock' in event:
                data = event['AddBlock']
                if 'Reorg' in data['event']:
                    data = data['event']['Reorg']
                    if data['rollback'] is not None:
                        self.total += 1

    def get_result(self):
        return self.total

    def clear(self):
        self.total = 0.


class RollbackDistanceMean(Bencher):
    def __init__(self, addr):
        super().__init__()
        self.total: float = 0.
        self.sum: float = 0.
        self.addr = addr

    def info(self) -> BencherInfo:
        return BencherInfo(name=f"Rollback distance mean in node {self.addr}", unit="blocks")

    def calculate(self, event, addr):
        if addr == self.addr:
            if 'AddBlock' in event:
                data = event['AddBlock']
                if 'Reorg' in data['event']:
                    data = data['event']['Reorg']
                    if data['rollback'] is not None:
                        self.total += 1
                        self.sum += data['rollback']['runtime_tick'] - \
                            data['rollback']['common_tick']

    def get_result(self):
        return 0. if self.total == 0 else self.sum / self.total

    def clear(self):
        self.total = 0.
        self.sum = 0.


class RealRollbackDistanceMean(Bencher):
    def __init__(self, addr):
        super().__init__()
        self.total: float = 0.
        self.sum: float = 0.
        self.addr = addr

    def info(self) -> BencherInfo:
        return BencherInfo(name=f"Real rollback distance mean in node {self.addr}", unit="blocks")

    def calculate(self, event, addr):
        if addr == self.addr:
            if 'AddBlock' in event:
                data = event['AddBlock']
                if 'Reorg' in data['event']:
                    data = data['event']['Reorg']
                    if data['rollback'] is not None:
                        self.total += 1
                        self.sum += data['rollback']['runtime_tick'] - \
                            data['rollback']['rolled_to']

    def get_result(self):
        return 0. if self.total == 0 else self.sum / self.total

    def clear(self):
        self.total = 0.
        self.sum = 0.


class BlocksBetweenRollbacks(Bencher):
    def __init__(self, addr):
        super().__init__()
        self.addr = addr
        self.last_height = -1
        self.total = 0.
        self.sum = 0.

    def info(self) -> BencherInfo:
        return BencherInfo(name=f"Blocks between rollbacks in node {self.addr}", unit="blocks")

    def calculate(self, event, addr):
        if addr == self.addr:
            if 'AddBlock' in event:
                data = event['AddBlock']
                if 'Reorg' in data['event']:
                    data = data['event']['Reorg']
                    if data['rollback'] is not None:
                        if self.last_height != -1:
                            distance = abs(data['old_tip']['height'] -
                                           self.last_height)
                            self.sum += distance
                            self.total += 1
                        self.last_height = data['old_tip']['height']

    def get_result(self):
        return 0. if self.total == 0 else self.sum / self.total

    def clear(self):
        self.last_height = -1
        self.total = 0.
        self.sum = 0.


class ComputedBlocks(Bencher):
    def __init__(self, addr):
        super().__init__()
        self.total = 0.
        self.addr = addr

    def info(self) -> BencherInfo:
        return BencherInfo(name=f"Computed blocks in node {self.addr}", unit="blocks")

    def calculate(self, event, addr):
        if addr == self.addr:
            if 'AddBlock' in event:
                data = event['AddBlock']
                if 'ComputedBlock' in data['event']:
                    data = data['event']['ComputedBlock']
                    self.total += 1

    def get_result(self):
        return self.total

    def clear(self):
        self.total = 0.


class Height(Bencher):
    def __init__(self, addr):
        super().__init__()
        self.height = 0
        self.addr = addr

    def info(self) -> BencherInfo:
        return BencherInfo(name=f"Height of node {self.addr}", unit="blocks")

    def calculate(self, event, addr):
        if 'Heartbeat' in event:
            data = event['Heartbeat']
            height = data['tip']['height']
            self.height = height

    def get_result(self):
        return self.height

    def clear(self):
        self.height = 0


class MeanHeight(Bencher):
    def __init__(self):
        super().__init__()
        self.heights = {}

    def info(self) -> BencherInfo:
        return BencherInfo(name="Mean height", unit="blocks")

    def calculate(self, event, addr):
        if 'Heartbeat' in event:
            data = event['Heartbeat']
            height = data['tip']['height']
            self.heights[addr] = height

    def get_result(self):
        return statistics.fmean(self.heights.values())

    def clear(self):
        self.heights = {}


class FailedMining(Bencher):
    def __init__(self):
        super().__init__()
        self.total: int = 0

    def info(self) -> BencherInfo:
        return BencherInfo(name="Failed Mining", unit="Logs")

    def calculate(self, event, addr):
        if 'Mining' in event:
            data = event['Mining']
            if 'Failure' in data['event']:
                self.total += 1

    def get_result(self):
        return self.total

    def clear(self):
        self.total = 0


# ============================================================
# Run

@dataclass
class RunConfig:
    n: int
    warmup: int
    execution_time: int
    benchers: List[Bencher]


def run(config: RunConfig):
    # create the list of benchmarkers

    print("Building in release mode")
    # running the command this way makes the thread blocks the execution
    subprocess.run("cargo test --release --no-run".split())

    print("\n\nRunning tests\n\n")
    for i in range(config.n + config.warmup):  # run n + warmup executions
        print(f"Running {i}")
        # FIXME: doing this way the program runs the expected time,
        # but the output is not totally captured as the process is killed by other command

        # runs the cargo test
        # doing this way because process doesn't end by itself
        process = subprocess.run(
            ["cargo", "test", "--release", "--", "network::network", "--ignored", "--nocapture"], capture_output=True, env=dict(os.environ, SIMULATION_TIME=str(config.execution_time)))
        # time.sleep(config.execution_time)

        # Send the signal to all the process groups
        # https://stackoverflow.com/questions/4789837/how-to-terminate-a-python-subprocess-launched-with-shell-true
        # os.killpg(os.getpgid(process.pid), signal.SIGKILL)

        if process.stdout is None:
            raise Exception("stdout was not captured")

        # gets output from stdout
        data = process.stdout.decode("utf-8").strip()

        # for each line of the output
        for line in data.splitlines():
            # try to read an event from this line
            try:
                event = json.loads(line)
                addr = event['addr']
                event = event['event']  # gets the event
                # for each bench
                for bench in config.benchers:
                    bench.calculate(event, addr)
            except json.JSONDecodeError as error:
                print(line)
                pass
                # print(error)
        for bench in config.benchers:
            bench.store_result()
            bench.clear()

    # calculates means with the results of each bencher
    result = []
    for bench in config.benchers:
        bench_info = bench.info()
        # throw away warmup executions
        results = bench.get_results()[config.warmup:]
        print(results)
        mean: float = statistics.fmean(results)
        stdev = statistics.stdev(results)
        result.append({
            'name': bench_info.name,
            'value': mean,
            'unit': bench_info.unit,
            'range': stdev
        })

    with open("bench_simulation_result.json", "w") as file:
        file.write(json.dumps(result))


def main():
    parser = argparse.ArgumentParser(
        description="Benchmark on network simulation.")

    parser.add_argument(
        "-n", type=int, default=5
    )

    parser.add_argument(
        "--warmup", type=int, default=1
    )

    parser.add_argument(
        "--execution_time", type=int, default=60
    )

    benchers: List[Bencher] = [
        UncleRate(),
        FailedMining(),
        RollbackCount(0),
        RollbackDistanceMean(0),
        RealRollbackDistanceMean(0),
        BlocksBetweenRollbacks(0),
        ComputedBlocks(0),
        Height(0),
        MeanHeight(),
    ]

    args = parser.parse_args()

    run(RunConfig(
        n=args.n,
        warmup=args.warmup,
        execution_time=args.execution_time,
        benchers=benchers
    ))


if __name__ == "__main__":
    main()
