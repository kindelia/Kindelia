import argparse
import json
import sys
import os
import signal
from abc import ABC, abstractmethod
from dataclasses import dataclass
import subprocess
import time


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
    def calculate(self, event):
        pass

    # TODO: add generic type?
    @abstractmethod
    def get_result(self):
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

    def calculate(self, event):
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


class FailedMining(Bencher):
    def __init__(self):
        super().__init__()
        self.total: int = 0

    def info(self) -> BencherInfo:
        return BencherInfo(name="Failed Mining", unit="Logs")

    def calculate(self, event):
        if 'Mining' in event:
            data = event['Mining']
            if 'Failure' in data['event']:
                self.total += 1

    def get_result(self):
        return self.total


# ============================================================
# Run

@dataclass
class RunConfig:
    n: int
    warmup: int
    execution_time: int
    benchers: list[Bencher]


def run(config: RunConfig):
    # create the list of benchmarkers

    print("Building in release mode")
    # running the command this way makes the thread blocks the execution
    subprocess.run("cargo test --release --no-run".split())

    print("\n\nRunning tests\n\n")
    for i in range(config.n + config.warmup):  # run n + warmup executions

        # runs the cargo test
        # doing this way because process doesn't end by itself
        process = subprocess.Popen(
            ["cargo", "test", "--release", "--", "network::network", "--ignored", "--nocapture"], stdout=subprocess.PIPE, preexec_fn=os.setsid)
        time.sleep(config.execution_time)

        # Send the signal to all the process groups
        # https://stackoverflow.com/questions/4789837/how-to-terminate-a-python-subprocess-launched-with-shell-true
        os.killpg(os.getpgid(process.pid), signal.SIGTERM)

        if process.stdout is None:
            raise Exception("stdout was not captured")

        # gets output from stdout
        data = process.stdout.read().decode("utf-8").strip()

        # for each line of the output
        for line in data.splitlines():
            # try to read an event from this line
            try:
                event = json.loads(line)
                event = event['event']  # gets the event
                # for each bench
                for bench in config.benchers:
                    bench.calculate(event)
                    bench.store_result()  # maybe put this inside calculate?
            except json.JSONDecodeError as error:
                print(error)

    # calculates means with the results of each bencher
    result = []
    for bench in config.benchers:
        bench_info = bench.info()
        # throw away warmup executions
        results = bench.get_results()[config.warmup:]

        mean: float = sum(results) / len(results) if len(results) != 0 else 0
        result.append({
            'name': bench_info.name,
            'value': mean,
            'unit': bench_info.unit
        })

    print(json.dumps(result), file=sys.stderr)


def main():
    parser = argparse.ArgumentParser(
        description="Benchmark on network simulation.")

    parser.add_argument(
        "-n", type=int, default=10
    )

    parser.add_argument(
        "--warmup", type=int, default=1
    )

    parser.add_argument(
        "--execution_time", type=int, default=10
    )

    benchers: list[Bencher] = [UncleRate(), FailedMining()]

    args = parser.parse_args()

    run(RunConfig(
        n=args.n,
        warmup=args.warmup,
        execution_time=args.execution_time,
        benchers=benchers
    ))


if __name__ == "__main__":
    main()
