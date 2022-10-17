import argparse
from dataclasses import dataclass
from json import dumps
import re
import subprocess
from typing import Callable


def exec(command: str, capture_output=True) -> str:
    p = subprocess.run(command.split(), capture_output=capture_output)
    if capture_output:
        return p.stdout.decode("utf-8").strip()
    else:
        return ""


def get_bench_result(line: str) -> int | None:
    result = re.search(r"\.\.\. bench:( )*((.)*)( )*ns/iter", line)
    if result is None:
        return None
    else:
        result = result.group(2)
        if type(result) == str:
            result = result.replace(",", "")  # remove all `,` from number
            return int(result)
        else:
            return None


def get_bench_deviation(line: str) -> int | None:
    result = re.search(r"\+/\-( )*((.)*)\)", line)
    if result is None:
        return None
    else:
        result = result.group(2)
        if type(result) == str:
            result = result.replace(",", "")  # remove all `,` from number
            return int(result)
        else:
            return None


def get_bench_name(line: str) -> str | None:
    result = re.search(r"test ((\w)*)( )*\.\.\.", line)
    if result is None:
        return None
    else:
        result = result.group(1)
        if type(result) == str:
            return result
        else:
            return None


@dataclass
class Result:
    mean: float
    deviation: float

    def __iter__(self):
        return iter(self)


def reduce_result(l: list[Result], init: float, f: Callable[[Result, float], float]):
    if len(l) == 0:
        return init
    else:
        return f(l[0], reduce_result(l[1:], init, f))


@dataclass
class RunConfig:
    n: int
    warmup: int
    command: str
    branches: list[str]


def run(config: RunConfig):
    def a(x, y): return x+y
    results = {}
    for branch in config.branches:
        exec(f"git checkout {branch}")
        print(f"On branch {branch}")
        for i in range(config.warmup):
            print(f"Executing warmup {i} for branch {branch}")
            exec(config.command)

        branch_results: dict[str, list[Result]] = {}

        for i in range(config.n):
            print(f"Executing {i} for branch {branch}")
            output = exec(config.command, capture_output=True)
            lines = output.splitlines()
            lines = list(
                filter(lambda line: line.find("... bench") != -1, lines))
            names = list(map(get_bench_name, lines))
            means = list(map(get_bench_result, lines))
            deviations = list(map(get_bench_deviation, lines))

            branch_results = {}
            for i in range(len(names)):
                name = names[i]
                mean = means[i]
                deviation = deviations[i]

                if name is None or mean is None or deviation is None:
                    raise TypeError

                if name in branch_results:
                    branch_results[name].append(
                        Result(mean=mean,
                               deviation=deviation)
                    )
                else:
                    branch_results[name] = [
                        Result(mean=mean,
                               deviation=deviation)
                    ]

        branch_result = {}
        for test in branch_results.keys():
            test_results = branch_results[test]
            sum_mean: float = reduce_result(test_results, 0,
                                            lambda r1, acc: r1.mean + acc)
            sum_deviation: float = reduce_result(test_results, 0,
                                                 lambda r1, acc: r1.deviation + acc)

            mean_means = sum_mean / config.n
            mean_deviation = sum_deviation / config.n

            branch_result[test] = {
                'mean': mean_means,
                'deviation': mean_deviation
            }

        results[branch] = branch_result

    print(dumps(results))


def main():
    parser = argparse.ArgumentParser(
        description="Benchmark on network simulation.")

    parser.add_argument(
        "-n", type=int, default=1
    )

    parser.add_argument(
        "--warmup", type=int, default=1
    )

    parser.add_argument(
        "--branches", type=str, default=["event-bench", "testing"], nargs='+'
    )

    command = "cargo +nightly bench -- --nocapture"

    args = parser.parse_args()

    run(RunConfig(
        n=args.n,
        warmup=args.warmup,
        command=command,
        branches=args.branches
    ))


if __name__ == "__main__":
    main()
