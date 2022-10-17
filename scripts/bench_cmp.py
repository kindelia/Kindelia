#!/usr/bin/env python3

from typing import Any, Callable, Dict, Union
import sys
from dataclasses import dataclass
from json import dumps
import re
import subprocess
import argparse


def eprint(*args: object, **kargs: object):
    print(*args, file=sys.stderr, **kargs)


class CommandFailed(Exception):
    def __init__(self, cmd: list[str], result: subprocess.CompletedProcess[Any]):
        super().__init__()
        self.cmd = cmd
        self.result = result

    def __repr__(self):
        return "Command '{command}' failed with code {p.returncode}.\nSTDOUT: {stdout}\nSTDERR: {stderr}"


def run_cmd(command: list[str], capture_output: bool = True) -> str:
    p = subprocess.run(command, capture_output=capture_output)
    if p.returncode != 0:
        print(p.stderr.decode("utf-8"))
        raise CommandFailed(command, p)
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
    deviation: int


def reduce_result(l: list[Result], init: float, f: Callable[[Result, float], float]):
    if len(l) == 0:
        return init
    else:
        return f(l[0], reduce_result(l[1:], init, f))


@dataclass
class StashMode:
    pass


@dataclass
class RefsMode:
    refs: list[str]


RunMode = Union[StashMode, RefsMode]


@dataclass
class RunConfig:
    mode: RunMode
    n: int
    warmup: int
    command: list[str]


def run(config: RunConfig) -> int:
    # bench name -> ( code ref -> result )
    results: Dict[str, Dict[str, Dict[str, float]]] = {}

    match config.mode:
        case StashMode():
            # This ensures the working tree or the stage is dirty
            try:
                run_cmd(
                    [
                        "sh",
                        "-c",
                        "not git diff --exit-code || not git diff --exit-code --cached",
                    ]
                )
            except CommandFailed as err:
                eprint(err)
                eprint("ERROR: Working tree is clean")
                return 1
            results["working"] = run_bench(config, "working")
            eprint("Stashing changes...")
            run_cmd(["git", "stash"])
            results["head"] = run_bench(config, "head")
            eprint("Popping stash...")
            run_cmd(["git", "stash", "pop"])
        case RefsMode(refs):
            for ref in refs:
                eprint(f"Checking out '{ref}'")
                run_cmd(["git", "checkout", "ref"])
                eprint(f"On {ref}")
                ref_result = run_bench(config, ref)
                results[ref] = ref_result
                pass

    print(dumps(results))
    return 0


def run_warmup(config: RunConfig):
    for i in range(config.warmup):
        eprint(f"Warmup #{i}")
        run_cmd(config.command)


def run_bench(config: RunConfig, run_name: str):
    run_warmup(config)

    benches_results: Dict[str, list[Result]] = {}
    for i in range(config.n):
        eprint(f"Execution #{i} for '{run_name}'")
        output = run_cmd(config.command, capture_output=True)
        lines = output.splitlines()
        lines = list(filter(lambda line: line.find("... bench") != -1, lines))
        names = list(map(get_bench_name, lines))
        means = list(map(get_bench_result, lines))
        deviations = list(map(get_bench_deviation, lines))

        for item in zip(names, means, deviations):
            bench_name: str | None
            mean: float | None
            deviation: int | None
            (bench_name, mean, deviation) = item
            if bench_name is None or mean is None or deviation is None:
                raise TypeError

            benches_results.setdefault(bench_name, []).append(
                Result(mean=mean, deviation=deviation)
            )

    run_results: Dict[str, Dict[str, float]] = {}
    for (bench_name, bench_results) in benches_results.items():
        sum_mean: float = reduce_result(bench_results, 0, lambda r1, acc: r1.mean + acc)
        sum_deviation: float = reduce_result(
            bench_results, 0, lambda r1, acc: r1.deviation + acc
        )
        mean_mean = sum_mean / config.n
        mean_deviation = sum_deviation / config.n
        run_results[bench_name] = {"mean": mean_mean, "deviation": mean_deviation}

    return run_results


def main():
    parser = argparse.ArgumentParser(description="Benchmark on network simulation.")

    parser.add_argument("-n", type=int, default=1)

    parser.add_argument("--warmup", type=int, default=1)

    parser.add_argument(
        "--stash", action="store_true"
    )  # default=["event-bench", "testing"]

    parser.add_argument(
        "--refs", type=str, nargs="+"
    )  # default=["event-bench", "testing"]

    command = ["cargo", "+nightly", "bench", "--", "--nocapture"]

    args = parser.parse_args()

    if args.refs is not None:
        mode = RefsMode(args.refs)
    elif args.stash:
        mode = StashMode()
    else:
        eprint("You must choose one mode of execution")
        return

    return run(
        RunConfig(
            mode=mode,
            n=args.n,
            warmup=args.warmup,
            command=command,
        )
    )


if __name__ == "__main__":
    ret = main()
    exit(ret)
