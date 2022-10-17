import argparse
from dataclasses import dataclass
from json import dumps
import re
import subprocess


@dataclass
class RunConfig:
    n: int
    warmup: int
    command: str
    branches: list[str]


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


def run(config: RunConfig):
    results = {}
    for branch in config.branches:
        print(branch)
        for i in range(config.warmup):
            print(f"Executing warmup {i} for branch {branch}")
            exec(config.command)

        branch_results = {}

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
                branch_results[name] = {
                    'mean': mean,
                    'deviation': deviation
                }

        results[branch] = branch_results
    
    print(dumps(results))


def main():
    parser = argparse.ArgumentParser(
        description="Benchmark on network simulation.")

    parser.add_argument(
        "-n", type=int, default=1
    )

    parser.add_argument(
        "--warmup", type=int, default=2
    )

    # parser.add_argument(
    #     "--command", type=str, default="cargo bench"
    # )

    parser.add_argument(
        "--branches", type=list[str], default=["dev", "master"]
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
