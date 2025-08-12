import io
import functools
import re
import sys


def _read_input() -> tuple[list[str], list[str]]:
    lines = io.TextIOWrapper(sys.stdin.buffer, encoding="utf-8").readlines()
    lines = [line.removesuffix("\n") for line in lines]

    towels = lines[0].split(", ")
    designs = lines[2:]

    return towels, designs


def design_ok(design: str, towels: list[str]) -> bool:
    # 1. Regex? Perhaps just see if the design matches any number of the towels
    # 2. Dynamic programming? See if the design can be constructed from the towels
    # This is not an optimization problem (yet), so let's not use DP
    print("design", design)

    tower_pattern = "|".join(towels)
    # Check if the design can be build from any sequence of towels
    pattern = re.compile(f"^(?:{tower_pattern})+$")

    match = pattern.fullmatch(design)
    if match:
        for group in match.groups():
            print(group)
    return bool(match)


@functools.lru_cache()
def count_configs(design: str, towels: tuple[str, ...]) -> int:
    if not design:
        return 1

    acc = 0
    for towel in towels:
        if design.startswith(towel):
            next = design[len(towel) :]
            acc += count_configs(next, towels)

    return acc


def main() -> int:
    towels, designs = _read_input()
    print("towels:", towels)
    print("designs:", designs)

    # Part 1
    # design_is_ok = [design_ok(design, towels) for design in designs]
    # ok_designs = [is_ok for is_ok in design_is_ok if is_ok]
    # print("ok designs:", len(ok_designs))

    # Part 2
    print(
        f"Number configs: {sum(count_configs(design, tuple(towels)) for design in designs)}"
    )

    return 0


if __name__ == "__main__":
    sys.exit(main())
