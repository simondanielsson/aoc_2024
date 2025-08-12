import sys
import copy
import pprint

# picoseconds = 65
picoseconds = 100


def _read_input() -> list[list[str]]:
    return [list(line.strip()) for line in sys.stdin.readlines()]


def display_grid(grid: list[list[str]]) -> None:
    for row in grid:
        print("".join(str(c)[-1:] for c in row))


def find_pos(grid: list[list[str]], value: str) -> tuple[int, int]:
    for row_number, row in enumerate(grid):
        if value in row:
            return row_number, row.index(value)

    raise ValueError(f"Could not find {value}")


def get_next(current_pos: tuple[int, int], dir: str) -> tuple[int, int]:
    row, col = current_pos

    if dir == "up":
        return (row - 1, col)
    if dir == "down":
        return (row + 1, col)
    if dir == "left":
        return (row, col - 1)
    if dir == "right":
        return (row, col + 1)

    raise ValueError("Invalid dir")


def in_grid(pos, rows, cols):
    row, col = pos
    return 0 <= row < rows and 0 <= col < cols


def next_pos(
    current_pos: tuple[int, int],
    grid: list[list[str]],
    rows: int,
    cols: int,
) -> tuple[int, int]:
    row, col = current_pos

    up = (row - 1, col)
    down = (row + 1, col)
    left = (row, col - 1)
    right = (row, col + 1)

    # Only one valid move
    return next(
        (r, c)
        for r, c in (up, down, left, right)
        if 0 <= r < rows and 0 <= c < cols and grid[r][c] in (".", "E")
    )


def calculate_cheat_distance(grid: list[list[str]]) -> list[list[int]]:
    print(grid)
    # row, col
    start_pos = find_pos(grid, "S")
    end_pos = find_pos(grid, "E")

    print(f"Start: {start_pos}; end: {end_pos}")

    original_grid = copy.deepcopy(grid)
    enumerated_grid: list[list[str | int]] = grid.copy()

    rows = len(grid)
    cols = len(grid[0])

    current_pos = start_pos
    dist = 0
    while current_pos != end_pos:
        row, col = current_pos
        enumerated_grid[row][col] = dist
        dist += 1
        print(f"Moving from {current_pos=}")
        current_pos = next_pos(current_pos, grid, rows, cols)

    enumerated_grid[end_pos[0]][end_pos[1]] = dist
    display_grid(enumerated_grid)

    # Calculate cheat distance
    cheat_distance = [
        [None if cell == "#" else "." for cell in row] for row in copy.deepcopy(grid)
    ]
    display_grid(cheat_distance)

    def traverse(current_pos, dir: str) -> None:
        cur_row, cur_col = current_pos
        up = get_next(current_pos, dir)
        upup = get_next(up, dir)
        if in_grid(upup, rows, cols):
            next_r, next_c = up
            r, c = upup
            if isinstance(enumerated_grid[r][c], int) and (
                cheat_distance[next_r][next_c] is None
                or isinstance(cheat_distance[next_r][next_c], int)
            ):
                cheat_distance[next_r][next_c] = max(
                    cheat_distance[next_r][next_c] or 0,
                    enumerated_grid[r][c] - enumerated_grid[cur_row][cur_col] - 2,
                )
                print(
                    f"{up=}, {upup=}. New cheat distance {cheat_distance[next_r][next_c]}"
                )

    grid = original_grid
    current_pos = start_pos
    print("Calculating cheat distance")
    print(f"{current_pos=}")
    while current_pos != end_pos:
        traverse(current_pos, "up")
        traverse(current_pos, "down")
        traverse(current_pos, "left")
        traverse(current_pos, "right")

        print(f"Moving from {current_pos=}")
        r, c = current_pos
        grid[r][c] = "-"
        current_pos = next_pos(current_pos, grid, rows, cols)

    display_grid(cheat_distance)

    return cheat_distance


def main() -> int:
    grid = _read_input()
    cheat_distance = calculate_cheat_distance(grid)
    counts = len(
        [
            cell
            for row in cheat_distance
            for cell in row
            if cell is not None and isinstance(cell, int) and cell >= picoseconds
        ]
    )
    print(f"Count with > {picoseconds}: {counts}")

    return 0


"""                  X
###############      # 
#234#012#.....#    X#S#X
#1#5#9#3#.###.#      # 
#0#678#4#.#...#      X
#######5#.#.###
#######6#.#...#
#######7#.###.#
###..3#8..#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

84 - 18 - 2 = 64
total_dist - cur_dist - n_skips
"""


if __name__ == "__main__":
    sys.exit(main())
