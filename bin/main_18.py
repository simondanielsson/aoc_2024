import pprint
import heapq
import sys


# N_BYTES = 12
# SIZE = 7
N_BYTES = 1024
SIZE = 71


def show_grid(grid: list[list[bool]]) -> None:
    for row in grid:
        for cell in row:
            if cell:
                print(".", end="")
            else:
                print("#", end="")
        print()
    print()


def get_neighbors(
    x: int,
    y: int,
    grid: list[list[bool]],
) -> list[tuple[int, int]]:
    up = (x, y - 1)
    down = (x, y + 1)
    left = (x - 1, y)
    right = (x + 1, y)

    neighbors = []
    if x > 0 and grid[y][x - 1]:
        neighbors.append(left)
    if x < SIZE - 1 and grid[y][x + 1]:
        neighbors.append(right)
    if y > 0 and grid[y - 1][x]:
        neighbors.append(up)
    if y < SIZE - 1 and grid[y + 1][x]:
        neighbors.append(down)

    return neighbors


def build_graph(grid: list[list[bool]]) -> dict[tuple[int, int], list[tuple[int, int]]]:
    graph = {}

    for y, row in enumerate(grid):
        for x, is_empty in enumerate(row):
            if is_empty:
                graph[(x, y)] = get_neighbors(x, y, grid)

    return graph


def heappop[T](heap: list[T]) -> T | None:
    try:
        return heapq.heappop(heap)
    except IndexError:
        return None


def dijsktras(neighbors: dict[tuple[int, int], list[tuple[int, int]]]) -> int:
    # (dist, x, y) to utilize tuple comparison. This breaks tie by taking the leftmost element
    heap: list[tuple[int, int, int]] = []
    heapq.heapify(heap)

    start_node = (0, 0, 0)
    end = (SIZE - 1, SIZE - 1)

    visited: set[tuple[int, int]] = set()
    heapq.heappush(heap, start_node)
    while next := heappop(heap):
        cur_dist, x, y = next
        cur_pos = (x, y)
        if cur_pos == end:
            return cur_dist
        elif cur_pos in visited:
            continue
        else:
            print(f"Visiting {cur_pos}")
            visited.add(cur_pos)
            for neighbor in neighbors[cur_pos]:
                dist_to_neighbor = cur_dist + 1
                heapq.heappush(heap, (dist_to_neighbor, *neighbor))

    raise ValueError("No path found")


def main() -> int:
    # True if coordinate empty, false otherwise
    grid = [[True] * SIZE for _ in range(SIZE)]
    for _ in range(N_BYTES):
        x, y = [int(i) for i in input().split(",")]
        grid[y][x] = False

    show_grid(grid)

    graph = build_graph(grid=grid)
    pprint.pprint(graph)
    shortest_path = dijsktras(neighbors=graph)

    print(shortest_path)

    return 0


if __name__ == "__main__":
    sys.exit(main())
