from enum import Enum
import sys
import copy
from dataclasses import dataclass


@dataclass
class State:
    A: int
    B: int
    C: int
    ptr: int


class Opcode(Enum):
    ADV = 0
    BXL = 1
    BST = 2
    JNZ = 3
    BXC = 4
    OUT = 5
    BDV = 6
    CDV = 7


def _increment_ptr(state: State) -> State:
    state.ptr += 2
    return state


def _combo(operand: int, state: State) -> int:
    if operand <= 3:
        return operand
    elif operand == 4:
        return state.A
    elif operand == 5:
        return state.B
    elif operand == 6:
        return state.C
    elif operand == 7:
        raise AssertionError("Never occurs")

    raise ValueError(f"Invalid operand: {operand}")


def _perform_op(op: Opcode, operand: int, state: State) -> tuple[State, int | None]:

    output = None
    match op:
        case Opcode.ADV:
            num = state.A
            dem = 2 ** _combo(operand, state)
            state.A = num // dem
            state = _increment_ptr(state)
        case Opcode.BXL:
            state.B = state.B ^ operand
            state = _increment_ptr(state)
        case Opcode.BST:
            state.B = _combo(operand, state) % 8
            state = _increment_ptr(state)
        case Opcode.JNZ if state.A != 0:
            state.ptr = operand
            # no pointer increment
        case Opcode.JNZ if state.A == 0:
            state = _increment_ptr(state)
        case Opcode.BXC:
            state.B = state.B ^ state.C
            state = _increment_ptr(state)
        case Opcode.OUT:
            output = _combo(operand, state) % 8
            state = _increment_ptr(state)
        case Opcode.BDV:
            num = state.A
            dem = 2 ** _combo(operand, state)
            state.B = num // dem
            state = _increment_ptr(state)
        case Opcode.CDV:
            num = state.A
            dem = 2 ** _combo(operand, state)
            state.C = num // dem
            state = _increment_ptr(state)
        case _:
            raise ValueError(f"Invalid opcode: {op}")

    return state, output


def _read_register() -> int:
    return int(input().split(":")[1][1:])


def _read() -> tuple[State, list[Opcode | int]]:
    A = _read_register()
    B = _read_register()
    C = _read_register()

    input()

    program_content = input().split(":")[1][1:].split(",")
    program = [
        Opcode(int(op)) if idx % 2 == 0 else int(op)
        for idx, op in enumerate(program_content)
    ]

    return State(A=A, B=B, C=C, ptr=0), program


def inner(state, program) -> list[int]:
    state = copy.deepcopy(state)

    outputs: list[int] = []
    while state.ptr < len(program):
        op, operand = program[state.ptr], program[state.ptr + 1]
        state, output = _perform_op(op=op, operand=operand, state=state)

        if output is not None:
            outputs.append(output)
            # print("outputs", outputs)

    return outputs


def main() -> int:
    state, program = _read()
    start_state = copy.deepcopy(state)
    program_int = [p.value if isinstance(p, Opcode) else p for p in program]
    print("testing state", state)

    a_state = 0
    for i in reversed(range(len(program))):
        # print("Testing A =", a_state)
        a_state <<= 3
        state.A = a_state

        while inner(state, program) != program_int[i:]:
            a_state += 1
            state.A = a_state

    print(a_state)
    # print(",".join(str(o) for o in outputs))
    return 0


if __name__ == "__main__":
    sys.exit(main())
