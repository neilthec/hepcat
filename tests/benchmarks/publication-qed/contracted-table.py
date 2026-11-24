"""Write a compact, readable table of external spinor polynomials.

Each line of a numerator is an integer coefficient followed by ordinary
external brackets and powers of the electron mass. All propagators appear
on the fraction line. These are the input data for the independent replay.
"""
import argparse
import json
from pathlib import Path


def write_table(data, output):
    print("particles", *data["particles"], file=output)
    print("coupling", data["charge_power"], data["sqrt_two_power"], file=output)
    print("pole-products", data["pole_products"], file=output)
    for fraction in data["fractions"]:
        print("fraction", fraction["divisor"], *fraction["poles"], file=output)
        for row in fraction["terms"]:
            print(*row, file=output)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("json",type=Path)
    parser.add_argument("table",type=Path)
    args = parser.parse_args()
    with args.table.open("w") as target:
        write_table(json.loads(args.json.read_text()), target)
