"""Expand the factored numerators printed in the main text against the catalog.

Words between spaces are literal external spinor products; integer coefficients
are collected exactly. The appendix is rendered directly from the checked data.
"""
from collections import Counter
from pathlib import Path
import argparse
import json


def term(words, coefficient=1):
    return Counter({tuple(sorted(words.split())): coefficient})


def product(*factors):
    result = term("")
    for factor in factors:
        out = Counter()
        for a,x in result.items():
            for b,y in factor.items():
                out[tuple(sorted(a+b))] += x*y
        result = out
    return result


def add(*parts):
    result = Counter()
    for part in parts:
        result.update(part)
    return result


def check(data, index, poly):
    expected = {tuple(sorted(words)):c for c,*words in data["fractions"][index]["terms"]}
    if {m:c for m,c in poly.items() if c} != expected:
        raise ValueError(f"Printed polynomial mismatch at denominator {data['fractions'][index]['poles']}")


def verify(directory):
    seven = json.loads((directory/"7-3.short.json").read_text())
    check(seven, 0, product(term("<15>"), add(term("m m <37> [27] [46]"),
          term("m <36> [2(125)7> [47]",2),term("[2(125)7> [36] [7(346)4>"))))
    check(seven, 1, product(add(term("m <13> [25]"), term("<15> [2(125)3>")),
          add(term("m <67> [47]"),term("[4(67)7> [67]",-1))))
    check(seven, 2, add(product(add(term("m <14> [25]"),term("<15> [2(125)4>")),
          add(term("m <37> [67]"),term("[37] [6(37)7>",-1))),
          product(term("<46> [37]"),add(term("m <15> [2(37)7>"),term("<1(125)(37)7> [25]"))),
          product(term("m <15>",-1),add(term("m <46> <37> [27]"),term("[46] [2(125)7> [37]")))))
    nine = json.loads((directory/"9-3.short.json").read_text())
    poles = ["s12","s34","t125","t1257","t346","t3468"]
    index = next(i for i,v in enumerate(nine["fractions"]) if v["poles"] == poles)
    check(nine, index, product(term("<15>",-1), add(
          product(term("m m <38> [46]"),add(term("m m <79> [27] [89]"),
              term("[2(125)7> [7(1257)9> [89]",2),term("[2(125)7> [79] [8(3468)9>",2),
              term("[27] [8(3468)9> [9(1257)7>"))),
          product(term("m <36>",2),add(term("m m <79> [27]"),term("[2(125)7> [7(1257)9>")),
              add(term("[4(346)8> [89]"),term("[48] [9(3468)8>"))),
          product(term("[2(125)7> [36] [8(346)4>"),
              add(term("m m <89> [79]"),term("[7(1257)9> [9(3468)8>"))))))
    print("Verified the four factored main-text numerators by exact expansion.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory",type=Path)
    verify(parser.parse_args().directory)
