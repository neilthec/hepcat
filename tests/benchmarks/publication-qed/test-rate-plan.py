"""Regression checks for operand-slot lifetimes, including aliased operands."""
from array import array
import importlib.util
import io
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location("rate_plan", Path(__file__).with_name("build-rate-plan.py"))
plan = importlib.util.module_from_spec(spec)
spec.loader.exec_module(plan)


class RatePlanTest(unittest.TestCase):
    def test_slots_survive_until_final_read(self):
        source = io.StringIO(
            "    const cdouble v0 = parameters[0];\n"
            "    const cdouble v1 = parameters[1];\n"
            "    const cdouble v2 = v0 * v0;\n"
            "    const cdouble v3 = v1 + v1;\n"
            "    return v0 + v1 + v2 + v3 + v3 + v3;\n}\n")
        with tempfile.TemporaryFile() as binary:
            table = plan.Table(binary)
            _, count, peak, result = plan.component(source, table)
            self.assertEqual((count, peak), (5, 4))
            binary.seek(0)
            words = array('i')
            words.fromfile(binary, table.words)
        self.assertEqual(list(words), [
            0, 0, 256, 1,       # two parameter loads
            2*256+1, 0, 0,             # v0 stays live until the return
            3*256+2, 1, 1,             # likewise v1
            result*256+3, 6, 0, 1, 2, 3, 3, 3,
        ])

    def test_extended_header(self):
        with tempfile.TemporaryFile() as binary:
            table = plan.Table(binary)
            table.emit("large destination", [1], 1 << 23)
            table.codes.update({i: i+1 for i in range(255)})
            table.emit("large opcode", [], 0)
            binary.seek(0)
            words = array('i')
            words.fromfile(binary, table.words)
            self.assertEqual(list(words), [-1, 1 << 23, 1, ~256, 0])

    def test_repeated_final_operand_frees_only_one_slot(self):
        source = io.StringIO(
            "    const cdouble v0 = parameters[0];\n"
            "    const cdouble v1 = v0 * v0;\n"
            "    const cdouble v2 = parameters[1];\n"
            "    return v1 + v2;\n}\n")
        with tempfile.TemporaryFile() as binary:
            _, _, peak, _ = plan.component(source, plan.Table(binary))
            self.assertEqual(peak, 2)


if __name__ == "__main__":
    unittest.main()
