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

    def test_sequential_layout_has_implicit_destinations(self):
        source = io.StringIO(
            "    const cdouble v0 = parameters[0];\n"
            "    const cdouble v1 = v0 * v0;\n"
            "    return v0 + v1;\n}\n")
        with tempfile.TemporaryFile() as binary:
            table = plan.Table(binary)
            self.assertEqual(plan.component(source, table, reuse=False), (0, 3, 3, 2))
            binary.seek(0)
            words = array('i')
            words.fromfile(binary, table.words)
            self.assertEqual(list(words), [0, 0, 1, 0, 0, 2, 0, 1])

    def test_narrow_stage_keeps_absolute_stream_offsets(self):
        with tempfile.TemporaryFile() as wide, tempfile.TemporaryFile() as narrow:
            table = plan.Table(wide)
            table.emit("previous", [70000])
            table.emit("stage", [0, 65535])
            self.assertEqual(plan.narrow_stage(table, 2, narrow), 0)
            self.assertEqual((wide.tell(), table.words), (8, 2))
            table.emit("stage", [7])
            self.assertEqual(plan.narrow_stage(table, 2, narrow), 3)
            table.emit("stage", [65536])
            self.assertIsNone(plan.narrow_stage(table, 2, narrow))
            self.assertEqual((wide.tell(), table.words), (16, 4))
            narrow.seek(0)
            words = array("H")
            words.fromfile(narrow, 5)
            self.assertEqual(list(words), [1, 0, 65535, 1, 7])

    def test_stage_spin_projection_indices(self):
        self.assertEqual(plan.index_spin_pairs(
            "chains[4][2 * (indices[1][0] > 0) + (indices[3][0] > 0)]", 7),
            "chains[4][spin_pairs[18]]")
        self.assertEqual(plan.index_spin_pairs("chains[4][2 * 0 + 0]", 7), "chains[4][0]")

    def test_later_stage_reads_the_original_producer(self):
        producers = {}
        with tempfile.TemporaryFile() as binary:
            table = plan.Table(binary)
            first = io.StringIO("    const cdouble v0 = shared[7] = parameters[2];\n"
                                "    return v0;\n}\n")
            second = io.StringIO("    return shared[7];\n}\n")
            plan.component(first, table, False, producers, base=10)
            self.assertEqual(producers, {7: 10})
            plan.component(second, table, False, producers, base=12)
            binary.seek(0)
            words = array("i")
            words.fromfile(binary, table.words)
            self.assertEqual(list(words), [0, 2, 1, 10, 1, 10])

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
