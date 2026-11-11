"""Test the production MadGraph C++ runtime without installing MadGraph.

A tiny reference library and helper exercise the actual subprocess, dynamic
loading, spin sums, comparison and reporting code. They do not reproduce the
comparison algorithm. All generated fixtures live in one temporary directory.
"""

# HEPCAT - High Energy Physics Constructive Amplitude Tool
#
# Copyright (C) 2026 Neil Christensen
#
# This file is part of HEPCAT.
#
# HEPCAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HEPCAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.


import ctypes
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


REFERENCE_SOURCE = r'''
#include <cmath>
extern "C" int hepcat_mg_initialize() { return INIT_STATUS; }
extern "C" void hepcat_mg_evaluate(const double*, const int* h, double* answer) {
  // MG receives ordinary helicities: a doubled massive-vector label would
  // be invalid here. This also tests the runtime's spin-label conversion.
  for (int leg = 0; leg < 4; ++leg) {
    if (h[leg] < -1 || h[leg] > 1) { *answer = NAN; return; }
  }
  *answer = REFERENCE_VALUE;
}
@SUM_INTERFACE@
'''


HELPER_SOURCE = r'''
import argparse
import json
import os
from pathlib import Path
import shutil
import sys

parser = argparse.ArgumentParser()
parser.add_argument("--config", type=Path, required=True)
parser.add_argument("--output", type=Path, required=True)
args = parser.parse_args()
config = json.loads(args.config.read_text())
output = args.output
output.mkdir()
mode = os.environ.get("HEPCAT_MG_RUNTIME_TEST_MODE", "pass")

try:
    assert config["PhysicalPDGs"] == [11, -11, 22, 22]
    assert config["AllowedParticlePDGs"] == [11, 22]
    assert config["Masses"] == [1, 1, 0, 0]
    assert config["ParameterMap"] == {"coupling": "g"}
    assert config["ParameterRules"] == {"other": 7}
    assert config["TimeConstraint"] > 0
    assert config["ZeroWidths"] is True
    if mode == "width_parameter":
        assert len(config["Parameters"]) == 2
        assert config["Parameters"][1] == {"Name": "width", "Value": 0}
    elif mode == "literal_width":
        assert len(config["Parameters"]) == 1
    assert config["OptimizationLevel"] == (3 if mode == "optimized" else 0)
    coupling = config["Parameters"][0]["Value"]
    assert config["Parameters"][0]["Name"] == "coupling"
    assert coupling in (1, 3)
    if mode == "helper_error":
        raise RuntimeError("Deliberate model mismatch: g does not match.")
    if mode == "cooperative_timeout":
        raise RuntimeError("The MadGraph preparation time limit expired.")
    library = ("wrong" if mode == "wrong" else
               "polarized" if mode == "helicity_sensitive" else
               "bad_init" if mode == "bad_init" else
               "updated" if coupling == 3 else "good")
    suffix = ".dylib" if sys.platform == "darwin" else ".so"
    shutil.copyfile(Path(__file__).parent / (library + suffix),
                    output / ("libhepcat_madgraph" + suffix))
    result = {"Status": "READY", "MatchedCoupling": coupling,
              "ZeroWidths": config["ZeroWidths"]}
except Exception as error:
    result = {"Status": "ERROR", "Message": str(error)}
    (output / "build.log").write_text("Useful compiler/model diagnostic.\n")
    (output / "result.json").write_text(json.dumps(result))
    print("MadGraph reference preparation failed: " + str(error), file=sys.stderr)
    sys.exit(1)
(output / "result.json").write_text(json.dumps(result))
'''


DRIVER_SOURCE = r'''
#include @RUNTIME_HEADER@
#include <iostream>

struct Process {
  static const std::size_t leg_count = 4;
  using Spins = std::array<int, 4>;
  using Parameters = std::array<cdouble, 1>;
  Parameters parameters = {{cdouble(1, 0)}};
  int phase_mode = 0;
  std::array<ldouble, 4> external_masses = {{1, 1, 0, 0}};

  const Parameters& get_parameters() const { return parameters; }
  const std::array<ldouble, 4>& masses() const { return external_masses; }
  static std::array<std::size_t, 2> incoming_legs() { return {{0, 1}}; }
  void set_momenta(const hepcat_spinas::Momenta<4>&) {}
  cdouble amp(const Spins& spins) {
    if (phase_mode == 3) return spins[2] < 0 ? cdouble(3, 0) : cdouble(1, 0);
    if (phase_mode == 1) return cdouble(0, 1) * parameters[0];
    if (phase_mode == 2) {
      const cdouble phases[4] = {cdouble(1, 0), cdouble(0, 1),
                                cdouble(-1, 0), cdouble(0, -1)};
      const int index = (spins[2] > 0 ? 2 : 0) + (spins[3] > 0 ? 1 : 0);
      return phases[index] * parameters[0];
    }
    return parameters[0];
  }
};

// These fixtures have the generated width-control interface. Their normal
// amplitudes deliberately disagree with the zero-width reference, so a PASS
// requires the validator to disable literal widths and declared width roles.
struct LiteralWidthProcess : Process {
  bool widths_enabled = true;
  bool momenta_ready = true;
  unsigned parameter_updates = 0, width_updates = 0, momentum_updates = 0;
  ldouble momentum_marker = 17;
  void set_parameters(const Parameters& values) {
    parameters = values; ++parameter_updates; momenta_ready = false;
  }
  void set_widths_enabled(bool enabled) {
    widths_enabled = enabled; ++width_updates; momenta_ready = false;
  }
  void set_momenta(const hepcat_spinas::Momenta<4>& point) {
    ++momentum_updates; momentum_marker = point[0][0]; momenta_ready = true;
  }
  cdouble amp(const Spins&) {
    if (!momenta_ready) throw std::logic_error("Missing momenta in width fixture");
    return (widths_enabled ? cdouble(2,1) : cdouble(1,0))*parameters[0];
  }
};

struct ParameterWidthProcess : LiteralWidthProcess {
  using Parameters = std::array<cdouble, 2>;
  Parameters width_parameters = {{cdouble(1,0),cdouble(3.5L,0)}};
  const Parameters& get_parameters() const { return width_parameters; }
  void set_parameters(const Parameters& values) {
    width_parameters = values; ++parameter_updates; momenta_ready = false;
  }
  cdouble amp(const Spins&) {
    if (!momenta_ready) throw std::logic_error("Missing momenta in width fixture");
    const bool nonzero_width = widths_enabled || width_parameters[1] != cdouble(0,0);
    return (nonzero_width ? cdouble(2,1) : cdouble(1,0))*width_parameters[0];
  }
};

template<class WidthProcess>
void run_width_case(WidthProcess& process,
                    const hepcat_spinas::MadGraphConfiguration& config,
                    const hepcat_spinas::MadGraphValidationOptions& options) {
  const auto original_parameters = process.get_parameters();
  const cdouble original_amplitude = process.amp(typename WidthProcess::Spins{{-1,-1,-2,-2}});
  const auto report = hepcat_spinas::validate_madgraph(process,config,options);
  const bool preserved = process.get_parameters() == original_parameters &&
    process.widths_enabled && process.momenta_ready && process.parameter_updates == 0 &&
    process.width_updates == 0 && process.momentum_updates == 0 && process.momentum_marker == 17 &&
    process.amp(typename WidthProcess::Spins{{-1,-1,-2,-2}}) == original_amplitude;
  std::cout << "{\"Validation\":";
  hepcat_spinas::write_madgraph_validation_report_json(report,std::cout);
  std::cout << ",\"CallerPreserved\":" << (preserved ? "true" : "false")
            << ",\"OriginalAmplitudeSquared\":" << std::norm(original_amplitude) << '}';
}

int main(int argc, char** argv) {
  using namespace hepcat_spinas;
  const std::string mode = argc > 1 ? argv[1] : "pass";
  const std::string directory = @FIXTURE_DIRECTORY@;
  setenv("HEPCAT_MG_RUNTIME_TEST_MODE", mode.c_str(), 1);

  MadGraphValidationOptions options;
  options.seed = 0;
  options.points = 7;
  options.progress_every = 0;
  MadGraphConfiguration config;
  config.madgraph_directory = "/fixture/no-MadGraph-required";
  config.model = "fixture";
  config.python_executable = @PYTHON_EXECUTABLE@;
  config.helper_path = directory + "/helper.py";
  config.leg_order = {0, 1, 2, 3};
  config.physical_pdgs = {11, -11, 22, 22};
  config.allowed_particle_pdgs = {11, 22};
  config.spin2 = {1, 1, 2, 2};
  config.helicities = {{0, 0, -2, -2}, {0, 0, -2, 2},
                       {0, 0, 2, -2}, {0, 0, 2, 2}};
  config.parameter_names = {"coupling"};
  config.parameter_map = {{"coupling", "g"}};
  config.parameter_rules = {{"other", 7}};
  config.normalization = ldouble(0.125L);
  if (mode == "optimized") config.optimization_level = 3;
  if (mode == "bad_optimization") config.optimization_level = 4;
  if (mode == "sum_api" || mode == "legacy_sum_api") {
    const std::string library = directory + (mode == "sum_api" ? "/good" : "/legacy") + @LIBRARY_SUFFIX@;
    madgraph_detail::Reference reference(library);
    if (mode == "legacy_sum_api") {
      try {
        reference.evaluate_sum(std::vector<double>(16));
      } catch (const std::runtime_error&) {
        std::cout << "{\"HasSum\":" << (reference.has_sum() ? "true" : "false")
                  << ",\"Rejected\":true}";
        return 0;
      }
      return 4;
    }
    std::cout << "{\"HasSum\":" << (reference.has_sum() ? "true" : "false")
              << ",\"RawSum\":" << reference.evaluate_sum(std::vector<double>(16)) << "}";
    return 0;
  }

  if (mode == "literal_width") {
    LiteralWidthProcess process;
    run_width_case(process,config,options);
    return 0;
  }
  if (mode == "width_parameter" || mode == "bad_width_index") {
    ParameterWidthProcess process;
    config.parameter_names.push_back("width");
    config.width_parameter_indices = {mode == "bad_width_index" ? std::size_t(2) : std::size_t(1)};
    run_width_case(process,config,options);
    return 0;
  }
  if (mode == "legacy_width_roles") config.width_parameter_indices = {0};

  Process process;
  if (mode == "global_phase") process.phase_mode = 1;
  if (mode == "helicity_phases") process.phase_mode = 2;
  if (mode == "helicity_sensitive") process.phase_mode = 3;
  MadGraphValidationReport report;
  if (mode == "watchdog") {
    const std::string pid_file = directory + "/watchdog.pid";
    const std::string script = "import os,time;open(" +
      madgraph_detail::json_string(pid_file) + ",'w').write(str(os.getpid()));time.sleep(30)";
    const int status = madgraph_detail::run_helper(
      {config.python_executable, "-c", script}, directory + "/watchdog.log", 1);
    report.status = status == 124 ? "TIMEOUT" : "ERROR";
    report.message = std::to_string(status);
    report.seed = 0;
  } else {
    if (mode == "unavailable") {
      config.available = false;
      config.reason = "MadGraph is intentionally absent from this test.";
    } else if (mode == "zero_points") options.points = 0;
    else if (mode == "bad_tolerance") options.relative_tolerance = -1;
    else if (mode == "bad_order") config.leg_order = {1, 0, 2, 3};
    else if (mode == "bad_helicity") config.helicities[0][2] = 0;
    else if (mode == "bad_particle_filter") config.allowed_particle_pdgs = {22};
    else if (mode == "vectors") config.spin2[0] = config.spin2[1] = 2;
    if (mode == "update_parameters") {
      report = validate_madgraph(process, config, options);
      if (!report.passed) {
        write_madgraph_validation_report_json(report, std::cout);
        return 0;
      }
      process.parameters[0] = cdouble(3, 0);
    }
    report = validate_madgraph(process, config, options);
  }
  write_madgraph_validation_report_json(report, std::cout);
}
'''


class MadGraphRuntimeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if os.name != "posix":
            raise unittest.SkipTest("The MadGraph runtime currently supports macOS and Linux.")
        compiler = shutil.which("clang++") or shutil.which("g++")
        if not compiler:
            raise unittest.SkipTest("A C++ compiler is needed for the runtime fixtures.")
        cls.repository = Path(__file__).resolve().parents[2]
        spinas = Path(os.environ.get("HEPCAT_SPINAS_DIRECTORY", cls.repository.parent / "spinas"))
        include = spinas / "include"
        if not (include / "spinas.h").is_file():
            raise unittest.SkipTest("SPINAS headers were not found; set HEPCAT_SPINAS_DIRECTORY.")
        cls.temporary = tempfile.TemporaryDirectory(prefix="hepcat-mg-runtime-test-")
        cls.addClassCleanup(cls.temporary.cleanup)
        cls.directory = Path(cls.temporary.name)
        cls.suffix = ".dylib" if sys.platform == "darwin" else ".so"
        shared_flags = ["-dynamiclib"] if sys.platform == "darwin" else ["-shared", "-fPIC"]
        for name, value, init in (("good", 1, 0), ("wrong", 2, 0),
                                  ("polarized", "(h[2] > 0 ? 9 : 1)", 0),
                                  ("updated", 9, 0), ("bad_init", 1, 1), ("legacy", 1, 0)):
            source = cls.directory / (name + ".cpp")
            source.write_text(REFERENCE_SOURCE.replace("REFERENCE_VALUE", str(value))
                              .replace("INIT_STATUS", str(init))
                              .replace("@SUM_INTERFACE@", "" if name == "legacy" else
                                       'extern "C" void hepcat_mg_evaluate_sum(const double*, double* answer) '
                                       '{ *answer = ' + ("80" if name == "polarized" else
                                         '16 * ' + str(value)) + '; }'))
            cls.compile([compiler, "-std=c++11"] + shared_flags +
                        [str(source), "-o", str(cls.directory / (name + cls.suffix))])
        (cls.directory / "helper.py").write_text(HELPER_SOURCE)
        source = (DRIVER_SOURCE
                  .replace("@RUNTIME_HEADER@", json.dumps(str(cls.repository / "source/internal/export/madgraph-runtime.hpp")))
                  .replace("@FIXTURE_DIRECTORY@", json.dumps(str(cls.directory)))
                  .replace("@PYTHON_EXECUTABLE@", json.dumps(sys.executable))
                  .replace("@LIBRARY_SUFFIX@", json.dumps(cls.suffix)))
        driver = cls.directory / "driver.cpp"
        driver.write_text(source)
        cls.executable = cls.directory / "runtime-test"
        arguments = [compiler, "-std=c++11", "-Wall", "-Wextra", "-Werror",
                     "-DWITH_LONG_DOUBLE", "-isystem", str(include), str(driver),
                     "-o", str(cls.executable)]
        if sys.platform != "darwin":
            arguments.extend(["-ldl", "-pthread"])
        cls.compile(arguments)

    @classmethod
    def compile(cls, command):
        result = subprocess.run(command, cwd=cls.directory, text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                timeout=120)
        if result.returncode:
            raise AssertionError("Could not compile runtime fixture:\n" + result.stdout)

    def run_case(self, mode):
        result = subprocess.run([str(self.executable), mode], cwd=self.directory,
                                text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                timeout=20)
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def test_raw_native_sum_has_no_averages_or_symmetry_factor(self):
        report = self.run_case("sum_api")
        self.assertTrue(report["HasSum"])
        self.assertEqual(report["RawSum"], 16)

    def test_older_fixed_helicity_reference_stays_loadable(self):
        report = self.run_case("legacy_sum_api")
        self.assertFalse(report["HasSum"])
        self.assertTrue(report["Rejected"])

    def test_optimized_configuration_reaches_builder(self):
        self.assertEqual(self.run_case("optimized")["Status"], "PASS")
        report = self.run_case("bad_optimization")
        self.assertEqual(report["Status"], "ERROR")
        self.assertIn("optimization", report["Message"])

    def test_all_massless_helicity_rows_pass(self):
        report = self.run_case("pass")
        self.assertTrue(report["Passed"])
        self.assertEqual(report["Status"], "PASS")
        self.assertEqual(len(report["Rows"]), 4)
        self.assertEqual({tuple(row["Helicities"]) for row in report["Rows"]},
                         {(0, 0, -2, -2), (0, 0, -2, 2),
                          (0, 0, 2, -2), (0, 0, 2, 2)})
        for row in report["Rows"]:
            self.assertEqual(row["ComparedPoints"], 7)
            self.assertEqual(row["MaximumRelativeError"], 0)

    def test_unphysical_amplitude_phases_do_not_change_validation(self):
        # Independent external-state conventions may change an amplitude phase.
        # They cannot change its squared modulus for any helicity assignment.
        for mode in ("global_phase", "helicity_phases"):
            with self.subTest(mode=mode):
                report = self.run_case(mode)
                self.assertTrue(report["Passed"])
                self.assertEqual(report["Status"], "PASS")
                self.assertTrue(all(row["MaximumRelativeError"] == 0
                                    for row in report["Rows"]))

    def test_outgoing_vector_wavefunction_convention_is_converted(self):
        # The fixture has unequal polarized rates, so a helicity swap cannot
        # be hidden by parity or by the exhaustive helicity sum.
        report = self.run_case("helicity_sensitive")
        self.assertTrue(report["Passed"])
        self.assertEqual(len(report["Rows"]), 4)
        self.assertTrue(all(row["MaximumRelativeError"] == 0
                            for row in report["Rows"]))

    def test_wrong_reference_fails_and_keeps_the_point(self):
        report = self.run_case("wrong")
        self.assertFalse(report["Passed"])
        self.assertEqual(report["Status"], "FAIL")
        for row in report["Rows"]:
            self.assertEqual(row["Mismatches"], 7)
            failure = row["FirstFailure"]
            self.assertEqual(failure["Point"], 1)
            self.assertEqual(len(failure["Momenta"]), 4)
            self.assertEqual(failure["HEPCATValue"], 0.5)
            self.assertEqual(failure["MadGraphValue"], 1)

    def test_seed_zero_is_retained_and_replays_across_helicities(self):
        first = self.run_case("wrong")
        second = self.run_case("wrong")
        self.assertEqual(first["Seed"], 0)
        momenta = first["Rows"][0]["FirstFailure"]["Momenta"]
        self.assertTrue(all(row["FirstFailure"]["Momenta"] == momenta for row in first["Rows"]))
        self.assertEqual(momenta, second["Rows"][0]["FirstFailure"]["Momenta"])

    def test_builder_errors_have_readable_diagnostics(self):
        report = self.run_case("helper_error")
        self.assertEqual(report["Status"], "ERROR")
        self.assertFalse(report["Passed"])
        self.assertEqual(report["Message"], "Deliberate model mismatch: g does not match.")
        self.assertIn("Useful compiler/model diagnostic", report["BuildLog"])
        self.assertEqual(json.loads(report["ReferenceBuildJSON"])["Status"], "ERROR")

    def test_unavailable_backend_never_passes(self):
        report = self.run_case("unavailable")
        self.assertEqual(report["Status"], "UNAVAILABLE")
        self.assertFalse(report["Passed"])
        self.assertEqual(report["Rows"], [])

    def test_timeout_report_is_not_a_pass(self):
        report = self.run_case("cooperative_timeout")
        self.assertEqual(report["Status"], "ERROR")
        self.assertFalse(report["Passed"])
        self.assertIn("time limit", report["Message"])

    def test_outer_watchdog_terminates_its_child(self):
        report = self.run_case("watchdog")
        self.assertEqual(report["Status"], "TIMEOUT")
        self.assertEqual(report["Message"], "124")
        pid = int((self.directory / "watchdog.pid").read_text())
        with self.assertRaises(ProcessLookupError):
            os.kill(pid, 0)

    def test_invalid_settings_are_rejected(self):
        for mode in ("zero_points", "bad_tolerance", "bad_order", "bad_helicity"):
            with self.subTest(mode=mode):
                report = self.run_case(mode)
                self.assertEqual(report["Status"], "ERROR")
                self.assertFalse(report["Passed"])
                self.assertEqual(report["Rows"], [])

    def test_reference_particle_filter_cannot_exclude_external_particles(self):
        report = self.run_case("bad_particle_filter")
        self.assertEqual(report["Status"], "ERROR")
        self.assertFalse(report["Passed"])
        self.assertIn("allowed reference theory", report["Message"])
        self.assertEqual(report["Rows"], [])

    def test_runtime_parameters_are_forwarded_again_on_each_call(self):
        report = self.run_case("update_parameters")
        self.assertEqual(report["Status"], "PASS")
        self.assertEqual(json.loads(report["ReferenceBuildJSON"])["MatchedCoupling"], 3)

    def test_widths_are_disabled_only_on_a_private_process_copy(self):
        for mode in ("literal_width", "width_parameter"):
            with self.subTest(mode=mode):
                result = self.run_case(mode)
                self.assertTrue(result["CallerPreserved"])
                self.assertEqual(result["OriginalAmplitudeSquared"], 5)
                report = result["Validation"]
                self.assertEqual(report["Status"], "PASS")
                self.assertTrue(report["Passed"])
                self.assertTrue(json.loads(report["ReferenceBuildJSON"])["ZeroWidths"])
                self.assertTrue(all(row["MaximumRelativeError"] == 0 for row in report["Rows"]))

    def test_invalid_width_roles_fail_without_mutating_the_caller(self):
        result = self.run_case("bad_width_index")
        self.assertTrue(result["CallerPreserved"])
        report = result["Validation"]
        self.assertEqual(report["Status"], "ERROR")
        self.assertIn("width parameter index", report["Message"])
        self.assertFalse(report["Passed"])
        self.assertEqual(report["Rows"], [])

    def test_legacy_process_with_declared_width_roles_requires_reexport(self):
        report = self.run_case("legacy_width_roles")
        self.assertEqual(report["Status"], "ERROR")
        self.assertFalse(report["Passed"])
        self.assertIn("Re-export", report["Message"])
        self.assertEqual(report["Rows"], [])

    def test_massive_vector_labels_are_converted(self):
        # The fake library refuses any ordinary helicity outside {-1,0,1}.
        # This passes only when doubled SPINAS vector labels are converted.
        self.assertEqual(self.run_case("vectors")["Status"], "PASS")

    def test_failed_reference_initialization_is_an_error(self):
        report = self.run_case("bad_init")
        self.assertEqual(report["Status"], "ERROR")
        self.assertFalse(report["Passed"])
        self.assertIn("initialize", report["Message"])



class SummedBridgeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        path = Path(__file__).resolve().parents[2] / "source/internal/export/madgraph-runtime.py"
        spec = importlib.util.spec_from_file_location("hepcat_madgraph_sum_builder", path)
        cls.builder = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.builder)

    def test_optimization_is_an_explicit_bounded_integer(self):
        for level in range(4):
            flags = self.builder.optimization_flags(level)
            self.assertEqual("-O" + str(level) in flags, level != 0)
        for level in (-1, 4, True, False, "3", 3.0, None):
            with self.subTest(level=level):
                with self.assertRaises(self.builder.ValidationSetupError):
                    self.builder.optimization_flags(level)

    def test_complete_generated_helicity_table_including_continuations(self):
        source = """      PARAMETER (NCOMB=2)
C     DATA (NHEL(I,9),I=1,4)/0,0,0,0/
      DATA (NHEL(I,1),I=1,4)/-1,1,
     $ -1,1/
      DATA (NHEL(I,2),I=1,4)/1,-1,1,-1/
"""
        expected = [[-1, 1, -1, 1], [1, -1, 1, -1]]
        self.assertEqual(self.builder.complete_helicity_table(source, 4), expected)
        repeated_unit = ("      SUBROUTINE SMATRIX(P,ANS)\n" + source +
                         "      END\n      SUBROUTINE GET_NHEL(H)\n" + source + "      END\n")
        self.assertEqual(self.builder.complete_helicity_table(repeated_unit, 4), expected)
        for changed in (source.replace("NCOMB=2", "NCOMB=3"),
                        source.replace("I=1,4", "I=1,3"),
                        source.replace("NHEL(I,2)", "NHEL(I,1)")):
            with self.subTest(source=changed):
                with self.assertRaises(self.builder.ValidationSetupError):
                    self.builder.complete_helicity_table(changed, 4)

    def test_compiled_sum_really_visits_every_state_without_normalization(self):
        compiler = shutil.which("gfortran") or shutil.which("/opt/homebrew/bin/gfortran")
        if not compiler:
            self.skipTest("GNU Fortran is required for the native spin-sum fixture.")
        table = [[-1, -1], [-1, 1], [1, -1], [1, 1]]
        source = self.builder.summed_bridge("fixture_matrix", table, 2) + """
double precision function fixture_matrix(p, h, color)
  use iso_c_binding
  implicit none
  real(c_double) :: p(4,2)
  integer(c_int) :: h(2), color(2)
  fixture_matrix = (h(1) + 2*h(2))**2
end function
"""
        with tempfile.TemporaryDirectory(prefix="hepcat-native-mg-sum-") as temporary:
            path = Path(temporary)
            (path / "bridge.f90").write_text(source)
            library = path / ("fixture.dylib" if sys.platform == "darwin" else "fixture.so")
            mode = "-dynamiclib" if sys.platform == "darwin" else "-shared"
            result = subprocess.run([compiler, mode, "-fPIC", "-O3", str(path / "bridge.f90"),
                                     "-o", str(library)], text=True, capture_output=True, timeout=60)
            self.assertEqual(result.returncode, 0, result.stderr)
            reference = ctypes.CDLL(str(library))
            reference.hepcat_mg_evaluate_sum.argtypes = [ctypes.POINTER(ctypes.c_double),
                                                       ctypes.POINTER(ctypes.c_double)]
            answer = ctypes.c_double()
            reference.hepcat_mg_evaluate_sum((ctypes.c_double * 8)(), ctypes.byref(answer))
            self.assertEqual(answer.value, 20)


if __name__ == "__main__":
    unittest.main()
