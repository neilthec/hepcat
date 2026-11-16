"""Worker sleep protection and clock evidence, without physics calculations."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from contextlib import redirect_stdout
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch

ROOT=Path(__file__).resolve().parents[2]
spec=importlib.util.spec_from_file_location('sm5_worker_lifecycle',ROOT/'tests/SM-5-point/run.py')
run=importlib.util.module_from_spec(spec);spec.loader.exec_module(run)

class SleepGuardTests(unittest.TestCase):
    def test_other_platforms_do_not_launch_a_helper(self):
        with patch.object(run.sys,'platform','linux'),patch.object(run.subprocess,'Popen') as launch:
            helper,info=run.start_worker_sleep_guard(SimpleNamespace(pid=17))
        self.assertIsNone(helper);self.assertEqual(info['Status'],'NOT_NEEDED');launch.assert_not_called()

    def test_missing_utility_is_explicit_and_nonfatal(self):
        with patch.object(run.sys,'platform','darwin'),patch.object(run.shutil,'which',return_value=None),patch.object(run.subprocess,'Popen') as launch:
            helper,info=run.start_worker_sleep_guard(SimpleNamespace(pid=17))
        self.assertIsNone(helper);self.assertEqual(info['Status'],'UNAVAILABLE');launch.assert_not_called()

    def test_guard_tracks_actual_worker_and_owns_its_own_session(self):
        guard=Mock(pid=23)
        with patch.object(run.sys,'platform','darwin'),patch.object(run.shutil,'which',return_value='/usr/bin/caffeinate'),patch.object(run.subprocess,'Popen',return_value=guard) as launch:
            helper,info=run.start_worker_sleep_guard(SimpleNamespace(pid=17))
        self.assertIs(helper,guard);self.assertEqual(info['WorkerPID'],17)
        launch.assert_called_once_with(['/usr/bin/caffeinate','-i','-w','17'],stdin=subprocess.DEVNULL,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL,start_new_session=True)

    def test_helper_startup_failure_does_not_fail_physics(self):
        with patch.object(run.sys,'platform','darwin'),patch.object(run.shutil,'which',return_value='/usr/bin/caffeinate'),patch.object(run.subprocess,'Popen',side_effect=OSError('synthetic unavailable')):
            helper,info=run.start_worker_sleep_guard(SimpleNamespace(pid=17))
        self.assertIsNone(helper);self.assertEqual(info['Status'],'UNAVAILABLE')

    def test_completed_guard_is_reaped_without_termination(self):
        guard=Mock();guard.poll.return_value=0
        run.stop_worker_sleep_guard(guard)
        guard.terminate.assert_not_called();guard.kill.assert_not_called();guard.wait.assert_called_once_with(timeout=2)

    def test_live_guard_is_terminated_and_reaped(self):
        guard=Mock();guard.poll.return_value=None
        run.stop_worker_sleep_guard(guard)
        guard.terminate.assert_called_once();guard.wait.assert_called_once_with(timeout=2)

    def test_stubborn_guard_is_killed_without_touching_worker(self):
        guard=Mock();guard.poll.return_value=None;guard.wait.side_effect=[subprocess.TimeoutExpired('guard',2),0]
        run.stop_worker_sleep_guard(guard)
        guard.terminate.assert_called_once();guard.kill.assert_called_once();self.assertEqual(guard.wait.call_count,2)

    def test_cleanup_failure_is_nonfatal(self):
        guard=Mock();guard.poll.side_effect=OSError('already gone')
        run.stop_worker_sleep_guard(guard);run.stop_worker_sleep_guard(None)

class ControllerClockTests(unittest.TestCase):
    def record(self,wall,monotonic):
        with tempfile.TemporaryDirectory() as d:
            path=Path(d)/'controller'/'attempt.json'
            with patch.object(run.time,'time',return_value=wall),patch.object(run.time,'monotonic',return_value=monotonic):
                run.record_worker_controller(path,{'Kind':'construct','RandomSeed':71},100,20,{'Status':'NOT_NEEDED'})
            return json.loads(path.read_text())

    def test_matching_clocks_record_durations_without_claiming_pass(self):
        d=self.record(110,30)
        self.assertEqual(d['Status'],'WORKER_CONTROLLER');self.assertEqual(d['RandomSeed'],71)
        self.assertEqual(d['MonotonicElapsedSeconds'],10);self.assertFalse(d['ClockDiscrepancyDetected'])
        self.assertNotIn('CheckStatuses',d);self.assertNotIn('CapsuleSHA256',d)

    def test_clock_jump_is_a_diagnostic_only(self):
        d=self.record(410,30)
        self.assertEqual(d['WallMinusMonotonicSeconds'],300);self.assertTrue(d['ClockDiscrepancyDetected'])
        self.assertEqual(d['Status'],'WORKER_CONTROLLER')

    def test_backward_wall_adjustment_is_also_reported(self):
        d=self.record(90,30)
        self.assertEqual(d['WallMinusMonotonicSeconds'],-20);self.assertTrue(d['ClockDiscrepancyDetected'])

    def test_controller_write_failure_does_not_replace_worker_outcome(self):
        with tempfile.TemporaryDirectory() as d,patch.object(run,'atomic_json',side_effect=OSError('full disk')),patch.object(run.sys,'stderr',new_callable=io.StringIO) as errors:
            run.record_worker_controller(Path(d)/'attempt.json',{},0,0,{})
            self.assertIn('full disk',errors.getvalue())

class WorkerLifecycleTests(unittest.TestCase):
    def invoke(self,mode):
        temporary=tempfile.TemporaryDirectory();self.addCleanup(temporary.cleanup)
        root=Path(temporary.name);here=root/'tests/SM-5-point';results=root/'tests/results/SM-5-point'
        here.mkdir(parents=True);results.mkdir(parents=True)
        config=results/'configuration.json';config.write_text('{"ContextSHA256":"synthetic"}')
        worker=Mock(pid=31,returncode=1)
        if mode=='interrupt':worker.wait.side_effect=KeyboardInterrupt
        elif mode=='timeout':worker.wait.side_effect=subprocess.TimeoutExpired('worker',1)
        else:worker.wait.return_value=1
        guard=Mock(pid=32);guard.poll.return_value=None
        patches=[patch.object(run,'ROOT',root),patch.object(run,'HERE',here),patch.object(run,'RESULTS',results),
            patch.object(run,'context_sha256',return_value='synthetic'),patch.object(run.subprocess,'Popen',side_effect=OSError('synthetic startup') if mode=='startup' else None,return_value=worker),
            patch.object(run,'start_worker_sleep_guard',side_effect=KeyboardInterrupt if mode=='helper_interrupt' else None,return_value=(guard,{'Status':'STARTED','PID':32,'WorkerPID':31})),patch.object(run,'stop_process')]
        entered=[p.start() for p in patches]
        try:
            args=SimpleNamespace(kernel='synthetic',timeout=1,export_timeout=1)
            with redirect_stdout(io.StringIO()):
                if mode in ('interrupt','helper_interrupt'):
                    with self.assertRaises(KeyboardInterrupt):run.run_worker(args,'construct','case',config)
                    answer=None
                else:answer=run.run_worker(args,'construct','case',config)
        finally:
            for p in reversed(patches):p.stop()
        files=list((results/'controller').glob('*.json'));self.assertEqual(len(files),1)
        record=json.loads(files[0].read_text())
        self.assertEqual(record['Identifier'],'case');self.assertEqual(record['ContextSHA256'],'synthetic')
        self.assertNotIn('case.controller.json',[p.name for p in results.iterdir()])
        if mode=='startup':
            entered[5].assert_not_called();guard.terminate.assert_not_called()
        elif mode=='helper_interrupt':
            entered[5].assert_called_once_with(worker);guard.terminate.assert_not_called();worker.wait.assert_not_called()
        else:
            entered[5].assert_called_once_with(worker);guard.terminate.assert_called_once();guard.wait.assert_called_once_with(timeout=2)
        return answer,record,entered[6],worker

    def test_normal_exit_reaps_guard_without_changing_error_status(self):
        answer,record,stop,worker=self.invoke('normal')
        self.assertEqual(answer['Status'],'ERROR');stop.assert_not_called();self.assertEqual(record['WorkerPID'],31)

    def test_external_timeout_stops_worker_and_reaps_guard(self):
        answer,_,stop,worker=self.invoke('timeout')
        self.assertEqual(answer['Status'],'TIMEOUT');stop.assert_called_once_with(worker)

    def test_user_interrupt_stops_worker_and_reaps_guard(self):
        _,_,stop,worker=self.invoke('interrupt');stop.assert_called_once_with(worker)

    def test_interrupt_during_helper_startup_also_stops_worker(self):
        _,_,stop,worker=self.invoke('helper_interrupt');stop.assert_called_once_with(worker)

    def test_worker_startup_failure_still_records_controller(self):
        answer,record,stop,_=self.invoke('startup')
        self.assertEqual(answer['Status'],'ERROR');self.assertEqual(record['IdleSleepGuard']['Status'],'NOT_STARTED');stop.assert_not_called()

if __name__=='__main__':unittest.main()
