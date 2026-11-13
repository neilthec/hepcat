"""Portable concurrency controls, with no Wolfram or external numerical jobs."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from pathlib import Path
import importlib.util
import threading
import time
import unittest

ROOT=Path(__file__).resolve().parents[2]
spec=importlib.util.spec_from_file_location('sm5_test_family_queue',ROOT/'tests/SM-5-point/family_queue.py')
queue=importlib.util.module_from_spec(spec);spec.loader.exec_module(queue)

class FamilyQueueTests(unittest.TestCase):
    def test_two_workers_overlap_but_never_exceed_two(self):
        barrier=threading.Barrier(2);lock=threading.Lock();active=peak=0;events=[];writer=threading.get_ident();accepted=[]
        def worker(item):
            nonlocal active,peak
            with lock:active+=1;peak=max(peak,active)
            if item<2:barrier.wait(timeout=2)
            events.append((item,'native'));time.sleep(.01);events.append((item,'export'))
            with lock:active-=1
            return {'ID':item,'Status':'PASS'}
        def accept(item,result,error):
            self.assertEqual(threading.get_ident(),writer);self.assertIsNone(error);accepted.append(item);return True
        result=queue.run_family_queue(range(6),worker,accept)
        self.assertEqual(peak,2);self.assertEqual(sorted(accepted),list(range(6)))
        self.assertEqual(result,{'Started':6,'Completed':6,'StoppedDispatch':False})
        for item in range(6):self.assertLess(events.index((item,'native')),events.index((item,'export')))
    def test_failed_family_stops_dispatch_and_preserves_active_success(self):
        barrier=threading.Barrier(2);released=threading.Event();started=[];accepted=[]
        def worker(item):
            started.append(item);barrier.wait(timeout=2)
            if item==0:return 'FAIL'
            self.assertTrue(released.wait(2));return 'PASS'
        def accept(item,result,error):
            accepted.append((item,result,error))
            if result=='FAIL':released.set();return False
            return True
        result=queue.run_family_queue(range(5),worker,accept)
        self.assertEqual(sorted(started),[0,1]);self.assertEqual({x[1]for x in accepted},{'FAIL','PASS'})
        self.assertTrue(result['StoppedDispatch']);self.assertEqual(result['Completed'],2)
    def test_worker_exception_is_recorded_and_other_started_job_is_drained(self):
        barrier=threading.Barrier(2);released=threading.Event();accepted=[]
        def worker(item):
            barrier.wait(timeout=2)
            if item==0:raise RuntimeError('controlled worker error')
            self.assertTrue(released.wait(2));return 'PASS'
        def accept(item,result,error):
            accepted.append((item,result,error))
            if error is not None:released.set();return False
            return True
        result=queue.run_family_queue(range(4),worker,accept)
        self.assertEqual(result['Started'],2);self.assertEqual(result['Completed'],2)
        self.assertTrue(any(isinstance(x[2],RuntimeError)for x in accepted))
        self.assertIn((1,'PASS',None),accepted)
    def test_one_worker_is_strictly_serial(self):
        order=[]
        result=queue.run_family_queue(range(3),lambda x:order.append(x)or x,lambda *args:True,max_workers=1)
        self.assertEqual(order,[0,1,2]);self.assertEqual(result['Completed'],3)
    def test_empty_queue_creates_no_job_or_callback(self):
        self.assertEqual(queue.run_family_queue([],lambda _:self.fail(),lambda *_:self.fail()),{'Started':0,'Completed':0,'StoppedDispatch':False})
    def test_invalid_worker_limits_are_rejected_before_dispatch(self):
        for workers in(0,3,-1,True,1.5):
            with self.subTest(workers=workers),self.assertRaises(ValueError):queue.run_family_queue([],lambda _:None,lambda *_:True,max_workers=workers)

if __name__=='__main__':unittest.main()
