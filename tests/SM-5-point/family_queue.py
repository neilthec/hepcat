"""Run disjoint family jobs with bounded concurrency and one result writer."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
from concurrent.futures import FIRST_COMPLETED, ThreadPoolExecutor, wait
import threading


def run_family_queue(families, worker, on_result, *, max_workers=2):
    """Call on_result only on the caller thread, once per started job.

    A false callback stops dispatch and drains already running jobs. Exceptions
    from a worker are delivered to that same callback instead of dropping its
    record. An interrupt caught while waiting stops dispatch, drains active
    jobs through the callback, and is then re-raised. A callback exception
    propagates after the executor waits for active workers to finish; callbacks
    not yet invoked are not replayed. Their raw job files remain worker-owned.
    Each worker owns its distinct case/export files; the callback owns ledgers.
    """
    if not isinstance(max_workers, int) or isinstance(max_workers, bool) or not 1 <= max_workers <= 2:
        raise ValueError('One or two family workers are supported.')
    caller = threading.get_ident()
    pending = iter(families)
    active = {}
    started = completed = 0
    stopping = False
    interrupted = False
    with ThreadPoolExecutor(max_workers=max_workers, thread_name_prefix='sm5-family') as executor:
        def dispatch():
            nonlocal started
            try:
                family = next(pending)
            except StopIteration:
                return False
            active[executor.submit(worker, family)] = family
            started += 1
            return True
        for _ in range(max_workers):
            if not dispatch():
                break
        while active:
            try:
                finished, _ = wait(active, return_when=FIRST_COMPLETED)
            except KeyboardInterrupt:
                stopping = interrupted = True
                continue
            for future in finished:
                family = active.pop(future)
                result = error = None
                try:
                    result = future.result()
                except BaseException as caught:
                    error = caught
                assert threading.get_ident() == caller
                keep_dispatching = on_result(family, result, error)
                completed += 1
                if not keep_dispatching:
                    stopping = True
            if not stopping:
                while len(active) < max_workers and dispatch():
                    pass
    if interrupted:
        raise KeyboardInterrupt
    return {'Started': started, 'Completed': completed, 'StoppedDispatch': stopping}
