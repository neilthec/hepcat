#!/usr/bin/env python3
"""Compatibility entry point for the shared CalcHEP precision audit."""
from pathlib import Path
import runpy

_tool = Path(__file__).resolve().parents[2] / 'source/internal/export/calchep-precision.py'
if __name__ == '__main__':
    runpy.run_path(str(_tool), run_name='__main__')
else:
    globals().update({name: value for name, value in runpy.run_path(str(_tool)).items()
                      if not name.startswith('__')})
