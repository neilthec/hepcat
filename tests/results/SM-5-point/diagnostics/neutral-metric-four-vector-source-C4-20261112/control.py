import subprocess,json,time,pathlib,os,signal,hashlib
r=pathlib.Path(__file__).resolve().parent
start=time.monotonic()
with (r/'pilot.log').open('w') as out:
 p=subprocess.Popen(['/Applications/Wolfram.app/Contents/MacOS/WolframKernel','-script',str(r/'pilot.wl')],stdout=out,stderr=subprocess.STDOUT,start_new_session=True)
 try:
  rc=p.wait(timeout=570)
 except subprocess.TimeoutExpired:
  os.killpg(p.pid,signal.SIGTERM)
  try:p.wait(timeout=10)
  except subprocess.TimeoutExpired:os.killpg(p.pid,signal.SIGKILL);p.wait()
  rc=124
(r/'controller.json').write_text(json.dumps({'ExitCode':rc,'Seconds':time.monotonic()-start,'ScriptSHA256':hashlib.sha256((r/'pilot.wl').read_bytes()).hexdigest(),'OriginalSource':'git HEAD 7062bef source snapshot','ProductionSourceChanged':False},indent=2)+'\n')
print((r/'controller.json').read_text())
raise SystemExit(rc)
