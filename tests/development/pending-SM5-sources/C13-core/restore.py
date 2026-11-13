#!/usr/bin/env python3
"""Restore reviewed patches only into a new isolated source copy. Never load it."""
from pathlib import Path
import argparse,hashlib,json,re,shutil
ARCHIVE=Path(__file__).resolve().parent
sha=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
def inside(path,root):
 try:path.relative_to(root);return True
 except ValueError:return False
def verify_baseline(root,manifest):
 for name,digest in manifest['BaselineFilesSHA256'].items():
  path=root/name
  if not path.is_file()or path.is_symlink()or sha(path)!=digest:raise ValueError('Baseline mismatch: '+name)
def apply_exact(before,patch):
 original=before.splitlines(keepends=True);lines=patch.splitlines(keepends=True);result=[];cursor=0;i=0
 while i<len(lines):
  match=re.match(r'^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@',lines[i])
  if not match:i+=1;continue
  position=max(0,int(match.group(1))-1);assert position>=cursor
  result.extend(original[cursor:position]);cursor=position;oldcount=newcount=0;i+=1
  while i<len(lines)and not lines[i].startswith('@@ '):
   line=lines[i];i+=1
   if line.startswith('\\ No newline'):raise ValueError('Unexpected no-newline marker')
   assert line and line[0]in' +-'
   if line[0]in' -':assert cursor<len(original)and original[cursor]==line[1:];cursor+=1;oldcount+=1
   if line[0]in' +':result.append(line[1:]);newcount+=1
  assert oldcount==int(match.group(2)or 1)and newcount==int(match.group(4)or 1)
 result.extend(original[cursor:]);return''.join(result)
def main():
 p=argparse.ArgumentParser(description=__doc__);p.add_argument('--baseline',required=True,type=Path);p.add_argument('--output',required=True,type=Path);args=p.parse_args();base=args.baseline.resolve(strict=True);out=args.output.resolve();manifest=json.loads((ARCHIVE/'manifest.json').read_text())
 if out.exists()or inside(out,base)or inside(out,ARCHIVE):raise ValueError('Output must be new and outside baseline/archive')
 verify_baseline(base,manifest)
 for item in manifest['Files']:assert sha(ARCHIVE/item['Patch'])==item['PatchSHA256']
 out.mkdir(parents=True);shutil.copytree(base/'source',out/'source');usage=out/'Documentation/English/SymbolUsage.wl';usage.parent.mkdir(parents=True);shutil.copy2(base/'Documentation/English/SymbolUsage.wl',usage)
 for item in manifest['Files']:
  relative=Path(item['Path']);assert not relative.is_absolute()and'..'not in relative.parts and relative.parts[0]=='source'
  target=out/relative;assert sha(target)==item['BaselineSHA256'];target.write_text(apply_exact(target.read_text(),(ARCHIVE/item['Patch']).read_text()));assert sha(target)==item['CandidateSHA256']
 extra=(out/'source/internal/HEPCAT-QCDCurrents.wl').read_bytes()[manifest['AppendedQCDBasePrefixBytes']+1:];assert hashlib.sha256(extra).hexdigest()==manifest['AppendedQCDDefinitionsSHA256'];(out/'reference-extension.wl').write_bytes(extra)
 verify_baseline(base,manifest)
 (out/'restored-manifest.json').write_text(json.dumps({'Status':'ISOLATED_CODE_RESTORED_NOT_LOADED','ArchiveManifestSHA256':sha(ARCHIVE/'manifest.json'),'ChangedFilesSHA256':{v['Path']:sha(out/v['Path'])for v in manifest['Files']},'BaselineUnchanged':True},indent=2)+'\n');print(out)
if __name__=='__main__':main()
