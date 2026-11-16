"""Exact checkpoint writer regression, with no package load or physics job."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT=Path(__file__).resolve().parents[2]
KERNEL=shutil.which('WolframKernel') or '/Applications/Wolfram.app/Contents/MacOS/WolframKernel'

@unittest.skipUnless(Path(KERNEL).is_file(),'A licensed Wolfram kernel is required.')
class CheckpointSerializationTests(unittest.TestCase):
    def test_actual_writers_match_original_bytes_and_round_trip(self):
        definitions=[]
        for name,end in [('evaluate.wl','(* Public-call captures'),('revalidate.wl','writeSummary[')]:
            text=(ROOT/'tests/SM-5-point'/name).read_text()
            start=text.index('writeRecord[');definition=text[start:text.index(end,start)]
            self.assertIn('BinarySerialize[',definition)
            self.assertNotIn('Normal[bytes]',definition)
            definitions.append(definition)
        with tempfile.TemporaryDirectory(prefix='hepcat-sm5-serialization-') as directory:
            script=Path(directory)/'test.wl'
            prelude='''$HistoryLength=0;
fixtureDirectory=FIXTURE_DIRECTORY;
fixtureData=BlockRandom[SeedRandom[717];<|"Status"->"SYNTHETIC_SERIALIZATION_FIXTURE", "Payload"->ByteArray[RandomInteger[{0,255},262144]],"ExactValues"->{1/3,17/19,HoldComplete[checkpointVariable^2+1]},"Nested"->{<|"Samples"->Range[4]|>}|>];
fixtureBytes=BinarySerialize[fixtureData,PerformanceGoal->"Size"];
fixtureBaseline=FileNameJoin[{fixtureDirectory,"old.wxf"}];
fixtureStream=OpenWrite[fixtureBaseline,BinaryFormat->True];BinaryWrite[fixtureStream,Normal[fixtureBytes],"Byte"];Close[fixtureStream];
fixtureResults={};
'''.replace('FIXTURE_DIRECTORY',json.dumps(directory))
            body=''
            for i,definition in enumerate(definitions):
                body+='Begin["CheckpointWriter'+str(i)+'`Private`"];\n'+definition+'\n'
                body+='''fixtureFile=FileNameJoin[{Global`fixtureDirectory,"writerINDEX.wxf"}];
writeRecord[fixtureFile,<|"Old"->True|>];writeRecord[fixtureFile,Global`fixtureData];
AppendTo[Global`fixtureResults,<|"BytesMatch"->(BinaryReadList[fixtureFile,"Byte"]===Normal[Global`fixtureBytes]),"RoundTrip"->(BinaryDeserialize[ByteArray[BinaryReadList[fixtureFile,"Byte"]]]===Global`fixtureData),"TemporaryRemoved"->!FileExistsQ[fixtureFile<>".tmp"],"Nonempty"->(FileByteCount[fixtureFile]>262144)|>];
End[];
'''.replace('INDEX',str(i))
            suffix='''Print["SERIALIZATION_JSON:",ExportString[<|"Writers"->fixtureResults,"Messages"->ToString[$MessageList,InputForm]|>,"RawJSON","Compact"->True]];Quit[If[And@@Flatten[Values/@fixtureResults]&&$MessageList==={},0,1]];'''
            script.write_text('MemoryConstrained[TimeConstrained[\n'+prelude+body+suffix+'\n,20,Quit[2]],128*1024^2,Quit[3]];')
            result=subprocess.run([KERNEL,'-script',str(script)],capture_output=True,text=True,timeout=30)
        self.assertEqual(result.returncode,0,result.stdout+result.stderr)
        lines=[line.split('SERIALIZATION_JSON:',1)[1] for line in result.stdout.splitlines() if 'SERIALIZATION_JSON:' in line]
        self.assertEqual(len(lines),1,result.stdout+result.stderr)
        report=json.loads(lines[0]);self.assertEqual(report['Messages'],'{}')
        self.assertEqual(len(report['Writers']),2)
        for row in report['Writers']:
            self.assertTrue(all(row.values()),row)

if __name__=='__main__':unittest.main()
