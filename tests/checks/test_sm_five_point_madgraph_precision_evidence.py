"""Synthetic read-only evidence checks. No physics kernel or compiler runs."""
# HEPCAT - High Energy Physics Constructive Amplitude Tool
# Copyright (C) 2026 Neil Christensen
# SPDX-License-Identifier: GPL-3.0-or-later
import copy
import csv
from decimal import Decimal, localcontext
import gzip
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import struct
import tempfile
import unittest
from unittest import mock

SCRIPT = Path(__file__).resolve().parents[1] / 'SM-5-point/madgraph_precision_evidence.py'
SPEC = importlib.util.spec_from_file_location('sm5_mg_precision', SCRIPT)
E = importlib.util.module_from_spec(SPEC); SPEC.loader.exec_module(E)


def sha(path): return hashlib.sha256(path.read_bytes()).hexdigest()
def write(path, data):
    path.parent.mkdir(parents=True, exist_ok=True); path.write_text(json.dumps(data))
def csvfile(path, fields, rows):
    path.parent.mkdir(parents=True, exist_ok=True)
    with (gzip.open(path,'wt',newline='') if path.suffix=='.gz' else path.open('w',newline='')) as f:
        writer=csv.DictWriter(f,fieldnames=fields);writer.writeheader();writer.writerows(rows)


class MadGraphPrecisionTests(unittest.TestCase):
    def setUp(self):
        tmp=tempfile.TemporaryDirectory(prefix='hepcat-mg-evidence-');self.addCleanup(tmp.cleanup)
        self.root=Path(tmp.name);self.results=self.root/'tests/results/SM-5-point';self.here=self.root/'tests/SM-5-point'
        self.folder=self.results/'precision/Demo';self.export=self.root/'exports/Demo';self.folder.mkdir(parents=True);self.export.mkdir(parents=True)
        (self.root/'source').mkdir();(self.root/'source/example.wl').write_text('(* synthetic source fingerprint only *)')
        self.here.mkdir(parents=True);write(self.here/'inventory.json',{})
        self.runtime=self.root/'runtime';self.runtime.mkdir();self.spin=self.runtime/'libspinas.a';self.spin.write_bytes(b'synthetic library')
        self.compiler=self.root/'compiler';self.compiler.write_bytes(b'synthetic reviewed compiler identity')
        self.configuration={'SPINASDirectory':str(self.runtime),'MadGraphDirectory':str(self.root/'mg'),'CalcHEPDirectory':str(self.root/'ch')}
        self.context=E.stage_context.fingerprint(E.stage_context.manifest(self.root,self.here,self.configuration))
        write(self.results/'configuration.json',{**self.configuration,'ContextSHA256':self.context})
        self.patchroot=mock.patch.object(E,'ROOT',self.root);self.patchhere=mock.patch.object(E,'HERE',self.here);self.patchroot.start();self.patchhere.start();self.addCleanup(self.patchroot.stop);self.addCleanup(self.patchhere.stop)
        self.family={'ID':'family_demo','CaseIDs':['case_a'],'ExportName':'Demo','Particles':['v','v','v','v'],'LabeledHelicityAssignmentCount':2}
        cap=self.results/'case_a.wxf';cap.write_bytes(b'synthetic complete native capsule')
        self.cases={'case_a':{'ID':'case_a','Status':'COMPLETE','ContextSHA256':self.context,'Rows':3,'FeynmanRows':1,'CheckStatuses':{'PASS':3},'CapsuleSHA256':sha(cap)}}
        write(self.results/'case_a.json',self.cases['case_a']);exportcap=self.results/'family_demo-export.wxf';exportcap.write_bytes(b'raw backend FAIL remains intact')
        self.hashes={'case_a':sha(cap)};self.contexts={'case_a':self.context}
        self.export_summary={'FamilyID':'family_demo','Status':'FAIL','CalcHEPStatus':'PASS','MadGraphStatus':'FAIL','Points':4,'ContextSHA256':self.context,'ExporterContextSHA256':self.context,'NativeCaseContexts':self.contexts,'CaseIDs':['case_a'],'CaseCapsulesSHA256':self.hashes,'CapsuleSHA256':sha(exportcap)}
        write(self.results/'family_demo-export.json',self.export_summary)
        self.states=[(-2,-2,-2,-2),(2,2,2,2)];self.norm=.125
        manifest={'Class':'Demo','Masses':[0,0,0,0],'HelicityConfigurations':[list(s)for s in self.states],'SymmetryFactor':.5,'InitialSpinAverage':4,'InitialColorAverage':1}
        write(self.export/'manifest.json',manifest)
        for name in ('process.cpp','process.hpp','madgraph-validation.cpp','madgraph-runtime.py','madgraph-runtime.hpp','spinas-runtime.hpp','spinas-validation.hpp'):(self.export/name).write_text('synthetic project '+name)
        self.lib=self.export/'build/libDemo_process.a';self.lib.parent.mkdir();self.lib.write_bytes(b'synthetic compiled native project')
        self.validation_lib=self.export/'build/libDemo_madgraph_validation.a';self.validation_lib.write_bytes(b'synthetic validation adapter archive')
        command=self.export/'build/CMakeFiles/Demo_validate_madgraph.dir/link.txt';command.parent.mkdir(parents=True);command.write_text('c++ -o validate '+str(self.lib)+' '+str(self.validation_lib)+' '+str(self.spin))
        self.points={p:[float(p+j)for j in range(16)]for p in range(1,5)}
        csvfile(self.folder/'points.csv',['Point']+[f'p{i}_{j}'for i in range(1,5)for j in range(4)], [dict(Point=p,**{f'p{i}_{j}':v[(i-1)*4+j]for i in range(1,5)for j in range(4)})for p,v in self.points.items()])
        (self.folder/'points.f64').write_bytes(b''.join(struct.pack('<16d',*self.points[p])for p in range(1,5)))
        self.rawrows=[]
        for i,state in enumerate(self.states):
            self.rawrows.append({'Helicities':list(state),'ExpectedExactZero':i==0,'ZeroAbsoluteTolerance':1e-22,'Status':'FAIL','Points':4,'RequestedPoints':4,'GeneratedPoints':4,'ComparedPoints':4,'Mismatches':1,'EvaluationErrors':0,'FirstFailure':{'Point':2,'Momenta':[self.points[2][j:j+4]for j in range(0,16,4)],'HEPCATValue':float(i),'MadGraphValue':3e-22 if i==0 else 1.0000002}})
        raw={'Status':'FAIL','Seed':2**63+123,'Rows':self.rawrows};write(self.folder/'raw.json',raw)
        write(self.folder/'failed-point.json',{'Point':2,'Seed':raw['Seed'],'Momenta':self.rawrows[0]['FirstFailure']['Momenta'],'FailedRows':self.rawrows})
        native=[];ref53=[];comparisons=[];quad=['CORE_PRECISION_BITS 113 CORE_STORAGE_BITS 128 INPUT_STORAGE_BITS 64\n'];maxima={s:[Decimal(0),Decimal(0)]for s in self.states}
        for point in range(1,5):
            for index,state in enumerate(self.states):
                v=float(index);native.append({'point':point,**{f'h{i}':h for i,h in enumerate(state,1)},'normalized':repr(v),'raw':repr(v/self.norm)})
                raw53=(24e-22 if index==0 else 8.0000016)if point==2 else (0. if index==0 else 8.)
                ref=raw53*self.norm;err=abs(ref-v);scale=max(abs(ref),abs(v));rel=err/scale if scale else 0.;passed=err<=(1e-22 if index==0 else 1e-7*scale)
                ref53.append({'Point':point,**{f'H{i}':h for i,h in enumerate(state,1)},'Native53Normalized':repr(v),'MadGraph53Raw':repr(raw53),'MadGraph53Normalized':repr(ref),'AbsoluteError':repr(err),'RelativeError':repr(rel),'Passed':int(passed)})
                q=Decimal('8e-23')if index==0 else Decimal(8);quad.append(' '.join(map(str,(point,*state,q)))+'\n')
                with localcontext()as c:
                    c.prec=80;value=Decimal.from_float(v);reference=q*Decimal.from_float(self.norm);error=abs(reference-value);scale=max(abs(reference),abs(value));relative=error/scale if scale else Decimal(0)
                    comparisons.append({'Point':point,**{f'H{i}':h for i,h in enumerate(state,1)},'Native53Normalized':str(value),'MadGraph113Raw':str(q),'MadGraph113Normalized':str(reference),'AbsoluteError':str(error),'RelativeError':str(relative),'Passed':1});maxima[state][0]=max(maxima[state][0],error);maxima[state][1]=max(maxima[state][1],relative)
        csvfile(self.folder/'native.csv.gz',list(native[0]),native);csvfile(self.folder/'double.csv.gz',list(ref53[0]),ref53);csvfile(self.folder/'comparisons.csv.gz',list(comparisons[0]),comparisons)
        with gzip.open(self.folder/'quad.txt.gz','wt')as f:f.write(''.join(quad))
        csvfile(self.folder/'native-53.csv',[f'h{i}'for i in range(1,5)]+['normalized'],[{**{f'h{i}':h for i,h in enumerate(s,1)},'normalized':str(float(j))}for j,s in enumerate(self.states)])
        write(self.folder/'raw-export-summary.json',self.export_summary)
        write(self.folder/'runtime.json',E.stage_context.manifest(self.root,self.here,self.configuration))
        write(self.folder/'project.json',{str(p.relative_to(self.export)):sha(p)for p in self.export.rglob('*')if p.is_file()and p.name not in ('link.txt','libDemo_madgraph_validation.a')})
        generated=self.folder/'generated-source/process/SubProcesses/P_demo/matrix.f';generated.parent.mkdir(parents=True);generated.write_text('synthetic reviewed matrix')
        write(self.folder/'generated.json',{'process/SubProcesses/P_demo/matrix.f':sha(generated)})
        for name in ('quad-stream.f90','quad-same-source-build.json','build-same-source-quad.py'):(self.folder/name).write_text('synthetic root-reviewed ABI fixture')
        control={'HistoricalAllPointNativeValuesWereSaved':False,'NoHistoricalAllPointBitIdentityClaim':True,'Seed':raw['Seed'],'Points':4,'Rows':8,'HelicitiesPerPoint':2}
        write(self.folder/'control.json',control)
        self.report={'Backend':'MadGraph','Status':'PRECISION_REPLAY_PASS','FamilyID':'family_demo','ContextSHA256':self.context,'ExportCapsuleSHA256':sha(exportcap),'CaseCapsulesSHA256':self.hashes,'NativeCaseContexts':self.contexts,'Points':4,'HelicityRows':2,'ComparedHelicityPoints':8,'InputStorageBits':64,'CorePrecisionBits':113,'CoreStorageBits':128,'InputKinematicsCorrected':False,'OtherAbsoluteTolerance':0,'Seed':raw['Seed'],'RelativeTolerance':str(Decimal.from_float(1e-7)),'GeneralExactZeroAbsoluteTolerance':str(Decimal.from_float(1e-22)),'NormalizationBinary64Hex':self.norm.hex(),'Mismatches':0,'EvaluationErrors':0,'RawCalcHEPStatus':'PASS','RawMadGraphStatus':'FAIL','RawOriginalExportStatusUnchanged':'FAIL','QualificationOrCanonicalPromotionPerformed':False,'AllGeneratedFortranSourceAndCardFilesIdentical53And113':True,'PointsFile':'points.csv','Binary64PointsFile':'points.f64','NativeValuesFile':'native.csv.gz','Reference53ValuesFile':'double.csv.gz','Reference113ValuesFile':'quad.txt.gz','IndependentComparisonFile':'comparisons.csv.gz','KnownFailedPointFile':'failed-point.json','RawReportFile':'raw.json','SourceRuntimeManifestFile':'runtime.json','ProjectHashesFile':'project.json','GeneratedSourceHashesFile':'generated.json','GeneratedSourceFileCount':1,'NativeDoubleControlFile':'control.json','OriginalKnownFailedPointAndNativeBits':control,'CompilerPath':str(self.compiler),'CompilerSHA256':sha(self.compiler),'NativeGeneratedLibraryPath':str(self.lib),'NativeGeneratedLibrarySHA256':sha(self.lib),'SPINASLibraryPath':str(self.spin),'SPINASLibrarySHA256':sha(self.spin),'Rows':[{'Helicities':list(s),'ExpectedExactZero':i==0,'Status':'PASS','ComparedPoints':4,'Mismatches':0,'EvaluationErrors':0,'MaximumAbsoluteError':str(maxima[s][0]),'MaximumRelativeError':str(maxima[s][1])}for i,s in enumerate(self.states)]}
        self.review={'Status':'REVIEWED_FULL_SAME_INPUT_MADGRAPH_PRECISION_REPLAY','ContextSHA256':self.context,'ExportCapsuleSHA256':sha(exportcap),'CompilerSHA256':sha(self.compiler),'ReviewedBy':'synthetic independent reviewer','ReviewedUTC':'2026-11-13T00:00:00Z',**{key:True for key in E.REVIEW_ASSERTIONS}}
        self.review['LinkCommandPath']=str(command.relative_to(self.export));self.review['LinkCommandSHA256']=sha(command);self.review['LinkedLibrariesSHA256']={str(p):sha(p)for p in (self.lib,self.validation_lib,self.spin)}
        self.link={'Backend':'MadGraph','FamilyID':'family_demo','ContextSHA256':self.context,'ExportCapsuleSHA256':sha(exportcap),'CaseCapsulesSHA256':self.hashes,'NativeCaseContexts':self.contexts,'Report':'precision/Demo/report.json','SemanticReview':'precision/Demo/review.json'}
        self.resign()

    def resign(self):
        self.report['Files']={str(p.relative_to(self.folder)):sha(p)for p in self.folder.rglob('*')if p.is_file()and p.name not in ('report.json','review.json')}
        self.report['RawReportSHA256']=self.report['Files'][self.report['RawReportFile']]
        write(self.folder/'report.json',self.report);self.link['ReportSHA256']=sha(self.folder/'report.json');self.review['ReportSHA256']=self.link['ReportSHA256'];self.review['EvidenceFiles']={n:self.report['Files'][n]for n in ('quad-stream.f90','quad-same-source-build.json','build-same-source-quad.py','generated.json','project.json','raw.json')}
        write(self.folder/'review.json',self.review);self.link['SemanticReviewSHA256']=sha(self.folder/'review.json');write(self.results/'family_demo-precision.json',self.link)

    def qualify(self):return E.qualification(self.family,self.export_summary,self.cases,self.context,self.results,self.export.parent,4)
    def test_complete_generic_packet_qualifies_without_changing_raw_status(self):
        before=copy.deepcopy(self.export_summary);q=self.qualify();self.assertIsNotNone(q);self.assertEqual(q['ComparedHelicityPoints'],8);self.assertEqual(q['RawMadGraphStatus'],'FAIL');self.assertEqual(before,self.export_summary)
    def test_default_requires_one_thousand_points(self):self.assertIsNone(E.qualification(self.family,self.export_summary,self.cases,self.context,self.results,self.export.parent))
    def test_missing_semantic_review_rejects(self): (self.folder/'review.json').unlink();self.assertIsNone(self.qualify())
    def test_unreviewed_abi_rejects(self):self.review['UnpromotedDriverExplicitBinary64ToQuad']=False;self.resign();self.assertIsNone(self.qualify())
    def test_report_edit_without_review_rebinding_rejects(self):self.report['Points']=5;write(self.folder/'report.json',self.report);self.assertIsNone(self.qualify())
    def test_changed_native_capsule_rejects(self):(self.results/'case_a.wxf').write_bytes(b'changed');self.assertIsNone(self.qualify())
    def test_changed_export_capsule_rejects(self):(self.results/'family_demo-export.wxf').write_bytes(b'changed');self.assertIsNone(self.qualify())
    def test_changed_project_rejects(self):(self.export/'process.cpp').write_text('changed');self.assertIsNone(self.qualify())
    def test_changed_library_rejects(self):self.lib.write_bytes(b'changed');self.assertIsNone(self.qualify())
    def test_changed_compiler_rejects(self):self.compiler.write_bytes(b'changed');self.assertIsNone(self.qualify())
    def test_changed_runtime_rejects(self):self.spin.write_bytes(b'changed');self.assertIsNone(self.qualify())
    def test_missing_mixed_native_context_rejects(self):self.link.pop('NativeCaseContexts');write(self.results/'family_demo-precision.json',self.link);self.assertIsNone(self.qualify())
    def test_lossy_seed_rejects(self):self.report['Seed']=float(self.report['Seed']);self.resign();self.assertIsNone(self.qualify())
    def test_wrong_seed_rejects(self):self.report['Seed']+=1;self.resign();self.assertIsNone(self.qualify())
    def test_repaired_kinematics_rejects(self):self.report['InputKinematicsCorrected']=True;self.resign();self.assertIsNone(self.qualify())
    def test_wrong_precision_rejects(self):self.report['CorePrecisionBits']=64;self.resign();self.assertIsNone(self.qualify())
    def test_expanded_relative_tolerance_rejects(self):self.report['RelativeTolerance']='1e-6';self.resign();self.assertIsNone(self.qualify())
    def test_expanded_zero_tolerance_rejects(self):self.report['GeneralExactZeroAbsoluteTolerance']='1e-21';self.resign();self.assertIsNone(self.qualify())
    def test_wrong_coverage_rejects(self):self.family['LabeledHelicityAssignmentCount']=3;self.assertIsNone(self.qualify())
    def test_duplicate_quad_row_rejects(self):
        p=self.folder/'quad.txt.gz'
        with gzip.open(p,'rt')as f:s=f.read()
        with gzip.open(p,'wt')as f:f.write(s+s.splitlines()[1]+'\n')
        self.resign();self.assertIsNone(self.qualify())
    def test_quad_disagreement_cannot_be_hidden_by_pass_fields(self):
        p=self.folder/'quad.txt.gz'
        with gzip.open(p,'rt')as f:s=f.read()
        with gzip.open(p,'wt')as f:f.write(s.replace(' 8\n',' 9\n',1))
        self.resign();self.assertIsNone(self.qualify())
    def test_point_bit_change_rejects_even_after_hash_rebinding(self):
        p=self.folder/'points.f64';b=bytearray(p.read_bytes());b[128]^=1;p.write_bytes(b);self.resign();self.assertIsNone(self.qualify())
    def test_wrong_saved_native_value_rejects(self):
        p=self.folder/'raw.json';d=json.loads(p.read_text());d['Rows'][1]['FirstFailure']['HEPCATValue']=2.;write(p,d);self.resign();self.assertIsNone(self.qualify())
    def test_missing_file_hash_cannot_skip_required_stream(self):
        self.report['Files'].pop('native.csv.gz');write(self.folder/'report.json',self.report);self.link['ReportSHA256']=sha(self.folder/'report.json');self.review['ReportSHA256']=self.link['ReportSHA256'];write(self.folder/'review.json',self.review);self.link['SemanticReviewSHA256']=sha(self.folder/'review.json');write(self.results/'family_demo-precision.json',self.link);self.assertIsNone(self.qualify())
    def test_forced_zero_on_nonzero_raw_state_rejects(self):
        self.report['Rows'][1]['ExpectedExactZero']=True;self.resign();self.assertIsNone(self.qualify())
    def test_false_nonzero_comparison_value_rejects(self):
        p=self.folder/'comparisons.csv.gz';rows=E._csv(p);rows[1]['RelativeError']='0.1';csvfile(p,list(rows[0]),rows);self.resign();self.assertIsNone(self.qualify())
    def test_path_escape_rejects(self):self.link['Report']='../outside.json';write(self.results/'family_demo-precision.json',self.link);self.assertIsNone(self.qualify())
    def test_raw_mg_pass_is_not_precision_failure_qualification(self):self.export_summary['MadGraphStatus']='PASS';self.assertIsNone(self.qualify())
    def test_raw_ch_failure_cannot_be_covered_by_mg_replay(self):self.export_summary['CalcHEPStatus']='FAIL';self.assertIsNone(self.qualify())
    def test_native_check_failure_rejects(self):self.cases['case_a']['CheckStatuses']={'PASS':2,'FAIL':1};self.assertIsNone(self.qualify())
    def test_standard_backend_dispatch_runs_the_complete_new_verifier(self):
        spec=importlib.util.spec_from_file_location('sm5_precision_dispatch_test',SCRIPT.with_name('precision_evidence.py'))
        base=importlib.util.module_from_spec(spec);spec.loader.exec_module(base)
        original=importlib.util.spec_from_file_location
        def fixture_spec(name,path,*args,**kwargs):
            result=original(name,path,*args,**kwargs)
            if Path(path).name=='madgraph_precision_evidence.py':
                execute=result.loader.exec_module
                def load(module):execute(module);module.ROOT=self.root;module.HERE=self.here
                result.loader.exec_module=load
            return result
        with mock.patch.object(base.importlib.util,'spec_from_file_location',side_effect=fixture_spec):
            value=base.qualification(self.family,self.export_summary,self.cases,self.context,self.results,self.export.parent,4)
        self.assertIsNotNone(value);self.assertEqual(value['Backend'],'MadGraph');self.assertEqual(value['ComparedHelicityPoints'],8)
    def test_unrecognized_backend_does_not_invoke_madgraph_verifier(self):
        spec=importlib.util.spec_from_file_location('sm5_precision_dispatch_other',SCRIPT.with_name('precision_evidence.py'))
        base=importlib.util.module_from_spec(spec);spec.loader.exec_module(base)
        self.link['Backend']='Other';write(self.results/'family_demo-precision.json',self.link)
        with mock.patch.object(base.importlib.util,'spec_from_file_location',side_effect=AssertionError('Wrong backend dispatch')):
            self.assertIsNone(base.qualification(self.family,self.export_summary,self.cases,self.context,self.results,self.export.parent,4))
    def historical_context(self):
        (self.root/'source/example.wl').write_text('(* reviewed new constructor implementation *)')
        return E.stage_context.fingerprint(E.stage_context.manifest(self.root,self.here,self.configuration))
    def test_current_manifest_must_equal_declared_current_context(self):
        self.historical_context();self.assertIsNone(self.qualify())
    def test_unadmitted_historical_context_rejects(self):
        current=self.historical_context()
        self.assertIsNone(E.qualification(self.family,self.export_summary,self.cases,current,self.results,self.export.parent,4))
    def test_admitted_historical_source_only_change_preserves_actual_context(self):
        current=self.historical_context();proof={'SyntheticVerifiedAdmission':True}
        with mock.patch.object(E.constructor_context,'admits_record',return_value=True) as admission, mock.patch.object(E.constructor_context,'public_proof',return_value={'Status':'VerifiedHistoricalEvidence'}):
            result=E.qualification(self.family,self.export_summary,self.cases,current,self.results,self.export.parent,4,proof)
        self.assertIsNotNone(result);self.assertEqual(admission.call_count,2);self.assertEqual(result['ContextSHA256'],self.context);self.assertEqual(result['ActiveExporterContextSHA256'],current);self.assertEqual(result['NativeCaseContexts'],self.contexts);self.assertEqual(result['ContextCompatibility']['Status'],'VerifiedHistoricalEvidence')
    def test_historical_checked_summary_annotations_do_not_relabel_raw_evidence(self):
        current=self.historical_context();proof={'SyntheticVerifiedAdmission':True}
        wrapped=copy.deepcopy(self.export_summary);wrapped['ContextCompatibility']={'Status':'VerifiedHistoricalEvidence'}
        cases=copy.deepcopy(self.cases);cases['case_a']['ContextCompatibility']={'Status':'VerifiedHistoricalEvidence'}
        with mock.patch.object(E.constructor_context,'admits_record',return_value=True), mock.patch.object(E.constructor_context,'public_proof',return_value={'Status':'VerifiedHistoricalEvidence'}):
            result=E.qualification(self.family,wrapped,cases,current,self.results,self.export.parent,4,proof)
        self.assertIsNotNone(result);self.assertEqual(result['RawExportStatus'],'FAIL');self.assertEqual(json.loads((self.results/'family_demo-export.json').read_text())['Status'],'FAIL')
    def test_historical_model_change_rejects_even_with_admission_stub(self):
        p=self.root/'models/vars1.mdl';p.parent.mkdir();p.write_text('changed model parameter')
        current=E.stage_context.fingerprint(E.stage_context.manifest(self.root,self.here,self.configuration))
        with mock.patch.object(E.constructor_context,'admits_record',return_value=True):self.assertIsNone(E.qualification(self.family,self.export_summary,self.cases,current,self.results,self.export.parent,4,{}))
    def test_historical_runtime_header_change_rejects_even_with_admission_stub(self):
        p=self.runtime/'include/runtime.h';p.parent.mkdir();p.write_text('changed numeric library interface')
        current=E.stage_context.fingerprint(E.stage_context.manifest(self.root,self.here,self.configuration))
        with mock.patch.object(E.constructor_context,'admits_record',return_value=True):self.assertIsNone(E.qualification(self.family,self.export_summary,self.cases,current,self.results,self.export.parent,4,{}))
    def test_changed_validation_adapter_archive_rejects(self):self.validation_lib.write_bytes(b'changed');self.assertIsNone(self.qualify())
    def test_changed_link_command_rejects(self):
        p=self.export/self.review['LinkCommandPath'];p.write_text(p.read_text()+' -O0');self.assertIsNone(self.qualify())
    def test_omitted_linked_archive_rejects(self):
        self.review['LinkedLibrariesSHA256'].pop(str(self.validation_lib));self.resign();self.assertIsNone(self.qualify())
    def test_unlinked_extra_archive_rejects(self):
        p=self.root/'extra.a';p.write_bytes(b'not used');self.review['LinkedLibrariesSHA256'][str(p)]=sha(p);self.resign();self.assertIsNone(self.qualify())
    def test_historical_full_stream_identity_claim_rejects(self):
        p=self.folder/'control.json';d=json.loads(p.read_text());d['HistoricalAllPointNativeValuesWereSaved']=True;write(p,d);self.report['OriginalKnownFailedPointAndNativeBits']=d;self.resign();self.assertIsNone(self.qualify())
    def test_changed_canonical_native_summary_rejects(self):
        p=self.results/'case_a.json';d=json.loads(p.read_text());d['Rows']=7;write(p,d);self.assertIsNone(self.qualify())
    def test_raw_double_control_must_reproduce_the_failure(self):
        p=self.folder/'double.csv.gz';rows=E._csv(p)
        for r in rows:
            r['MadGraph53Raw']='0'if r['H1']=='-2'else'8';r['MadGraph53Normalized']=r['Native53Normalized'];r['AbsoluteError']='0';r['RelativeError']='0';r['Passed']='1'
        csvfile(p,list(rows[0]),rows);self.resign();self.assertIsNone(self.qualify())
    def test_truncated_quad_archive_fails_closed(self):
        p=self.folder/'quad.txt.gz';p.write_bytes(p.read_bytes()[:20]);self.resign();self.assertIsNone(self.qualify())
    def test_float_zero_error_count_rejects(self):self.report['EvaluationErrors']=0.;self.resign();self.assertIsNone(self.qualify())
    def test_consistent_tiny_nonzero_native_cannot_use_exact_zero_absolute_bound(self):
        tiny=1e-25
        p=self.folder/'native.csv.gz';native=E._csv(p)
        for row in native:
            if row['h1']=='-2':row['normalized']=repr(tiny);row['raw']=repr(tiny/self.norm)
        csvfile(p,list(native[0]),native)
        p=self.folder/'native-53.csv';single=E._csv(p);single[0]['normalized']=repr(tiny);csvfile(p,list(single[0]),single)
        p=self.folder/'raw.json';raw=json.loads(p.read_text());raw['Rows'][0]['FirstFailure']['HEPCATValue']=tiny;write(p,raw)
        p=self.folder/'failed-point.json';point=json.loads(p.read_text());point['FailedRows']=raw['Rows'];write(p,point)
        p=self.folder/'double.csv.gz';rows=E._csv(p)
        for row in rows:
            value=tiny if row['H1']=='-2'else 1.;ref=float(row['MadGraph53Raw'])*self.norm;error=abs(ref-value);scale=max(abs(ref),abs(value));row.update(Native53Normalized=repr(value),AbsoluteError=repr(error),RelativeError=repr(error/scale if scale else 0.),Passed=int(error<=(1e-22 if row['H1']=='-2'else 1e-7*scale)))
        csvfile(p,list(rows[0]),rows)
        p=self.folder/'comparisons.csv.gz';rows=E._csv(p)
        with localcontext()as context:
            context.prec=80
            for row in rows:
                value=Decimal.from_float(tiny if row['H1']=='-2'else 1.);ref=Decimal(row['MadGraph113Raw'])*Decimal.from_float(self.norm);error=abs(ref-value);scale=max(abs(ref),abs(value));row.update(Native53Normalized=str(value),AbsoluteError=str(error),RelativeError=str(error/scale if scale else 0))
            self.report['Rows'][0]['MaximumAbsoluteError']=str(abs(Decimal('8e-23')*Decimal.from_float(self.norm)-Decimal.from_float(tiny)))
            self.report['Rows'][0]['MaximumRelativeError']=str(Decimal(self.report['Rows'][0]['MaximumAbsoluteError'])/(Decimal('8e-23')*Decimal.from_float(self.norm)))
        csvfile(p,list(rows[0]),rows);self.resign();self.assertIsNone(self.qualify())
    def test_original_zero_classification_cannot_be_added_by_replay(self):
        p=self.folder/'raw.json';d=json.loads(p.read_text());d['Rows'][1]['ExpectedExactZero']=True;write(p,d);self.resign();self.assertIsNone(self.qualify())


if __name__=='__main__':unittest.main()
