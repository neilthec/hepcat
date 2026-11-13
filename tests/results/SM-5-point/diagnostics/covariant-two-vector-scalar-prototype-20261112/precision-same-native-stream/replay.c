
#include <dlfcn.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "num_out.h"
int main(int argc,char**argv){
 if(argc!=7)return 1;
 void*handle=dlopen(argv[1],RTLD_NOW|RTLD_LOCAL);if(!handle){fprintf(stderr,"%s\n",dlerror());return 2;}
 CalcHEP_interface**exported=dlsym(handle,"PtrInterface_hepcat_precision");if(!exported||!*exported)return 3;
 CalcHEP_interface*interface=*exported;const int legs=atoi(argv[6]);
 if(interface->nin!=2||interface->nout+2!=legs||interface->nprc!=1)return 4;
 FILE*parameters=fopen(argv[3],"r");if(!parameters)return 5;char line[16384];fgets(line,sizeof line,parameters);
 int assigned[1024]={0};if(interface->nvar>=1024)return 6;
 while(fgets(line,sizeof line,parameters)){
  char*comma=strchr(line,',');if(!comma)return 7;*comma++=0;
  int index=0;for(int i=1;i<=interface->nvar;++i)if(!strcmp(line,interface->varName[i]))index=i;
  if(!index)return 8;
#ifdef _QUADGCC_
  interface->va[index]=strtoflt128(comma,NULL);
#elif defined(_LONG_)
  interface->va[index]=strtold(comma,NULL);
#else
  interface->va[index]=strtod(comma,NULL);
#endif
  assigned[index]=1;
 }
 fclose(parameters);for(int i=1;i<=interface->nvar;++i)if(!assigned[i])return 9;
 *interface->twidth=0;*interface->gtwidth=0;*interface->gswidth=0;if(interface->calcFunc())return 10;
 /* CalcHEP may sort external states. Recover the physical-to-reference map by PDG. */
 int pdgs[20]={11,-11,23,23,25},order[20],used[20]={0};
 for(int i=0;i<legs;i++){int pdg=0;interface->pinf(1,i+1,NULL,&pdg);order[i]=-1;
  for(int j=0;j<legs;j++)if(!used[j]&&pdgs[j]==pdg){order[i]=j;used[j]=1;break;}if(order[i]<0)return 11;}
 FILE*input=fopen(argv[2],"r"),*output=fopen(argv[4],"w");if(!input||!output)return 12;
 fprintf(output,"point,hepcat,original_calchep,replayed_calchep,relative_error,absolute_error\n");fgets(line,sizeof line,input);
 int points=0,mismatches=0,errors=0,matches=0;double maxrelative=0,maxabsolute=0,tolerance=strtod(argv[5],NULL);
 while(fgets(line,sizeof line,input)){
  double row[83];char*cursor=line;for(int i=0;i<3+4*legs;i++){char*end;row[i]=strtod(cursor,&end);if(end==cursor)return 13;cursor=*end==','?end+1:end;}
  REAL momenta[80];for(int leg=0;leg<legs;leg++)for(int j=0;j<4;j++)momenta[4*leg+j]=(REAL)row[3+4*order[leg]+j];
  int error=0;double value=interface->sqme(1,1.0,momenta,NULL,&error);points++;
  if(error||!isfinite(value)||value<0)errors++;if(value==row[2])matches++;
  double absolute=fabs(value-row[1]),scale=fmax(fabs(value),fabs(row[1])),relative=scale?absolute/scale:0;
  if(relative>tolerance)mismatches++;if(relative>maxrelative)maxrelative=relative;if(absolute>maxabsolute)maxabsolute=absolute;
  fprintf(output,"%.0f,%.17g,%.17g,%.17g,%.17g,%.17g\n",row[0],row[1],row[2],value,relative,absolute);
 }
 fclose(input);fclose(output);
#ifdef _QUADGCC_
 const int bits=FLT128_MANT_DIG;
#elif defined(_LONG_)
 const int bits=LDBL_MANT_DIG;
#else
 const int bits=DBL_MANT_DIG;
#endif
 printf("{\"MantissaBits\":%d,\"REALBytes\":%zu,\"Points\":%d,\"Mismatches\":%d,\"Errors\":%d,\"OriginalBitMatches\":%d,\"MaximumRelativeError\":%.17g,\"MaximumAbsoluteError\":%.17g}\n",bits,sizeof(REAL),points,mismatches,errors,matches,maxrelative,maxabsolute);
 dlclose(handle);return errors?14:0;
}
