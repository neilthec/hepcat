// HEPCAT - High Energy Physics Constructive Amplitude Tool
// Copyright (C) 2026 Neil Christensen
// SPDX-License-Identifier: GPL-3.0-or-later
// Optional metadata-only wire copier. No package or Wolfram evaluation occurs.
// WXF grammar follows WolframClientForPython constants.py and wxfparser.py.
// Copies wire bytes without evaluating, simplifying or reserializing expressions.
#include <zlib.h>
#include <cmath>
#include <sys/resource.h>
#include <algorithm>
#include <array>
#include <chrono>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>
namespace fs=std::filesystem;
using Clock=std::chrono::steady_clock;
static std::string esc(const std::string&s) {
  std::string o="\"";
  for(unsigned char c:s) {
    if(c=='"'||c=='\\') {
      o+='\\';
      o+=c;
    }
    else if(c<32) {
      char b[7];
      std::snprintf(b,7,"\\u%04x",c);
      o+=b;
    }
    else o+=c;
  }
  return o+'"';
}
static uint64_t checkedMul(uint64_t a,uint64_t b) {
  if(b&&a>UINT64_MAX/b)throw std::runtime_error("array size overflow");
  return a*b;
}
static uint64_t residentBytes(uint64_t value, bool isBytes) {
  return isBytes ? value : checkedMul(value, 1024);
}
struct Limits {
  double seconds=180;
  uint64_t bytes=uint64_t(16)<<30,tokens=2000000000ULL,output=uint64_t(64)<<20;
  unsigned depth=1024;
};
struct Budget {
  Limits l;
  Clock::time_point start=Clock::now();
  uint64_t tokens=0,output=0;
  uint64_t maxRSS=0;
  void check() {
    struct rusage ru{};
    if(getrusage(RUSAGE_SELF,&ru)==0) {
      const uint64_t rss = residentBytes(uint64_t(ru.ru_maxrss),
#ifdef __APPLE__
        true
#else
        false
#endif
      );
      maxRSS=std::max<uint64_t>(maxRSS,rss);
      if(maxRSS>(uint64_t(512)<<20))throw std::runtime_error("resident memory limit");
    }
    if(std::chrono::duration<double>(Clock::now()-start).count()>l.seconds)throw std::runtime_error("time limit");
  }
  void token() {
    if(++tokens>l.tokens)throw std::runtime_error("token limit");
    if((tokens&16383)==0)check();
  }
};
struct Writer {
  FILE*f=nullptr;
  z_stream z{};
  std::array<unsigned char,65536> pending{}, out{};
  size_t used=0;
  uint64_t bytes=0;
  Budget& budget;
  fs::path part,final;
  Writer(const fs::path&p,Budget&b):budget(b),part(p.string()+".partial"),final(p) {
    if(fs::exists(part)||fs::exists(final))throw std::runtime_error("output exists");
    f=std::fopen(part.c_str(),"wbx");
    if(!f)throw std::runtime_error("cannot create output");
    std::fwrite("8C:",1,3,f);
    if(deflateInit(&z,1)!=Z_OK)throw std::runtime_error("deflate init");
  }
  void flush(int mode=Z_NO_FLUSH) {
    z.next_in=pending.data();
    z.avail_in=unsigned(used);
    int r;
    do {
      z.next_out=out.data();
      z.avail_out=out.size();
      r=deflate(&z,mode);
      if(r!=Z_OK&&r!=Z_STREAM_END)throw std::runtime_error("deflate error");
      size_t n=out.size()-z.avail_out;
      if(n&&std::fwrite(out.data(),1,n,f)!=n)throw std::runtime_error("output write");
    }
    while(z.avail_in||(!z.avail_out)||(mode==Z_FINISH&&r!=Z_STREAM_END));
    used=0;
  }
  void put(const unsigned char*p,size_t n) {
    budget.output+=n;
    bytes+=n;
    if(budget.output>budget.l.output)throw std::runtime_error("total extracted payload limit");
    while(n) {
      size_t k=std::min(n,pending.size()-used);
      std::memcpy(pending.data()+used,p,k);
      used+=k;
      p+=k;
      n-=k;
      if(used==pending.size())flush();
    }
  }
  void finish() {
    flush(Z_FINISH);
    deflateEnd(&z);
    if(std::fclose(f)!=0) {
      f=nullptr;
      throw std::runtime_error("output close");
    }
    f=nullptr;
    fs::rename(part,final);
  }
  ~Writer() {
    if(f) {
      deflateEnd(&z);
      std::fclose(f);
      std::error_code e;
      fs::remove(part,e);
    }
  }
};
struct Reader {
  FILE*f=nullptr;
  bool compressed=false,ended=false;
  z_stream z{};
  std::array<unsigned char,65536> in{}, out{};
  size_t pos=0,size=0;
  uint64_t count=0,produced=0;
  Budget& budget;
  std::vector<Writer*> sinks;
  Reader(const fs::path&p,Budget&b):budget(b) {
    f=std::fopen(p.c_str(),"rb");
    if(!f)throw std::runtime_error("input open");
    int a=std::fgetc(f),c=std::fgetc(f);
    if(a!='8')throw std::runtime_error("WXF version");
    if(c=='C') {
      compressed=true;
      c=std::fgetc(f);
    }
    if(c!=':')throw std::runtime_error("WXF header");
    if(compressed&&inflateInit(&z)!=Z_OK)throw std::runtime_error("inflate init");
  }
  bool fill() {
    budget.check();
    pos=size=0;
    if(ended)return false;
    if(!compressed) {
      size=std::fread(out.data(),1,out.size(),f);
      if(!size) {
        if(std::ferror(f))throw std::runtime_error("input read");
        ended=true;
        return false;
      }
    }
    else while(!size) {
      if(!z.avail_in) {
        size_t n=std::fread(in.data(),1,in.size(),f);
        if(!n)throw std::runtime_error("truncated zlib stream");
        z.next_in=in.data();
        z.avail_in=unsigned(n);
      }
      z.next_out=out.data();
      z.avail_out=out.size();
      int r=inflate(&z,Z_NO_FLUSH);
      size=out.size()-z.avail_out;
      if(r==Z_STREAM_END) {
        ended=true;
        if(z.avail_in||std::fgetc(f)!=EOF)throw std::runtime_error("trailing compressed bytes");
      }
      else if(r!=Z_OK)throw std::runtime_error("invalid zlib stream");
      if(ended&&!size)return false;
    }
    produced+=size;
    if(produced>budget.l.bytes)throw std::runtime_error("uncompressed input limit");
    return true;
  }
  void take(unsigned char*dst,uint64_t n) {
    while(n) {
      if(pos==size&&!fill())throw std::runtime_error("truncated WXF payload");
      size_t k=size_t(std::min<uint64_t>(n,size-pos));
      const auto*p=out.data()+pos;
      if(dst) {
        std::memcpy(dst,p,k);
        dst+=k;
      }
      for(auto*s:sinks)s->put(p,k);
      pos+=k;
      count+=k;
      n-=k;
    }
  }
  unsigned char byte() {
    unsigned char b;
    take(&b,1);
    return b;
  }
  uint64_t varint() {
    uint64_t v=0;
    for(unsigned i=0;i<9;++i) {
      unsigned char b=byte();
      if(i==8) {
        if((b&127)==0||(b&128))throw std::runtime_error("invalid ninth varint byte");
        v|=uint64_t(b)<<56;
        return v;
      }
      v|=uint64_t(b&127)<<(7*i);
      if(!(b&128))return v;
    }
    throw std::runtime_error("varint");
  }
  bool eof() {
    if(pos<size)return false;
    return !fill();
  }
  ~Reader() {
    if(compressed)inflateEnd(&z);
    if(f)std::fclose(f);
  }
};
struct Key {
  char type='?';
  std::string text;
  bool operator<(const Key&b)const {
    return type<b.type||(type==b.type&&text<b.text);
  }
  bool valid()const {
    return type!='?';
  }
};
struct Node {
  std::map<Key,std::unique_ptr<Node>> children;
  int extract=-1,index=-1;
  Node* child(const Key&k) {
    auto i=children.find(k);
    return i==children.end()?nullptr:i->second.get();
  }
};
struct Target {
  unsigned mode=0;
  bool found=false;
  uint64_t start=0,bytes=0;
  std::string kind,head;
  uint64_t length=0;
  std::vector<Key> keys;
};
static uint32_t req32(std::istream&i) {
  unsigned char b[4];
  if(!i.read((char*)b,4))throw std::runtime_error("request truncated");
  return uint32_t(b[0])|(uint32_t(b[1])<<8)|(uint32_t(b[2])<<16)|(uint32_t(b[3])<<24);
}
static std::string reqstr(std::istream&i) {
  uint32_t n=req32(i);
  if(n>1048576)throw std::runtime_error("request string limit");
  std::string s(n,'\0');
  if(!i.read(s.data(),n))throw std::runtime_error("request truncated");
  return s;
}
struct Parser {
  Budget&b;
  Reader&r;
  fs::path directory;
  Node root;
  std::vector<Target> targets;
  unsigned active=0;
  uint64_t metadataBytes=0;
  Parser(Budget&bb,Reader&rr,fs::path d,const fs::path&req):b(bb),r(rr),directory(std::move(d)) {
    if(fs::file_size(req)>1048576)throw std::runtime_error("request file limit");
    std::ifstream f(req,std::ios::binary);
    if(!f)throw std::runtime_error("requests open");
    uint32_t n=req32(f);
    if(n>512)throw std::runtime_error("request count");
    targets.resize(n);
    for(uint32_t i=0;i<n;++i) {
      int mode=f.get();
      if(mode<0||mode>1)throw std::runtime_error("request mode");
      targets[i].mode=mode;
      uint32_t depth=req32(f);
      if(depth>256)throw std::runtime_error("request depth");
      Node*p=&root;
      for(uint32_t j=0;j<depth;++j) {
        int t=f.get();
        if(t!='s'&&t!='i'&&t!='y'&&t!='a'&&t!='h')throw std::runtime_error("request key type");
        Key k {
          char(t),reqstr(f)
        };
        auto&x=p->children[k];
        if(!x)x=std::make_unique<Node>();
        p=x.get();
      }
      int&slot=mode?p->index:p->extract;
      if(slot!=-1)throw std::runtime_error("duplicate request");
      slot=i;
    }
    if(f.peek()!=EOF)throw std::runtime_error("trailing request bytes");
  }
  static std::string integer(const unsigned char*p,unsigned n) {
    uint64_t x=0;
    for(unsigned i=0;i<n;++i)x|=uint64_t(p[i])<<(8*i);
    bool negative=(p[n-1]&128)!=0;
    if(!negative)return std::to_string(x);
    if(n<8)x|=(~uint64_t(0))<<(8*n);
    return "-"+std::to_string((~x)+1);
  }
  static std::string normalizeInt(std::string s) {
    bool neg=!s.empty()&&s[0]=='-';
    size_t start=(!s.empty()&&(s[0]=='-'||s[0]=='+'))?1:0;
    while(start+1<s.size()&&s[start]=='0')++start;
    s=s.substr(start);
    return neg&&s!="0"?"-"+s:s;
  }
  static std::string shortHead(std::string s) {
    auto p=s.rfind('`');
    return p==std::string::npos?s:s.substr(p+1);
  }
  void step(unsigned depth) {
    if(depth>b.l.depth)throw std::runtime_error("depth limit");
    b.token();
  }
  Key atom(unsigned char t,bool decode) {
    if(t=='s'||t=='S'||t=='B'||t=='I'||t=='R') {
      uint64_t n=r.varint();
      if(decode&&(t=='s'||t=='S'||t=='I')&&n<=1048576) {
        std::string v(size_t(n),'\0');
        r.take((unsigned char*)v.data(),n);
        return Key{t=='s'?'y':t=='I'?'i':'s',t=='I'?normalizeInt(v):v};
      }
      r.take(nullptr,n);
      return {
      };
    }
    unsigned n=t=='C'?1:t=='j'?2:t=='i'?4:(t=='L'||t=='r')?8:0;
    if(n) {
      unsigned char v[8];
      r.take(decode?v:nullptr,n);
      if(decode&&t!='r')return Key{'i',integer(v,n)};
      return {
      };
    }
    if(t==0xc1||t==0xc2) {
      unsigned type=r.byte(),sz=0;
      switch(type) {
        case 0:case 0x10:sz=1;
        break;
        case 1:case 0x11:sz=2;
        break;
        case 2:case 0x12:case 0x22:sz=4;
        break;
        case 3:case 0x13:case 0x23:case 0x33:sz=8;
        break;
        case 0x34:sz=16;
        break;
        default:throw std::runtime_error("array type");
      }
      if(t==0xc1&&type>=0x10&&type<=0x13)throw std::runtime_error("unsigned packed array");
      uint64_t rank=r.varint(),count=1;
      if(!rank||rank>1024)throw std::runtime_error("array rank");
      for(uint64_t i=0;i<rank;++i) {
        uint64_t dim=r.varint();
        if(!dim)throw std::runtime_error("zero array dimension");
        count=checkedMul(count,dim);
      }
      r.take(nullptr,checkedMul(count,sz));
      return {
      };
    }
    throw std::runtime_error("unknown WXF token "+std::to_string(t));
  }
  void skip(unsigned depth) {
    step(depth);
    unsigned char t=r.byte();
    if(t=='f') {
      uint64_t n=r.varint();
      skip(depth+1);
      for(uint64_t i=0;i<n;++i)skip(depth+1);
    }
    else if(t=='A') {
      uint64_t n=r.varint();
      for(uint64_t i=0;i<n;++i) {
        unsigned char q=r.byte();
        b.token();
        if(q!='-'&&q!=':')throw std::runtime_error("association rule expected");
        skip(depth+1);
        skip(depth+1);
      }
    }
    else if(t=='-'||t==':')throw std::runtime_error("rule outside association");
    else atom(t,false);
  }
  Key key(unsigned depth) {
    step(depth);
    unsigned char t=r.byte();
    if(t=='f') {
      uint64_t n=r.varint();
      skip(depth+1);
      for(uint64_t i=0;i<n;++i)skip(depth+1);
      return {
      };
    }
    if(t=='A') {
      uint64_t n=r.varint();
      for(uint64_t i=0;i<n;++i) {
        unsigned char q=r.byte();
        b.token();
        if(q!='-'&&q!=':')throw std::runtime_error("association key rule expected");
        skip(depth+1);
        skip(depth+1);
      }
      return {
      };
    }
    if(t=='-'||t==':')throw std::runtime_error("rule outside association");
    return atom(t,true);
  }
  void expr(Node*node,unsigned depth) {
    if(!node) {
      skip(depth);
      return;
    }
    std::unique_ptr<Writer> writer;
    Target*idx=nullptr;
    uint64_t start=r.count;
    if(node->extract>=0) {
      auto&t=targets[node->extract];
      if(t.found)throw std::runtime_error("ambiguous duplicate association key");
      t.found=true;
      t.start=start;
      if(++active>32)throw std::runtime_error("overlapping extraction limit");
      writer=std::make_unique<Writer>(directory/("extract-"+std::to_string(node->extract)+".wxf"),b);
      r.sinks.push_back(writer.get());
    }
    if(node->index>=0) {
      idx=&targets[node->index];
      if(idx->found)throw std::runtime_error("ambiguous index path");
      idx->found=true;
      idx->start=start;
    }
    if(node->children.empty()&&!idx)skip(depth);
    else {
      step(depth);
      unsigned char t=r.byte();
      if(idx)idx->kind=t=='A'?"Association":t=='f'?"Function":"Atom";
      if(t=='A') {
        uint64_t n=r.varint();
        if(idx)idx->length=n;
        for(uint64_t i=0;i<n;++i) {
          unsigned char q=r.byte();
          b.token();
          if(q!='-'&&q!=':')throw std::runtime_error("association rule expected");
          Key k=key(depth+1);
          if(idx) {
            if(idx->keys.size()>=100000)throw std::runtime_error("index key limit");
            metadataBytes+=k.text.size()+sizeof(Key);
            if(metadataBytes>(uint64_t(16)<<20))throw std::runtime_error("index metadata limit");
            idx->keys.push_back(k);
          }
          expr(k.valid()?node->child(k):nullptr,depth+1);
        }
      }
      else if(t=='f') {
        uint64_t n=r.varint();
        Key h;
        if(Node*hn=node->child(Key{'h',""}
        )) {
          std::unique_ptr<Writer> hw;
          uint64_t hs=r.count;
          if(!hn->children.empty())throw std::runtime_error("head path must be terminal");
          if(hn->extract>=0) {
            auto&ht=targets[hn->extract];
            if(ht.found)throw std::runtime_error("duplicate head");
            ht.found=true;
            ht.start=hs;
            hw=std::make_unique<Writer>(directory/("extract-"+std::to_string(hn->extract)+".wxf"),b);
            r.sinks.push_back(hw.get());
          }
          h=key(depth+1);
          if(hw) {
            r.sinks.pop_back();
            targets[hn->extract].bytes=r.count-hs;
            hw->finish();
          }
          if(hn->index>=0) {
            auto&ht=targets[hn->index];
            ht.found=true;
            ht.start=hs;
            ht.bytes=r.count-hs;
            ht.kind="Head";
            ht.head=h.type=='y'?h.text:"";
          }
        }
        else h=key(depth+1);
        std::string name=h.type=='y'?shortHead(h.text):"";
        if(idx) {
          idx->length=n;
          idx->head=h.type=='y'?h.text:"";
          if(name=="List")idx->kind="List";
        }
        for(uint64_t i=0;i<n;++i) {
          Node*child=nullptr;
          if(name=="List")child=node->child(Key{'i',std::to_string(i)}
          );
          else if(name=="Failure"||name=="Missing") {
            if(i==0)child=node->child(Key{'s',"Tag"}
            );
            else if(Node*p=node->child(Key{'s',name}
            ))child=p->child(Key{'i',std::to_string(i)}
            );
          }
          else child=node->child(Key{'a',std::to_string(i)}
          );
          expr(child,depth+1);
        }
      }
      else if(t=='-'||t==':')throw std::runtime_error("rule outside association");
      else atom(t,false);
    }
    if(idx)idx->bytes=r.count-start;
    if(writer) {
      r.sinks.pop_back();
      auto&t=targets[node->extract];
      t.bytes=r.count-start;
      writer->finish();
      --active;
    }
  }
  void run() {
    expr(&root,0);
    if(!r.eof())throw std::runtime_error("trailing uncompressed expression");
  }
  void report() {
    b.check();
    std::cout << "{\"CompleteParse\":true,\"Tokens\":" << b.tokens
      << ",\"UncompressedBytes\":" << r.count
      << ",\"ExtractedPayloadBytes\":" << b.output
      << ",\"MaximumResidentBytes\":" << b.maxRSS
      << ",\"Seconds\":" << std::chrono::duration<double>(Clock::now()-b.start).count()
      << ",\"Targets\":[";
    for(size_t i=0;i<targets.size();++i) {
      if(i)std::cout<<',';
      auto&t=targets[i];
      std::cout<<"{\"Index\":"<<i<<",\"Mode\":"<<t.mode<<",\"Found\":"<<(t.found?"true":"false")<<",\"Start\":"<<t.start<<",\"PayloadBytes\":"<<t.bytes;
      if(t.mode&&t.found) {
        std::cout<<",\"Kind\":"<<esc(t.kind)<<",\"Head\":"<<esc(t.head)<<",\"Length\":"<<t.length<<",\"Keys\":[";
        for(size_t j=0;j<t.keys.size();++j) {
          if(j)std::cout<<',';
          std::cout<<"{\"Type\":"<<esc(std::string(1,t.keys[j].type))<<",\"Value\":"<<esc(t.keys[j].text)<<'}';
        }
        std::cout<<']';
      }
      std::cout<<'}';
    }
    std::cout<<"]}\n";
  }
};
int main(int argc,char**argv) {
  try {
    if(argc!=9)throw std::runtime_error("usage: extractor input request outputdir seconds maxbytes maxtokens maxdepth maxoutput");
    Budget b;
    b.l.seconds=std::stod(argv[4]);
    b.l.bytes=std::stoull(argv[5]);
    b.l.tokens=std::stoull(argv[6]);
    b.l.depth=unsigned(std::stoul(argv[7]));
    b.l.output=std::stoull(argv[8]);
    if(!std::isfinite(b.l.seconds) || b.l.seconds<=0 || !b.l.bytes ||
       !b.l.tokens || !b.l.depth || !b.l.output) {
      throw std::runtime_error("positive finite bounds required");
    }
    Reader r(argv[1],b);
    Parser p(b,r,argv[3],argv[2]);
    p.run();
    p.report();
    return 0;
  }
  catch(const std::exception&e) {
    std::cerr<<e.what()<<'\n';
    return 1;
  }
}
