// Evaluate supplied polynomials, including allowed negative variable powers.
// Wolfram first certifies the external kinematics, identities, spin components
// and denominator clearing. The Python tools verify any factor rearrangement.
// This evaluator then combines every coefficient using Boost arbitrary-size
// integers and exact rational arithmetic, without floating-point sampling.
// A root is ZERO when all coefficients cancel and NONZERO when some remain.
// Unsupported input or a resource limit returns NOT_PROVED.
// Expressions come entirely from the input graph; no amplitude table is read.
/*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*/

#include <boost/multiprecision/cpp_int.hpp>
#include <algorithm>
#include <array>
#include <chrono>
#include <cstdint>
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>
#if defined(__APPLE__) || defined(__linux__)
#include <unistd.h>
#endif
#ifdef __APPLE__
#include <mach/mach.h>
#endif

using Integer = boost::multiprecision::cpp_int;
using Rational = boost::multiprecision::cpp_rational;
using Exponents = std::array<std::int32_t, 16>;
struct NotProved : std::runtime_error { using std::runtime_error::runtime_error; };
struct Limits {
  std::size_t nodes = 250000, terms = 200000, liveTerms = 300000;
  std::uint64_t products = 20000000;
  unsigned bits = 4096;
  std::uint64_t rssBytes = 1073741824ULL;
  double seconds = 60;
};
struct State {
  Limits cap;
  std::chrono::steady_clock::time_point started = std::chrono::steady_clock::now();
  std::size_t live = 0, peakLive = 0, peakTerms = 0, evaluated = 0;
  std::uint64_t products = 0, ticks = 0;
  std::uint64_t cacheHits = 0;
  std::uint64_t peakRSS = 0;
#if defined(__APPLE__) || defined(__linux__)
  std::int64_t expectedParent = static_cast<std::int64_t>(getppid());
#else
  std::int64_t expectedParent = 0;
#endif
  double elapsed() const { return std::chrono::duration<double>(std::chrono::steady_clock::now()-started).count(); }
  void time(bool force = false) {
    if (force || (++ticks & 1023) == 0) {
#if defined(__APPLE__) || defined(__linux__)
      if(static_cast<std::int64_t>(getppid())!=expectedParent) throw NotProved("ParentExited");
#endif
      if(elapsed() > cap.seconds) throw NotProved("TimeLimit");
#ifdef __APPLE__
      mach_task_basic_info_data_t info{}; mach_msg_type_number_t count=MACH_TASK_BASIC_INFO_COUNT;
      if(task_info(mach_task_self(),MACH_TASK_BASIC_INFO,reinterpret_cast<task_info_t>(&info),&count)!=KERN_SUCCESS)
        throw NotProved("RSSCheckUnavailable");
      peakRSS=std::max(peakRSS,static_cast<std::uint64_t>(info.resident_size));
      if(info.resident_size>cap.rssBytes) throw NotProved("RSSLimit");
#elif defined(__linux__)
      std::ifstream status("/proc/self/statm"); std::uint64_t virtualPages=0,residentPages=0;
      const auto pageSize=sysconf(_SC_PAGESIZE);
      if(!(status>>virtualPages>>residentPages) || pageSize<=0) throw NotProved("RSSCheckUnavailable");
      const auto bytes=residentPages*static_cast<std::uint64_t>(pageSize);
      peakRSS=std::max(peakRSS,bytes);
      if(bytes>cap.rssBytes) throw NotProved("RSSLimit");
#else
      throw NotProved("RSSCheckUnavailable");
#endif
    }
  }
  void coefficient(const Rational& value) {
    auto n = numerator(value); auto d = denominator(value);
    if (n < 0) n = -n;
    if ((n != 0 && boost::multiprecision::msb(n) >= cap.bits) ||
        (d != 0 && boost::multiprecision::msb(d) >= cap.bits)) throw NotProved("CoefficientBitLimit");
  }
};
struct Hash {
  std::size_t operator()(const Exponents& key) const noexcept {
    std::size_t result = 1469598103934665603ULL;
    for (auto item : key) { result ^= static_cast<std::uint32_t>(item); result *= 1099511628211ULL; }
    return result;
  }
};
struct Polynomial {
  State* state;
  std::unordered_map<Exponents, Rational, Hash> terms;
  explicit Polynomial(State& s) : state(&s) {}
  Polynomial(const Polynomial&) = delete;
  Polynomial& operator=(const Polynomial&) = delete;
  ~Polynomial() { state->live -= terms.size(); }
  void add(const Exponents& powers, const Rational& coefficient) {
    state->time(); if (coefficient == 0) return;
    auto found = terms.find(powers);
    if (found == terms.end()) {
      state->coefficient(coefficient);
      if (terms.size() >= state->cap.terms) throw NotProved("NodeTermLimit");
      if (state->live >= state->cap.liveTerms) throw NotProved("LiveTermLimit");
      terms.emplace(powers, coefficient); ++state->live;
      state->peakLive = std::max(state->peakLive, state->live);
      state->peakTerms = std::max(state->peakTerms, terms.size());
    } else {
      Rational sum = found->second + coefficient;
      state->coefficient(sum);
      if (sum == 0) { terms.erase(found); --state->live; }
      else found->second = std::move(sum);
    }
  }
};
using Poly = std::unique_ptr<Polynomial>;
static Exponents shifted(const Exponents& a, const Exponents& b) {
  Exponents result{};
  for (unsigned i=0; i<16; ++i) {
    auto value = static_cast<std::int64_t>(a[i]) + b[i];
    if (value < std::numeric_limits<std::int32_t>::min() || value > std::numeric_limits<std::int32_t>::max())
      throw NotProved("ExponentOverflow");
    result[i] = static_cast<std::int32_t>(value);
  }
  return result;
}
static Poly constant(State& state, const Rational& value) {
  auto result = std::make_unique<Polynomial>(state); result->add(Exponents{}, value); return result;
}
static Poly multiply(const Polynomial& left, const Polynomial& right) {
  auto& state = *left.state; auto result = std::make_unique<Polynomial>(state);
  if (left.terms.empty() || right.terms.empty()) return result;
  if (left.terms.size() > (state.cap.products-state.products)/right.terms.size()) throw NotProved("ProductLimit");
  state.products += left.terms.size()*right.terms.size();
  for (const auto& a : left.terms) for (const auto& b : right.terms)
    result->add(shifted(a.first,b.first), a.second*b.second);
  return result;
}
static Rational rationalPower(State& state, Rational value, std::uint64_t exponent) {
  Rational result = 1;
  while (exponent) {
    state.time(true);
    if (exponent & 1) { result *= value; state.coefficient(result); }
    exponent >>= 1;
    if (exponent) { value *= value; state.coefficient(value); }
  }
  return result;
}
static Poly power(const Polynomial& base, std::int64_t exponent) {
  auto& state = *base.state;
  if (exponent == 0) return constant(state,1);
  if (exponent < 0) {
    if (base.terms.size() != 1) throw NotProved("InverseNotNonzeroMonomial");
    const auto& item = *base.terms.begin(); Exponents powers{};
    for (unsigned i=0;i<16;++i) {
      auto value = static_cast<std::int64_t>(item.first[i])*exponent;
      if (value < std::numeric_limits<std::int32_t>::min() || value > std::numeric_limits<std::int32_t>::max())
        throw NotProved("ExponentOverflow");
      powers[i] = static_cast<std::int32_t>(value);
    }
    auto result=std::make_unique<Polynomial>(state);
    result->add(powers,rationalPower(state,Rational(1)/item.second,static_cast<std::uint64_t>(-exponent)));
    return result;
  }
  auto result=constant(state,1); auto factor=std::make_unique<Polynomial>(state);
  for(const auto& item:base.terms) factor->add(item.first,item.second);
  auto n=static_cast<std::uint64_t>(exponent);
  while(n) {
    state.time(true); if(n&1) result=multiply(*result,*factor);
    n>>=1; if(n) factor=multiply(*factor,*factor);
  }
  return result;
}
static std::int64_t smallInteger(const std::string& token) {
  if(token.empty()) throw NotProved("InvalidInteger");
  std::size_t used=0; std::int64_t result;
  try { result=std::stoll(token,&used,10); } catch(...) { throw NotProved("IntegerRange"); }
  if(used!=token.size()) throw NotProved("InvalidInteger"); return result;
}
static Integer bigInteger(const std::string& token, State& state) {
  if(token.empty() || token.size()>20000) throw NotProved("IntegerSizeLimit");
  std::size_t i=0; bool negative=false;
  if(token[i]=='-' || token[i]=='+') { negative=token[i]=='-'; ++i; }
  if(i==token.size()) throw NotProved("InvalidInteger");
  Integer result=0;
  for(;i<token.size();++i) {
    if(token[i]<'0'||token[i]>'9') throw NotProved("InvalidInteger");
    result*=10; result+=token[i]-'0'; state.time();
    if(result!=0 && boost::multiprecision::msb(result)>=state.cap.bits) throw NotProved("CoefficientBitLimit");
  }
  return negative ? -result : result;
}
struct Node { char kind=0; Rational value=0; unsigned variable=0; std::int64_t exponent=0; std::vector<std::size_t> children; };
struct Input { unsigned variables=0; std::vector<Node> nodes{Node{}}; std::vector<std::size_t> roots; };
static Input parse(std::istream& stream, State& state) {
  Input input; std::string line, word; std::size_t edges=0; bool header=false, ended=false;
  while(std::getline(stream,line)) {
    state.time(true);
    if(line.size()>16000000) throw NotProved("LineSizeLimit");
    std::istringstream row(line); if(!(row>>word) || word[0]=='#') continue;
    if(ended) throw NotProved("DataAfterEnd");
    if(!header) {
      std::string version,extra;
      if(word!="HEPCAT_LAURENT_DAG" || !(row>>version) || version!="1" || row>>extra) throw NotProved("InvalidHeader");
      header=true; continue;
    }
    auto token=[&]() { std::string s; if(!(row>>s)) throw NotProved("MissingToken"); return s; };
    if(word=="Variables") {
      if(input.variables || input.nodes.size()!=1) throw NotProved("DuplicateVariables");
      auto n=smallInteger(token()); if(n<1||n>16) throw NotProved("VariableLimit"); input.variables=static_cast<unsigned>(n);
    } else if(word=="Root") {
      auto id=smallInteger(token()); if(id<1||static_cast<std::size_t>(id)>=input.nodes.size()) throw NotProved("UnknownRoot");
      if(input.roots.size()>=4096) throw NotProved("RootLimit"); input.roots.push_back(static_cast<std::size_t>(id));
    } else if(word=="End") ended=true;
    else {
      if(!input.variables || !input.roots.empty()) throw NotProved("NodeAfterRootOrBeforeVariables");
      auto id=smallInteger(token()); if(id<1||static_cast<std::size_t>(id)!=input.nodes.size()) throw NotProved("NonSequentialNode");
      if(input.nodes.size()>state.cap.nodes) throw NotProved("NodeLimit"); Node node;
      auto child=[&]() { auto c=smallInteger(token()); if(c<1||c>=id) throw NotProved("NonTopologicalChild"); if(++edges>2000000) throw NotProved("EdgeLimit"); node.children.push_back(static_cast<std::size_t>(c)); };
      if(word=="Constant") {
        node.kind='C'; Integer n=bigInteger(token(),state),d=bigInteger(token(),state);
        if(d==0) throw NotProved("ZeroDenominator"); node.value=Rational(n)/d; state.coefficient(node.value);
      } else if(word=="Variable") {
        node.kind='V'; auto v=smallInteger(token()); if(v<1||v>input.variables) throw NotProved("VariableIndex"); node.variable=static_cast<unsigned>(v-1);
      } else if(word=="Add"||word=="Multiply") {
        node.kind=word=="Add"?'A':'M'; auto count=smallInteger(token());
        if(count<0||count>250000) throw NotProved("ChildLimit"); for(std::int64_t i=0;i<count;++i) child();
      } else if(word=="Power") {
        node.kind='P'; child(); node.exponent=smallInteger(token());
        if(node.exponent < -1000000 || node.exponent > 1000000) throw NotProved("PowerLimit");
      } else throw NotProved("UnknownNodeType");
      input.nodes.push_back(std::move(node));
    }
    std::string extra; if(row>>extra) throw NotProved("ExtraToken");
  }
  if(!header||!ended||!input.variables||input.roots.empty()) throw NotProved("IncompleteInput"); return input;
}
int main(int argc,char** argv) {
  State state; std::vector<std::size_t> rootIds; std::vector<std::string> answers; std::vector<std::size_t> rootTerms;
  std::size_t currentNode=0,currentChildren=0; char currentKind='?';
  try {
    if(argc<2) throw NotProved("Usage_laurent_proof_INPUT_options");
    for(int i=2;i<argc;i+=2) {
      if(i+1>=argc) throw NotProved("MissingOptionValue"); std::string key=argv[i],value=argv[i+1];
      if(key=="--seconds") {
        std::size_t used=0; try { state.cap.seconds=std::stod(value,&used); } catch(...) { throw NotProved("InvalidSeconds"); }
        if(used!=value.size() || !(state.cap.seconds>0 && state.cap.seconds<=3600)) throw NotProved("InvalidSeconds");
      } else {
        auto count=smallInteger(value); if(count<1) throw NotProved("InvalidCap");
        if(key=="--max-terms" && count<=2000000) state.cap.terms=static_cast<std::size_t>(count);
        else if(key=="--max-live-terms" && count<=8000000) state.cap.liveTerms=static_cast<std::size_t>(count);
        else if(key=="--max-products" && count<=1000000000) state.cap.products=static_cast<std::uint64_t>(count);
        else if(key=="--max-nodes" && count<=500000) state.cap.nodes=static_cast<std::size_t>(count);
        else if(key=="--max-bits" && count<=65536) state.cap.bits=static_cast<unsigned>(count);
        else if(key=="--max-rss-mib" && count<=2048) state.cap.rssBytes=static_cast<std::uint64_t>(count)*1048576ULL;
        else if(key=="--parent-pid" && count<=std::numeric_limits<std::int32_t>::max()) state.expectedParent=count;
        else throw NotProved("UnknownOptionOrCapTooLarge");
      }
    }
    std::ifstream stream(argv[1]); if(!stream) throw NotProved("CannotOpenInput");
    Input input=parse(stream,state); rootIds=input.roots;
    answers.assign(rootIds.size(),"NOT_PROVED"); rootTerms.assign(rootIds.size(),0);
    using SharedPoly=std::shared_ptr<Polynomial>;
    std::vector<SharedPoly> cache(input.nodes.size()); std::size_t cachedTerms=0;
    constexpr std::size_t cacheNodeTerms=5000,cacheTotalTerms=50000;
    std::function<SharedPoly(std::size_t,unsigned)> evaluate;
    evaluate=[&](std::size_t i,unsigned depth)->SharedPoly {
      const auto& node=input.nodes[i];
      auto context=[&]() { currentNode=i; currentKind=node.kind; currentChildren=node.children.size(); };
      context(); state.time(true);
      if(depth>2048) throw NotProved("EvaluationDepthLimit");
      if(cache[i]) { ++state.cacheHits; return cache[i]; }
      if(++state.evaluated>2000000) throw NotProved("EvaluationCountLimit");
      Poly result;
      if(node.kind=='C') result=constant(state,node.value);
      else if(node.kind=='V') { result=constant(state,0); Exponents key{}; key[node.variable]=1; result->add(key,1); }
      else if(node.kind=='A') {
        result=constant(state,0);
        for(auto child:node.children) {
          auto value=evaluate(child,depth+1); context();
          for(const auto& item:value->terms) result->add(item.first,item.second);
        }
      } else if(node.kind=='M') {
        result=constant(state,1);
        // New grouped cofactors have later IDs than their literal factors.
        // Evaluate them first, while still visiting every child even after a
        // zero result, so unsupported inverse subtrees cannot be skipped.
        for(auto child=node.children.rbegin();child!=node.children.rend();++child) {
          auto value=evaluate(*child,depth+1); context(); result=multiply(*result,*value);
        }
      } else if(node.kind=='P') {
        auto value=evaluate(node.children[0],depth+1); context(); result=power(*value,node.exponent);
      } else throw NotProved("InvalidNode");
      SharedPoly answer(std::move(result));
      if(answer->terms.size()<=cacheNodeTerms) {
        if(cachedTerms+answer->terms.size()>cacheTotalTerms) { for(auto& value:cache) value.reset(); cachedTerms=0; }
        cache[i]=answer; cachedTerms+=answer->terms.size();
      }
      return answer;
    };
    for(std::size_t r=0;r<rootIds.size();++r) {
      auto result=evaluate(rootIds[r],0); answers[r]=result->terms.empty()?"ZERO":"NONZERO"; rootTerms[r]=result->terms.size();
      std::cout<<"CHECKPOINT_ROOT "<<r+1<<" NODE "<<rootIds[r]<<" "<<answers[r]<<" TERMS "<<rootTerms[r]<<" SECONDS "<<state.elapsed()<<std::endl;
    }
    std::cout<<"STATUS COMPLETE\nVARIABLES "<<input.variables<<"\nNODES "<<input.nodes.size()-1<<"\n";
    for(std::size_t i=0;i<rootIds.size();++i) std::cout<<"ROOT "<<i+1<<" NODE "<<rootIds[i]<<" "<<answers[i]<<" TERMS "<<rootTerms[i]<<"\n";
    std::cout<<"ALL_ROOTS_ZERO "<<(std::all_of(answers.begin(),answers.end(),[](const auto& s){return s=="ZERO";})?"TRUE":"FALSE")<<"\n";
  } catch(const NotProved& error) {
    std::cout<<"STATUS NOT_PROVED\nREASON "<<error.what()<<"\n";
    std::cout<<"CURRENT_NODE "<<currentNode<<" KIND "<<currentKind<<" CHILDREN "<<currentChildren<<"\n";
    for(std::size_t i=0;i<rootIds.size();++i) std::cout<<"ROOT "<<i+1<<" NODE "<<rootIds[i]<<" "<<answers[i]<<" TERMS "<<rootTerms[i]<<"\n";
    for(std::size_t i=0;i<rootIds.size();++i) if(answers[i]=="NOT_PROVED") { std::cout<<"NEXT_ROOT "<<i+1<<" NODE "<<rootIds[i]<<"\n"; break; }
    std::cout<<"EVALUATED_NODES "<<state.evaluated<<"\nCACHE_HITS "<<state.cacheHits<<"\nPEAK_LIVE_TERMS "<<state.peakLive<<"\nPEAK_NODE_TERMS "<<state.peakTerms<<"\nPRODUCTS "<<state.products<<"\nPEAK_SAMPLED_RSS_BYTES "<<state.peakRSS<<"\nSECONDS "<<state.elapsed()<<"\n"; return 2;
  } catch(const std::exception& error) {
    std::cout<<"STATUS NOT_PROVED\nREASON NativeException\nDETAIL "<<error.what()<<"\n";
    std::cout<<"CURRENT_NODE "<<currentNode<<" KIND "<<currentKind<<" CHILDREN "<<currentChildren<<"\n";
    for(std::size_t i=0;i<rootIds.size();++i) std::cout<<"ROOT "<<i+1<<" NODE "<<rootIds[i]<<" "<<answers[i]<<" TERMS "<<rootTerms[i]<<"\n";
    return 2;
  }
  std::cout<<"EVALUATED_NODES "<<state.evaluated<<"\nCACHE_HITS "<<state.cacheHits<<"\nPEAK_LIVE_TERMS "<<state.peakLive<<"\nPEAK_NODE_TERMS "<<state.peakTerms<<"\nPRODUCTS "<<state.products<<"\nPEAK_SAMPLED_RSS_BYTES "<<state.peakRSS<<"\nSECONDS "<<state.elapsed()<<"\n"; return 0;
}
