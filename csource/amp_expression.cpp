#include "amp_expression.hpp"
#include "hepcat.h"
#include "reduce.hpp"

#include <algorithm>
#include <climits>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <new>
#include <mutex>
#include <stdexcept>
#include <unordered_map>

namespace hepcat {
namespace {
using Shells = std::map<std::vector<int>,Id>;

void insertShell(Shells& shells, std::vector<int> legs, Id mass, const std::vector<Id>& masses) {
    std::sort(legs.begin(),legs.end());
    if (legs.size()<2 || legs.size()>=masses.size() || legs.front()<1 ||
        size_t(legs.back())>masses.size() ||
        std::adjacent_find(legs.begin(),legs.end())!=legs.end())
        throw std::invalid_argument("invalid numerator shell subset");
    std::vector<int> other;
    for (size_t i=1;i<=masses.size();++i)
        if (!std::binary_search(legs.begin(),legs.end(),static_cast<int>(i)))
            other.push_back(static_cast<int>(i));
    if (other.size()==1 && mass!=masses[other.front()-1])
        throw std::invalid_argument("shell conflicts with the external mass on its complement");
    for (const auto* cut : {&legs,&other}) {
        auto it=shells.find(*cut);
        if (it!=shells.end() && it->second!=mass)
            throw std::invalid_argument("conflicting masses for the same or complementary cut");
    }
    shells.emplace(std::move(legs),mass);
}
}

Id reduceInto(Algebra& storage, Id input, const std::vector<Id>& masses, const Shells& shells) {
    // Borrow the imported tree. Trial expressions live only in this call.
    // Copy back just the new nodes reachable from the accepted result.
    const auto originalSize=storage.size();
    Algebra trial(&storage);
    trial.setKinematics(masses,shells);
    const Id local=trial.simplify(input);
    const Id answer=collectCandidate(trial,local);
    std::unordered_map<Id,Id> saved;
    std::function<Id(Id)> retain = [&](Id id) -> Id {
        if (id<originalSize) return id;
        if (auto found=saved.find(id); found!=saved.end()) return found->second;
        const auto& n=trial.node(id);
        auto args=n.args;
        for (auto& child : args) child=retain(child);
        auto out=storage.raw(n.head,std::move(args),n.compound);
        saved[id]=out;
        return out;
    };
    return retain(answer);
}

struct amp_expression::context {
    std::vector<Id> masses;
    Shells shells;
    std::shared_ptr<std::mutex> lock=std::make_shared<std::mutex>();
};

amp_expression::amp_expression(const std::string& expr,const std::vector<std::string>& masses,
                               const std::vector<numerator_shell>& shells)
    : arena_(std::make_shared<Algebra>()) {
    if (masses.empty() || masses.size()>INT_MAX) throw std::invalid_argument("invalid external mass count");
    auto ctx=std::make_shared<context>();
    for (const auto& mass : masses) ctx->masses.push_back(arena_->parse(mass));
    for (const auto& shell : shells)
        insertShell(ctx->shells,shell.legs,arena_->parse(shell.mass),ctx->masses);
    root_=arena_->parse(expr);
    kinematics_=std::move(ctx);
}

amp_expression& amp_expression::reduce() {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    if (reduced_) return *this;
    const auto answer=reduceInto(*arena_,root_,kinematics_->masses,kinematics_->shells);
    if (!kinematics_->shells.empty() && root_!=answer) {
        // Comparing two continuations preserves cross terms when several
        // lines are on shell at once. Re-reducing the amplitude adds only
        // the new change to the remainder already retained.
        const auto offCut=reduceInto(*arena_,root_,kinematics_->masses,{});
        if (offCut!=answer) {
            std::vector<Id> terms{offCut,arena_->mul({arena_->integer(-1),answer})};
            if (remainder_) terms.push_back(*remainder_);
            auto difference=reduceInto(*arena_,arena_->add(std::move(terms)),kinematics_->masses,{});
            if (difference==arena_->integer(0)) remainder_.reset();
            else remainder_=difference;
        }
    }
    root_=answer;
    reduced_=true;
    return *this;
}

std::optional<amp_expression> amp_expression::discarded_remainder() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    if (!remainder_) return std::nullopt;
    auto output=*this;
    auto external=std::make_shared<context>(*kinematics_);
    external->shells.clear();
    output.kinematics_=std::move(external);
    output.root_=*remainder_;
    output.reduced_=false;
    output.remainder_.reset();
    return output;
}

namespace {
Id copy_tree(const Algebra& from, Algebra& to, Id root) {
    std::unordered_map<Id,Id> copied;
    std::function<Id(Id)> visit = [&](Id id) -> Id {
        if (auto it = copied.find(id); it != copied.end()) return it->second;
        const auto& n = from.node(id);
        auto args = n.args;
        for (Id& child : args) child = visit(child);
        Id result = to.raw(n.head, std::move(args), n.compound);
        copied.emplace(id, result);
        return result;
    };
    return visit(root);
}
}

void amp_expression::restore_prepared(const std::string& remainder) {
    Id rest = arena_->parse(remainder);
    if (rest != arena_->integer(0)) remainder_ = rest;
    reduced_ = true;
}

Id amp_expression::import_into(Algebra& destination, bool remainder) const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    if (remainder && !remainder_) return destination.integer(0);
    return copy_tree(*arena_, destination, remainder ? *remainder_ : root_);
}

void amp_expression::set_from(const Algebra& source, Id result) {
    root_ = copy_tree(source, *arena_, result);
}

std::string amp_expression::str() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    return arena_->format(root_);
}
std::size_t amp_expression::particle_count() const { return kinematics_->masses.size(); }

} // namespace hepcat

struct hepcat_amp { hepcat::amp_expression value; };
namespace {
thread_local std::string last_error;

char* copyString(const std::string& value) {
    auto* copy=static_cast<char*>(std::malloc(value.size()+1));
    if (!copy) throw std::bad_alloc();
    std::memcpy(copy,value.c_str(),value.size()+1);
    return copy;
}
}

extern "C" {
hepcat_amp* hepcat_create(const char* expression,size_t count,const char* const* masses,
                          const hepcat_shell* shells,size_t shellCount) {
    try {
        last_error.clear();
        if (!expression || !count || count>INT_MAX || !masses || (shellCount && !shells))
            throw std::invalid_argument("invalid amplitude arguments");
        std::vector<std::string> external;
        for (size_t i=0;i<count;++i) {
            if (!masses[i]) throw std::invalid_argument("missing external mass");
            external.emplace_back(masses[i]);
        }
        std::vector<hepcat::numerator_shell> cuts;
        for (size_t i=0;i<shellCount;++i) {
            const auto& shell=shells[i];
            if (!shell.legs || !shell.mass || shell.leg_count>=count)
                throw std::invalid_argument("invalid shell arguments");
            cuts.push_back({{shell.legs,shell.legs+shell.leg_count},shell.mass});
        }
        return new hepcat_amp{hepcat::amp_expression(expression,external,cuts)};
    } catch (const std::exception& e) { last_error=e.what(); return nullptr; }
}
void hepcat_destroy(hepcat_amp* a) { delete a; }
const char* hepcat_error(void) { return last_error.c_str(); }
void hepcat_free_string(char* s) { std::free(s); }
int reduce(hepcat_amp* a) {
    try {
        last_error.clear();
        if (!a) throw std::invalid_argument("missing amplitude");
        a->value.reduce();
        return 1;
    } catch (const std::exception& e) { last_error=e.what(); return 0; }
}
char* hepcat_format(const hepcat_amp* a) {
    try {
        last_error.clear();
        if (!a) throw std::invalid_argument("missing amplitude");
        return copyString(a->value.str());
    } catch (const std::exception& e) { last_error=e.what(); return nullptr; }
}
char* hepcat_format_remainder(const hepcat_amp* a) {
    try {
        last_error.clear();
        if (!a) throw std::invalid_argument("missing amplitude");
        auto tail=a->value.discarded_remainder();
        return tail ? copyString(tail->str()) : nullptr;
    } catch (const std::exception& e) { last_error=e.what(); return nullptr; }
}
}
