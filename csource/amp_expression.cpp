#include "amp_expression.hpp"
#include "hepcat.h"
#include "identities_detail.hpp"
#include "mathematica.hpp"
#include "permutation_sum.hpp"
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

// The trial borrows all IDs below originalSize from storage. Copy back only
// the new nodes reachable from the chosen alternative, preserving DAG sharing.
Id retain_borrowed(Algebra& storage, const Algebra& trial, Id answer, std::size_t originalSize) {
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
} // namespace

Id reduceInto(Algebra& storage, Id input, const std::vector<Id>& masses, const Shells& shells) {
    const auto originalSize=storage.size();
    Algebra trial(&storage);
    trial.setKinematics(masses,shells);
    const Id local=trial.simplify(input);
    return retain_borrowed(storage, trial, collectCandidate(trial, local), originalSize);
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

namespace {
Id reduce_and_retain(Algebra& a, Id root, const std::vector<Id>& masses, const Shells& shells,
                     std::optional<Id>& remainder) {
    const auto answer=reduceInto(a,root,masses,shells);
    if(!shells.empty() && root!=answer) {
        const auto off_cut=reduceInto(a,root,masses,{});
        if(off_cut!=answer) {
            std::vector<Id> terms{off_cut,a.mul({a.integer(-1),answer})};
            if(remainder)terms.push_back(*remainder);
            auto difference=reduceInto(a,a.add(std::move(terms)),masses,{});
            if(difference==a.integer(0)) remainder.reset();
            else remainder=difference;
        }
    }
    return answer;
}
}

amp_expression& amp_expression::reduce() { return reduce_with_factor_passes(3); }

amp_expression& amp_expression::reduce(generation_references preference) {
    reduce();
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    std::optional<bool> physical;
    if (preference == generation_references::physical || preference == generation_references::cyclic)
        physical = preference == generation_references::physical;
    if (physical != summand_physical_) {
        summand_physical_ = physical;
        permutations_.reset();
    }
    return *this;
}

amp_expression& amp_expression::reduce_with_factor_passes(unsigned passes) {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    if (!reduced_) {
        root_=reduce_and_retain(*arena_,root_,kinematics_->masses,kinematics_->shells,remainder_);
        if (kinematics_->shells.empty())
            root_=collect_scalar_factors(*arena_,root_,passes);
        if (passes > 1 && kinematics_->shells.empty()) {
            const auto original_size = arena_->size();
            Algebra trial(arena_.get());
            root_ = retain_borrowed(*arena_, trial, collect_simple_fractions(trial, root_), original_size);
        }
        reduced_=true;
    }
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
    output.ordered_formula_.reset();
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

Id validation_detail::import_expression(Algebra& a, const amp_expression& exp, bool remainder) {
    return exp.import_into(a, remainder);
}

std::vector<Id> amp_expression::import_masses(Algebra& destination) const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    std::vector<Id> result;
    for (Id mass : kinematics_->masses) result.push_back(copy_tree(*arena_,destination,mass));
    return result;
}

void amp_expression::set_from(const Algebra& source, Id result) {
    root_ = copy_tree(source, *arena_, result);
}

// A generated row is born in a worker arena borrowing the model. Run the
// reduction there too, then retain only the two reachable output trees.
void amp_expression::reduce_from(Algebra& worker, Id source, std::optional<Id> original, bool search) {
    auto masses=import_masses(worker);
    Shells shells;
    for(const auto& [cut,mass]:kinematics_->shells)
        shells.emplace(cut,copy_tree(*arena_,worker,mass));
    std::optional<Id> rest;
    Id result;
    if (!search) {
        // A complete graph current will be checked on every cut. Preserve the
        // exact row and its off-shell continuation without searching over bases.
        worker.setKinematics(masses, shells);
        result = worker.simplify(source);
        worker.setKinematics(masses, {});
        const Id full = worker.simplify(original.value_or(source));
        if (full != result)
            rest = worker.add({full, worker.mul({worker.integer(-1), result})});
    } else if (original && *original != source) {
        result = reduceInto(worker,source,masses,shells);
        Id off_cut = reduceInto(worker,*original,masses,{});
        Id difference = reduceInto(worker,worker.add({off_cut,worker.mul({worker.integer(-1),result})}),masses,{});
        if (difference != worker.integer(0)) rest = difference;
    } else {
        result = reduce_and_retain(worker,source,masses,shells,rest);
    }
    set_from(worker,result);
    if(rest) remainder_=copy_tree(worker,*arena_,*rest);
    reduced_=true;
}

std::string amp_expression::str() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    return arena_->format(root_);
}
struct amp_expression::cached_permutations {
    Id source;
    std::optional<permutation_sum> sum;
};

namespace {
Id reduce_kernel(Algebra& trial, Id source, const permutation_sum& projection) {
    // A local current exchange may not commute with a global Bose group.
    const bool direct_bose = projection.inner_exchanges.empty();
    auto bose = [&](Id k) {
        return direct_bose ? simplify_permutation_kernel(trial, k, projection.legs, projection.other_groups)
                           : k;
    };
    Id kernel = reduceInto(trial, bose(source), trial.masses(), {});
    kernel = collect_scalar_factors(trial, kernel, 3);
    kernel = collect_simple_fractions(trial, kernel);
    return simplify_signed_kernel(trial, bose(kernel), projection);
}
} // namespace

Id reduce_projected_candidate(Algebra& a, Id source, const permutation_sum& projection) {
    const auto initial_size = a.size();
    Algebra trial(&a);
    trial.setKinematics(a.masses(), {});
    const auto kernel = reduce_kernel(trial, source, projection);
    return retain_borrowed(a, trial, kernel, initial_size);
}

const amp_expression::cached_permutations& amp_expression::reduced_permutations() const {
    if (permutations_ && permutations_->source == root_) return *permutations_;
    Algebra trial(arena_.get());
    trial.setKinematics(kinematics_->masses, {});
    auto sum = compact_permutations(trial, root_);
    std::optional<permutation_sum> ordered;
    if (summand_physical_ || !ordered_formula_)
        ordered = recognize_ordered_current(trial,root_,summand_physical_);
    if (!ordered && ordered_formula_) {
        const auto& groups = ordered_formula_->symmetric_groups;
        ordered = permutation_sum{groups.empty() ? std::vector<int>{} : groups.front(),
                                  trial.parse(ordered_formula_->summand),ordered_formula_->orbit_sizes,
                                  {groups.empty() ? groups.end() : groups.begin()+1,groups.end()},ordered_formula_->alternating_groups,
                                  ordered_formula_->joint_exchange,ordered_formula_->inner_exchanges};
        for (const auto& term : ordered_formula_->projected_terms)
            ordered->projected_terms.push_back({trial.parse(term.summand),term.exchanges});
    }
    if (ordered) {
        Id kernel;
        if (ordered->projected_terms.empty()) kernel=reduce_kernel(trial,ordered->kernel,*ordered);
        else {
            for (auto& term : ordered->projected_terms) {
                permutation_sum local = *ordered;
                local.inner_exchanges.insert(local.inner_exchanges.begin(),term.exchanges.begin(),term.exchanges.end());
                term.kernel=reduce_kernel(trial,term.kernel,local);
            }
            kernel=sum_projected_terms(trial,ordered->projected_terms);
        }
        const Id previous = sum ? sum->kernel : root_;
        if (trial.cost(kernel) + 16 < trial.cost(root_) && trial.cost(kernel) < trial.cost(previous) &&
            (ordered_formula_ || expression_complexity(trial,kernel) <= expression_complexity(trial,previous))) {
            ordered->kernel = kernel;
            sum = std::move(ordered);
        }
    }
    if (sum) {
        const auto original_size=arena_->size();
        sum->kernel = retain_borrowed(*arena_,trial,sum->kernel,original_size);
        for (auto& term : sum->projected_terms)
            term.kernel = retain_borrowed(*arena_,trial,term.kernel,original_size);
    }
    permutations_ = std::make_shared<cached_permutations>(cached_permutations{root_,std::move(sum)});
    return *permutations_;
}

std::optional<permutation_formula> amp_expression::permutation_form() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    if (!reduced_ || !kinematics_->shells.empty()) return {};
    const auto& sum = reduced_permutations().sum;
    if (!sum) return {};
    if (sum->other_groups.empty() && sum->alternating_groups.empty() && !sum->joint_exchange && sum->inner_exchanges.empty())
        return permutation_formula{sum->legs,arena_->format(sum->kernel),sum->orbit_sizes};
    Algebra trial(arena_.get());
    trial.setKinematics(kinematics_->masses,{});
    const auto signed_kernel = expand_signed_operators(trial,*sum);
    const auto expanded = expand_permutation_groups(trial,signed_kernel,sum->other_groups);
    return permutation_formula{sum->legs,trial.format(expanded),sum->orbit_sizes};
}

std::optional<amp_expression::permutation_product> amp_expression::grouped_permutation_form() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    if (!reduced_ || !kinematics_->shells.empty()) return {};
    const auto& sum = reduced_permutations().sum;
    if (!sum) return {};
    Algebra trial(arena_.get());
    trial.setKinematics(kinematics_->masses,{});
    const Id kernel = expand_signed_operators(trial,*sum);
    permutation_product result{{},trial.format(kernel)};
    if (sum->legs.size()>1) result.groups.push_back(sum->legs);
    result.groups.insert(result.groups.end(),sum->other_groups.begin(),sum->other_groups.end());
    return result;
}

std::optional<amp_expression::signed_permutation_product> amp_expression::signed_permutation_form() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    if (!reduced_ || !kinematics_->shells.empty()) return {};
    const auto& sum = reduced_permutations().sum;
    if (!sum) return {};
    Algebra trial(arena_.get());
    trial.setKinematics(kinematics_->masses,{});
    const Id legacy=apply_exchanges(trial,sum->kernel,sum->inner_exchanges);
    signed_permutation_product result{{},sum->alternating_groups,trial.format(legacy),
                                      sum->joint_exchange,sum->orbit_sizes,sum->inner_exchanges};
    for (const auto& term : sum->projected_terms)
        result.projected_terms.push_back({arena_->format(term.kernel),term.exchanges});
    if (result.projected_terms.empty() && !sum->inner_exchanges.empty())
        result.projected_terms.push_back({arena_->format(sum->kernel),{}});
    if (sum->legs.size()>1) result.symmetric_groups.push_back(sum->legs);
    result.symmetric_groups.insert(result.symmetric_groups.end(),sum->other_groups.begin(),sum->other_groups.end());
    return result;
}

std::optional<std::size_t> amp_expression::permutation_summand_complexity() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    if (!reduced_ || !kinematics_->shells.empty()) return {};
    const auto& sum = reduced_permutations().sum;
    if (!sum) return {};
    if (sum->projected_terms.empty()) return expression_complexity(*arena_,sum->kernel);
    std::size_t total=0;
    for (const auto& term : sum->projected_terms)
        total+=expression_complexity(*arena_,term.kernel);
    return total;
}

namespace {
std::string leg_list(const std::vector<int>& legs) {
    std::string out;
    for (int i : legs) {
        if (!out.empty()) out += ",";
        out += std::to_string(i);
    }
    return out;
}
std::string cycles(const signed_exchange& exchange) {
    std::string out;
    for (auto [i,j] : exchange.swaps)
        out += "(" + std::to_string(i) + "," + std::to_string(j) + ")";
    return out;
}
}

std::string amp_expression::plain_text() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    if (reduced_ && kinematics_->shells.empty()) {
        if (const auto& sum = reduced_permutations().sum) {
            std::string groups = "(" + leg_list(sum->legs) + ")";
            for (const auto& other : sum->other_groups) groups += " and (" + leg_list(other) + ")";
            std::string operations;
            auto exchange = [&](const signed_exchange& op) {
                if (operations.empty())
                    operations = op.sign < 0 ? "Subtract from the summand its image under the simultaneous exchange "
                                             : "Add to the summand its image under the simultaneous exchange ";
                else
                    operations += op.sign < 0 ? "Then subtract its image under the simultaneous exchange "
                                              : "Then add its image under the simultaneous exchange ";
                operations += cycles(op) + ".\n";
            };
            for (const auto& inner : sum->inner_exchanges) exchange(inner);
            if (sum->joint_exchange) exchange(*sum->joint_exchange);
            for (const auto& other : sum->alternating_groups)
                operations += "Antisymmetrize in (" + leg_list(other) + "), carrying each spin with its momentum.\n";
            std::string summand;
            for (const auto& term : sum->projected_terms) {
                if (!summand.empty()) summand += "\n+ ";
                for (auto it=term.exchanges.rbegin(); it!=term.exchanges.rend(); ++it)
                    summand += std::string(it->sign < 0 ? "(1 - P" : "(1 + P") + cycles(*it) + ") ";
                summand += "[" + format_plain_text(*arena_,term.kernel) + "]";
            }
            if (sum->projected_terms.empty()) summand = format_plain_text(*arena_,sum->kernel);
            if (sum->legs.empty() && sum->other_groups.empty()) return operations + "Summand\n" + summand;
            return operations + "Sum over all permutations of external legs " + groups +
                   (operations.empty() ? " applied to\n" : ".\nSummand\n") + summand;
        }
    }
    return format_plain_text(*arena_, root_);
}
std::string amp_expression::latex() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    if (reduced_ && kinematics_->shells.empty()) {
        if (const auto& sum = reduced_permutations().sum) {
            std::string group;
            auto add_group = [&](const std::vector<int>& legs) {
                if (!group.empty()) group += "\\times ";
                group += "S_{\\{" + leg_list(legs) + "\\}}";
            };
            if (sum->legs.size()>1) add_group(sum->legs);
            for (const auto& other : sum->other_groups)
                add_group(other);
            std::string sign;
            for (const auto& other : sum->alternating_groups) {
                const auto labels = leg_list(other);
                add_group(other);
                sign += "\\operatorname{sgn}(\\pi_{\\{" + labels + "\\}})";
            }
            auto projector = [](const signed_exchange& op) {
                return std::string(op.sign < 0 ? "\\left(1-" : "\\left(1+") +
                       "\\mathsf P_{" + cycles(op) + "}\\right)";
            };
            std::string exchange, summand;
            if (sum->joint_exchange) exchange += projector(*sum->joint_exchange);
            for (auto it=sum->inner_exchanges.rbegin(); it!=sum->inner_exchanges.rend(); ++it)
                exchange += projector(*it);
            for (const auto& term : sum->projected_terms) {
                if (!summand.empty()) summand += " + ";
                for (auto it=term.exchanges.rbegin(); it!=term.exchanges.rend(); ++it)
                    summand += projector(*it);
                summand += "\\left[" + format_latex(*arena_,term.kernel) + "\\right]";
            }
            if (sum->projected_terms.empty()) summand = format_latex(*arena_,sum->kernel);
            return "\\sum_{\\pi\\in " + group + "}" + sign + "\\pi\\!" + exchange + "\\left[" +
                   summand + "\\right]";
        }
    }
    return format_latex(*arena_, root_);
}
std::string amp_expression::mathematica(bool include_mass_rules) const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    return format_mathematica(*arena_, root_, kinematics_->masses, kinematics_->shells, include_mass_rules);
}

std::vector<identity_candidate> amp_expression::identity_candidates(const identity_options& options) const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    Algebra trial(arena_.get());
    trial.setKinematics(kinematics_->masses, {});
    return identity_detail::enumerate(trial, root_, kinematics_, options);
}

Id amp_expression::apply_in_trial(Algebra& trial, const identity_candidate& edit) const {
    if (!edit.belongs_to(kinematics_, root_))
        throw std::invalid_argument("Identity edit belongs to a different expression or mass context");
    trial.setKinematics(kinematics_->masses, {});
    return identity_detail::apply(trial, root_, edit);
}

identity_score amp_expression::preview_identity(const identity_candidate& edit) const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    Algebra trial(arena_.get());
    const auto alternative = apply_in_trial(trial, edit);
    return {expression_complexity(trial, alternative), trial.cost(alternative)};
}

amp_expression amp_expression::apply_identity(const identity_candidate& edit) const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    const auto original_size = arena_->size();
    Algebra trial(arena_.get());
    const Id alternative = apply_in_trial(trial, edit);
    auto result = *this;
    result.root_ = retain_borrowed(*arena_, trial, alternative, original_size);
    result.ordered_formula_.reset();
    result.reduced_ = false;
    return result;
}
std::size_t amp_expression::leaf_count() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    return arena_->cost(root_);
}
std::size_t amp_expression::complexity() const {
    std::lock_guard<std::mutex> guard(*kinematics_->lock);
    return reported_expression_complexity(*arena_, root_);
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
