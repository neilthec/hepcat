#include "reduce.hpp"
#include <algorithm>
#include <array>
#include <charconv>
#include <set>
namespace hepcat {
namespace {
struct SearchLimit {};
using Terms = std::vector<Id>;
struct Monomial {
  mpq_class coefficient = 1;
  std::map<Id, long> powers;
};
struct ProductCollection {
  Algebra &a;
  bool extract_content, collect_propagators, prefer_positive;
  std::size_t work = 0, sums = 0;
  std::map<Id, Id> memo;
  long exponent(Id id) {
    const auto &s = a.node(id);
    long x;
    if (s.compound)
      return 0;
    auto q = std::from_chars(s.head.data(), s.head.data() + s.head.size(), x);
    return q.ec == std::errc{} && q.ptr == s.head.data() + s.head.size() &&
                   x >= -4096 && x <= 4096
               ? x
               : 0;
  }
  void append_factors(Monomial &m, Id id, long p = 1) {
    mpq_class q;
    const auto &n = a.node(id);
    if (a.number(id, q)) {
      a.number(a.pow(id, p), q);
      m.coefficient *= q;
      return;
    }
    if (n.head == "Times") {
      for (Id k : n.args)
        append_factors(m, k, p);
      return;
    }
    if (n.head == "Power" && n.args.size() == 2) {
      long e = exponent(n.args[1]);
      if (e && std::abs(e * p) < 4096) {
        append_factors(m, n.args[0], e * p);
        return;
      }
      // Integral powers of positive numerical roots simplify exactly. Symbolic
      // and complex roots remain opaque to this rule.
      if (a.number(n.args[1], q) && q.get_den() != 1) {
        mpq_class base;
        if (a.number(n.args[0], base) && base > 0) {
          mpq_class product = q * p;
          if (product.get_den() == 1 && product.get_num().fits_slong_p() &&
              product >= -4096 && product <= 4096) {
            append_factors(m, n.args[0], product.get_num().get_si());
            return;
          }
        }
      }
    }
    if ((m.powers[id] += p) == 0)
      m.powers.erase(id);
  }
  Id expression(const Monomial &m, bool group = false) {
    Terms fs{a.exact(m.coefficient)};
    std::map<long, Terms> by;
    for (auto [k, v] : m.powers) {
      if (group && (v < 0 || v > 1))
        by[v].push_back(k);
      else
        fs.push_back(a.pow(k, v));
    }
    for (auto &[v, ks] : by)
      fs.push_back(a.pow(a.mul(ks), v));
    return a.mul(fs);
  }
  void divide(Monomial &x, const Monomial &y) {
    x.coefficient /= y.coefficient;
    for (auto [k, e] : y.powers)
      if ((x.powers[k] -= e) == 0)
        x.powers.erase(k);
  }
  Id shorter(Id x, Id y) { return a.cost(x) < a.cost(y) ? x : y; }
  std::optional<Id> exact_square_root(Monomial m) {
    if (m.coefficient <= 0 ||
        !mpz_perfect_square_p(m.coefficient.get_num_mpz_t()) ||
        !mpz_perfect_square_p(m.coefficient.get_den_mpz_t()))
      return {};
    mpz_class num, den;
    mpz_sqrt(num.get_mpz_t(), m.coefficient.get_num_mpz_t());
    mpz_sqrt(den.get_mpz_t(), m.coefficient.get_den_mpz_t());
    m.coefficient = mpq_class(num, den);
    for (auto &[k, e] : m.powers) {
      if (e % 2)
        return {};
      e /= 2;
    }
    return expression(m);
  }
  Id factor_sum(Terms terms, unsigned d = 0) {
    Id raw = a.add(terms);
    if (a.node(raw).head != "Plus" || d > 24)
      return raw;
    terms = a.node(raw).args;
    if (terms.size() > 128 || ++sums > 6000)
      throw SearchLimit{};
    std::vector<Monomial> ms(terms.size());
    for (size_t i = 0; i < terms.size(); ++i)
      append_factors(ms[i], terms[i]);
    Monomial common = ms[0];
    mpz_class gcd = abs(common.coefficient.get_num()),
              lcm = common.coefficient.get_den();
    bool neg = (common.coefficient < 0);
    for (size_t i = 1; i < ms.size(); ++i) {
      auto &m = ms[i];
      mpz_gcd(gcd.get_mpz_t(), gcd.get_mpz_t(), m.coefficient.get_num_mpz_t());
      mpz_lcm(lcm.get_mpz_t(), lcm.get_mpz_t(), m.coefficient.get_den_mpz_t());
      neg = neg && m.coefficient < 0;
      for (auto it = common.powers.begin(); it != common.powers.end();) {
        auto q = m.powers.find(it->first);
        if (q == m.powers.end() || (q->second < 0) != (it->second < 0)) {
          it = common.powers.erase(it);
          continue;
        }
        it->second = it->second < 0 ? std::max(it->second, q->second)
                                    : std::min(it->second, q->second);
        ++it;
      }
    }
    // Collect only propagators already present with negative powers in this
    // sum. Their order never exceeds the maximum denominator order already
    // present.
    if (collect_propagators) {
      for (auto &m : ms)
        for (auto [k, v] : m.powers) {
          if (v < 0 && a.node(k).head == "PropDen" &&
              (!common.powers.count(k) || v < common.powers[k]))
            common.powers[k] = v;
        }
    }
    if (prefer_positive) {
      size_t negs = 0;
      for (auto &m : ms)
        negs += m.coefficient < 0;
      neg = negs * 2 > ms.size();
    }
    common.coefficient = mpq_class(neg ? -gcd : gcd, lcm);
    if (common.coefficient != 1 || !common.powers.empty()) {
      for (auto &m : ms)
        divide(m, common);
      Terms inner;
      for (auto &m : ms)
        inner.push_back(expression(m));
      auto f = factor_sum(inner, d + 1);
      auto candidate = a.mul({expression(common), f});
      return extract_content ? candidate : shorter(candidate, raw);
    }
    Id best = raw;
    if (ms.size() == 3) {
      for (size_t i = 0; i < 3; i++)
        for (size_t j = i + 1; j < 3; j++) {
          auto x = exact_square_root(ms[i]), y = exact_square_root(ms[j]);
          if (!x || !y)
            continue;
          Monomial cross;
          append_factors(cross, *x);
          append_factors(cross, *y);
          cross.coefficient *= 2;
          if (cross.powers != ms[3 - i - j].powers)
            continue;
          mpq_class sign = ms[3 - i - j].coefficient / cross.coefficient;
          if (sign == 1 || sign == -1)
            best = shorter(a.pow(a.add({*x, a.mul({a.exact(sign), *y})}), 2),
                           best);
        }
    }
    // Try grouping subsets whose terms share a costly spinor, tensor, or scalar
    // factor, even if no literal factor is common to the entire sum.
    std::map<std::pair<Id, bool>, size_t> presence;
    for (auto &m : ms)
      for (auto [k, v] : m.powers)
        ++presence[{k, v < 0}];
    std::set<std::vector<size_t>> tried;
    std::vector<std::pair<size_t, std::vector<size_t>>> choices;
    for (auto [key, count] : presence) {
      if (count < 2 || count == ms.size())
        continue;
      std::vector<size_t> group;
      for (size_t i = 0; i < ms.size(); ++i) {
        auto it = ms[i].powers.find(key.first);
        if (it != ms[i].powers.end() && (it->second < 0) == key.second)
          group.push_back(i);
      }
      if (tried.insert(group).second)
        choices.push_back({(count - 1) * a.cost(key.first), group});
    }
    std::sort(choices.rbegin(), choices.rend());
    size_t count = 0;
    for (auto &c : choices) {
      if (++count > unsigned(d < 2 ? 5 : 2))
        break;
      Terms part, other;
      for (size_t i = 0; i < terms.size(); ++i) {
        (std::binary_search(c.second.begin(), c.second.end(), i) ? part : other)
            .push_back(terms[i]);
      }
      Id grouped = factor_sum(part, d + 1);
      if (a.node(grouped).head == "Plus")
        continue;
      Id result = a.add({grouped, factor_sum(other, d + 1)});
      best = shorter(result, best);
    }
    return best;
  }
  Terms expanded(Id x) {
    const auto &n = a.node(x);
    if (n.head != "Times" && n.head != "Plus")
      return {x};
    Terms out;
    if (n.head == "Plus") {
      for (auto k : n.args) {
        auto ts = expanded(k);
        out.insert(out.end(), ts.begin(), ts.end());
        if (out.size() > 140)
          throw SearchLimit{};
      }
    } else {
      out = {a.integer(1)};
      for (Id k : n.args) {
        auto ts = expanded(k);
        if (out.size() * ts.size() > 140 ||
            (work += out.size() * ts.size()) > 4000)
          throw SearchLimit{};
        Terms next;
        for (auto lhs : out)
          for (auto rhs : ts) {
            Monomial m;
            append_factors(m, lhs);
            append_factors(m, rhs);
            next.push_back(expression(m));
          }
        out = std::move(next);
      }
    }
    return out;
  }
  Id expand_factor(Id x) { return factor_sum(expanded(x)); }
  Id collect(Id x) {
    if (auto it = memo.find(x); it != memo.end())
      return it->second;
    const auto &n = a.node(x);
    Id result = x;
    if (n.head == "Plus" || n.head == "Times" || n.head == "Power") {
      Terms ts;
      for (Id k : n.args)
        ts.push_back(collect(k));
      Id y = a.call(n.head, ts);
      if (n.head == "Plus")
        result = factor_sum(ts);
      else if (n.head == "Power" || n.head == "Times") {
        Monomial m;
        append_factors(m, y);
        result = expression(m);
      }
    }
    return memo[x] = result;
  }
  Id group_powers(Id x) {
    const auto &n = a.node(x);
    if (n.head == "Plus") {
      Terms fs;
      for (Id k : n.args)
        fs.push_back(group_powers(k));
      return a.add(fs);
    }
    if (n.head == "Times") {
      Terms fs;
      for (Id k : n.args)
        fs.push_back(group_powers(k));
      Id y = a.mul(fs);
      Monomial m;
      append_factors(m, y);
      return shorter(expression(m, true), y);
    }
    if (n.head == "Power" && n.args.size() == 2) {
      return a.call(n.head, {group_powers(n.args[0]), n.args[1]});
    }
    return x;
  }
};
} // namespace

// Only call on the completed amplitude. The search uses no shell, mass,
// model, or helicity information and treats each spinor chain as one factor.
Id compact_commuting_products(Algebra &a, Id original) {
  if (a.cost(original) > 6000)
    return original;
  Id best = original;
  std::set<Id> compared{original};
  try {
    auto rank =
        std::make_pair(expression_complexity(a, original), a.cost(original));
    auto consider = [&](Id candidate) {
      if (!compared.insert(candidate).second)
        return;
      auto next = std::make_pair(expression_complexity(a, candidate),
                                 a.cost(candidate));
      if (next < rank) {
        best = candidate;
        rank = next;
      }
    };
    const std::array<std::array<bool, 3>, 5> options{{{false, false, false},
                                                      {true, false, false},
                                                      {true, true, false},
                                                      {true, false, true},
                                                      {true, true, true}}};
    for (auto choices : options) {
      ProductCollection search{a, choices[0], choices[1], choices[2]};
      try {
        consider(search.group_powers(original));
        auto local = search.collect(original);
        consider(local);
        consider(search.group_powers(local));
        auto expanded = search.expand_factor(local);
        consider(expanded);
        consider(search.group_powers(expanded));
      } catch (const SearchLimit &) {
        // Other completed trials can still be used.
      }
    }
  } catch (const std::length_error &) {
    return original;
  } catch (const std::overflow_error &) {
    return original;
  }
  return best;
}
} // namespace hepcat
