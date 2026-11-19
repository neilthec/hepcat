#include "amplitude.hpp"

#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

using hepcat::amplitude;
using hepcat::amp_expression;
using hepcat::amplitude_item;

namespace {
void check(bool ok, const char* message) {
    if (!ok) throw std::runtime_error(message);
}

std::string tail(const amp_expression& expr) {
    auto rest = expr.discarded_remainder();
    return rest ? rest->str() : "0";
}

void same(const std::vector<amplitude_item>& left, const std::vector<amplitude_item>& right) {
    check(left.size() == right.size(), "item count changed");
    for (std::size_t i = 0; i < left.size(); ++i) {
        check(left[i].expression.str() == right[i].expression.str(), "row order or expression changed");
        check(tail(left[i].expression) == tail(right[i].expression), "remainder differs");
        check(left[i].shells.size() == right[i].shells.size(), "shell count changed");
        for (std::size_t j = 0; j < left[i].shells.size(); ++j) {
            check(left[i].shells[j].legs == right[i].shells[j].legs &&
                  left[i].shells[j].mass == right[i].shells[j].mass, "shell metadata changed");
        }
    }
}
}

int main() {
    try {
        amplitude batch({"0","0","0","0"});
        check(&batch.reduce() == &batch && batch.last_worker_count() == 0, "empty amp");
        const std::string dot = "MomProd[1,2]";
        batch.add_diagram(dot, {{{1,2},"2"}});
        batch.add_factorization_product(dot, {{2,1},"4"});
        batch.add_diagram(dot);
        batch.add_factorization_product(dot, {{3,4},"6"});
        batch.add_factorization_product(dot, {{2,1},"4"});
        auto serial = batch;
        serial.reduce(1);
        check(serial.last_worker_count() == 1, "explicit single worker ignored");
        check(batch.diagrams()[0].expression.str() == dot, "reducing a copy changed the source");
        batch.reduce(3);
        check(batch.last_worker_count() == 3, "explicit worker count ignored");
        same(serial.diagrams(), batch.diagrams());
        same(serial.factorization_products(), batch.factorization_products());
        const auto& diagrams = batch.diagrams();
        check(diagrams[0].expression.str() == "2" && diagrams[1].expression.str() == dot,
              "a diagram adopted another diagram's shell");
        check(tail(diagrams[0].expression) != "0" && tail(diagrams[1].expression) == "0",
              "off-shell remainder scope");
        auto& products = batch.factorization_products();
        check(products[0].expression.str() == "8" && products[1].expression.str() == "18" &&
              products[2].expression.str() == "8", "duplicate or complementary channels conflated");
        check(products[0].shells[0].legs == std::vector<int>({2,1}), "channel label reordered");
        batch.reduce();
        check(batch.last_worker_count() == 0, "completed items scheduled again");
        bool rejected = false;
        try { batch.add_diagram(dot, {{{1,2},"2"},{{3,4},"4"}}); }
        catch (const std::invalid_argument&) { rejected = true; }
        check(rejected && batch.diagrams().size() == 2, "conflicting shells altered batch");
        batch.add_diagram("Plus[foo,foo]");
        batch.reduce(99);
        check(batch.last_worker_count() == 1 && batch.diagrams()[2].expression.str() == "Times[2,foo]",
              "reducing appended item");
        same(serial.factorization_products(), batch.factorization_products());
        auto failing = serial;
        failing.add_diagram("Plus[x,x]");
        failing.add_factorization_product("Power[0,-1]", {{1,3},"0"});
        for (std::size_t workers : {1,3}) {
            bool failed = false;
            try { failing.reduce(workers); }
            catch (const std::runtime_error& e) {
                failed = std::string(e.what()).find("factorization product 4") != std::string::npos;
            }
            check(failed && failing.diagrams().back().expression.str() == "Plus[x,x]" &&
                  failing.last_worker_count() == 1, "failure published partial work or lost the failing row");
        }
        return 0;
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
