#include "validation_precision_parameters.hpp"
#include "model.hpp"
#include <iostream>
#include <stdexcept>
using namespace hepcat;
using namespace hepcat::validation;
namespace {
std::string key(const model& m, const std::string& name) {
    for (auto rows : {&m.variables(), &m.functions()})
        for (const auto& p : *rows)
            if (p.name == name)
                return m.algebra().format(p.symbol);
    throw std::runtime_error("missing model parameter " + name);
}
void check(const model& m) {
    auto parameters = precise_model_parameters(m);
    const auto get = [&](const std::string& n) { return parameters.at(key(m, n)).x; };
    precision::scalar residual = get("CW") * get("MZ") - get("MW");
    if (abs(residual) > precision::scalar("1e-42", precision::bits))
        throw std::runtime_error("MZ did not preserve MW/CW in the replay");
    residual = get("SW") * get("SW") + get("CW") * get("CW") - 1;
    if (abs(residual) > precision::scalar("1e-42", precision::bits))
        throw std::runtime_error("weak mixing relation evaluated at low precision");
    residual = 2000 * get("Me") - 1;
    if (abs(residual) > precision::scalar("1e-42", precision::bits))
        throw std::runtime_error("default electron mass rounded before replay");
    auto mw = key(m, "MW"), mz = key(m, "MZ"), cw = key(m, "CW");
    auto adjusted = precise_model_parameters(m, {{"MW", {43, 0}}, {mw, {21, 0}}});
    residual = adjusted.at(mz).x * adjusted.at(cw).x - 21;
    if (abs(residual) > precision::scalar("1e-42", precision::bits))
        throw std::runtime_error("scoped override did not propagate to derived mass");
}
} // namespace
int main(int argc, char** argv) {
    if (argc != 3)
        return 2;
    check(model(argv[1], 1));
    check(model(argv[2], 1));
    std::cout << "Constructive and Feynman model inputs retain 160-bit precision.\n";
}
