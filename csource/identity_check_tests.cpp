#include "amp_expression.hpp"
#include <iostream>
#include <stdexcept>

using namespace hepcat;
namespace {
void require(bool ok, const char* message) {
    if (!ok) throw std::runtime_error(message);
}

void test_signed_comparison() {
    const std::vector<std::string> masses{"m", "m", "0", "0", "0", "0"};
    const amp_expression original("MomProd[1,1]", masses), square("Power[m,2]", masses);
    identity_check_options options;
    options.seed = 41619;
    options.samples = 5;
    auto report = original.check_identity(square, options);
    require(report.passed() && report.compared == 5 && !report.failing_sample, "external on-shell identity");
    auto negative = amp_expression("Times[-1,Power[m,2]]", masses);
    report = original.check_identity(negative, options);
    require(report.status == identity_check_status::mismatch && report.failing_sample, "relative sign lost");
    options.seed = report.failing_sample;
    require(original.check_identity(negative, options).compared == 1, "cannot reproduce mismatch");

    const amp_expression pole("PropDen[Mom[Multiparticle[1,3]],m,w]", masses);
    const amp_expression expanded("Plus[Times[2,MomProd[1,3]],Times[I,m,w]]", masses);
    require(pole.check_identity(expanded, options).passed(), "complex width or channel sign");
    const amp_expression diagram("MomProd[1,3]", masses, {{{1,3}, "m"}});
    require(diagram.check_identity(amp_expression("0",masses), options).status == identity_check_status::mismatch,
            "identity checks inadvertently put an internal propagator on shell");

    const amp_expression first("g", masses), second("h", masses);
    require(!first.check_identity(second, options).passed(), "independent model parameters conflated");
    options.parameters = {{"g", "2/3"}, {"h", "4/6"}};
    require(first.check_identity(second, options).passed(), "explicit rational parameter assignment");
    options.parameters.clear();
    const amp_expression singular("Power[0,-1]", masses), unknown("Unrecognized[x]", masses);
    require(singular.check_identity(singular, options).status == identity_check_status::unresolved,
            "comparison passed with no regular samples");
    require(unknown.check_identity(unknown, options).status == identity_check_status::unresolved,
            "unknown physics was treated as a free scalar");
    bool rejected = false;
    try { original.check_identity(amp_expression("0", {"0","0","0","0"})); }
    catch (const std::invalid_argument&) { rejected = true; }
    require(rejected, "incompatible external legs accepted");
}

void test_color() {
    const std::vector<std::string> masses(4, "0");
    const amp_expression first("ColorDelta[ColorIndex[1],ColorIndex[2]]", masses);
    const amp_expression second("ColorDelta[ColorIndex[2],ColorIndex[1]]", masses);
    identity_check_options options;
    options.seed = 139;
    require(first.check_identity(second, options).status == identity_check_status::unresolved,
            "missing color representations accepted");
    options.color_representations = {3, -3, 1, 1};
    require(first.check_identity(second, options).passed(), "transpose of color delta");
    const amp_expression one("1", masses);
    require(first.check_identity(one, options).status == identity_check_status::mismatch,
            "off-diagonal color components missed");
}
} // namespace

int main() {
    try {
        test_signed_comparison();
        test_color();
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
