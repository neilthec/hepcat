#include "amp_expression.hpp"
#include "reduce.hpp"

#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <thread>
#include "process_memory.hpp"

using Clock=std::chrono::steady_clock;
static double seconds(Clock::time_point a,Clock::time_point b) {
    return std::chrono::duration<double>(b-a).count();
}

static void barrier(const char* signal,const char* reply) {
    auto prefix=std::getenv("HEPCAT_BENCH_PHASE");
    if (!prefix || !*prefix) return;
    const auto file=std::string(prefix)+".";
    std::ofstream(file+signal).close();
    const auto deadline=Clock::now()+std::chrono::seconds(60);
    while (!std::filesystem::exists(file+reply)) {
        if (Clock::now()>deadline) throw std::runtime_error("benchmark acknowledgement timed out");
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
}

int main(int argc,char** argv) {
    try {
        if (argc!=3) throw std::runtime_error("usage: hepcat-reduce INPUT OUTPUT");
        const auto start=Clock::now();
        std::ifstream input(argv[1]);
        std::string count, massText, shellText, expression;
        if (!std::getline(input,count) || !std::getline(input,massText) ||
            !std::getline(input,shellText) || !std::getline(input,expression))
            throw std::runtime_error("invalid benchmark input");
        hepcat::Algebra metadata;
        auto list=metadata.parse(massText);
        auto cuts=metadata.parse(shellText);
        if (metadata.node(list).head!="List" || metadata.node(cuts).head!="List")
            throw std::runtime_error("expected mass and shell lists");
        std::vector<std::string> masses;
        std::vector<hepcat::numerator_shell> shells;
        for (auto id : metadata.node(list).args) masses.push_back(metadata.format(id));
        if (std::stoul(count)!=masses.size()) throw std::runtime_error("particle count disagrees with masses");
        for (auto id : metadata.node(cuts).args) {
            const auto& s=metadata.node(id);
            if (s.head!="Shell" || s.args.size()!=2 || metadata.node(s.args[0]).head!="List")
                throw std::runtime_error("expected Shell[List[...],mass]");
            std::vector<int> legs;
            for (auto leg : metadata.node(s.args[0]).args) legs.push_back(metadata.leg(leg));
            shells.push_back({std::move(legs),metadata.format(s.args[1])});
        }
        hepcat::amp_expression amp(expression,masses,shells);
        const auto imported=Clock::now();
        barrier("ready","go");
        const auto rssBefore=highWaterRSS();
        const auto before=Clock::now();
        amp.reduce();
        const auto after=Clock::now();
        const auto rssAfter=highWaterRSS();
        barrier("done","ack");
        const auto writing=Clock::now();
        std::ofstream output(argv[2]);
        output << amp.str() << '\n';
        output.close();
        if (!output) throw std::runtime_error("cannot write result");
        std::cout << std::setprecision(10) << "{\"Seconds\":" << seconds(before,after)
                  << ",\"HighWaterRSSBeforeBytes\":" << rssBefore
                  << ",\"HighWaterRSSAfterBytes\":" << rssAfter
                  << ",\"ImportSeconds\":" << seconds(start,imported)
                  << ",\"ExportSeconds\":" << seconds(writing,Clock::now()) << "}\n";
        return 0;
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
