#pragma once
#include "generation_plan.hpp"
#include "model.hpp"
#include "reduce.hpp"

namespace hepcat::generation {
void resolve_diagram_colors(std::vector<plan>&, const model&, const std::vector<std::string>&);
Id vertex_color(Algebra&, const model&, const term&, std::size_t vertex_index,
                const std::vector<std::string>& process);
Id relabel_color(Algebra&, const model&, const term&, std::size_t vertex_index, Id tensor,
                 const std::vector<std::string>& process);
} // namespace hepcat::generation
