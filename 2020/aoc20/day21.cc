#include "aoc20/day21.h"

#include <algorithm>
#include <ios>
#include <iostream>
#include <istream>
#include <iterator>
#include <map>
#include <string>
#include <unordered_map>

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/strings/str_split.h"
#include "absl/strings/string_view.h"

namespace aoc20 {
namespace day21 {

std::istream& operator>>(std::istream& is, Entry& e) {
  std::string line;
  std::getline(is, line);
  std::vector<absl::string_view> parts = absl::StrSplit(line, " (contains ");
  if (parts.size() != 2) {
    is.setstate(std::ios::failbit);
    return is;
  }
  parts[1].remove_suffix(1);  // get rid of ")"
  e.ingredients = absl::StrSplit(parts[0], ' ');
  e.allergens = absl::StrSplit(parts[1], ", ");
  return is;
}

std::map<std::string, std::string> DetermineAllergenes(
    const std::vector<Entry>& entries) {
  // Each allergen is found in _exactly one_ ingredient. (Note: that means
  // globally, not per Entry.) Each ingredient contains zero or one allergen.

  // Obtain all possible ingredients and allergens.
  StrSet ui;
  StrSet ua;
  for (const Entry& e : entries) {
    ui.insert(e.ingredients.begin(), e.ingredients.end());
    ua.insert(e.allergens.begin(), e.allergens.end());
  }

  // Determine the potential candidates for each allergen. A candidate has to
  // appear in every entry for which the allergen is listed.
  std::unordered_map<std::string, StrSet> candidates;
  for (const std::string& a : ua) {
    candidates[a] = ui;
  }
  for (const Entry& e : entries) {
    for (const std::string& a : e.allergens) {
      // Retain only the union of the existing candidates and the set of
      // ingredients.
      StrSet i = e.ingredients;
      i.merge(candidates[a]);
    }
  }
  // From here we can further narrow it down by successively removing the ones
  // that fit.
  // Since there are very few allergens, doing this in O(n^2) is not a problem.
  // Otherwise, we could sort by popcount and sift down like on the other day.
  std::map<std::string, std::string> allergen_to_ingredient;
  bool allempty = false;
  while (!allempty) {
    allempty = true;
    for (auto& [a, cs] : candidates) {
      if (cs.size() == 0) {
        continue;
      }
      allempty = false;
      if (cs.size() == 1) {
        std::string i = *cs.begin();
        allergen_to_ingredient[a] = i;
        for (auto& [_, ocs] : candidates) {
          ocs.erase(i);
        }
      }
    }
  }

  return allergen_to_ingredient;
}

int64_t Part1(const std::vector<Entry>& entries) {
  auto allergen_to_ingredient = DetermineAllergenes(entries);
  StrSet ingredients_with_allergens;
  std::transform(allergen_to_ingredient.begin(), allergen_to_ingredient.end(),
                 std::inserter(ingredients_with_allergens,
                               ingredients_with_allergens.begin()),
                 [](const auto& kv) { return kv.second; });

  int64_t count{0};
  for (const auto& e : entries) {
    for (const auto& i : e.ingredients) {
      if (ingredients_with_allergens.count(i) == 0) {
        ++count;
      }
    }
  }
  return count;
}

std::string Part2(const std::vector<Entry>& entries) {
  auto allergen_to_ingredient = DetermineAllergenes(entries);
  std::vector<absl::string_view> ingredients_sorted_by_allergen;
  ingredients_sorted_by_allergen.reserve(allergen_to_ingredient.size());
  for (const auto& [a, i] : allergen_to_ingredient) {
    ingredients_sorted_by_allergen.push_back(i);
  }
  return absl::StrJoin(ingredients_sorted_by_allergen.begin(),
                       ingredients_sorted_by_allergen.end(), ",");
}

}  // namespace day21
}  // namespace aoc20
