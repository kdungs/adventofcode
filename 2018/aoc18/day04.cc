#include "aoc18/day04.h"

#include <numeric>
#include <optional>
#include <regex>
#include <unordered_map>
#include <variant>
#include <vector>

namespace aoc18 {
namespace day04 {

namespace {

struct ShiftChange {
  int guard;
};

struct FallAsleep {
  int minute;
};

struct WakeUp {
  int minute;
};

using Event = std::variant<ShiftChange, FallAsleep, WakeUp>;

std::optional<Event> ParseEvent(const std::string& str) {
  static const std::regex re{
      R"(\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\] ((falls asleep)|(wakes up)|Guard #(\d+) begins shift))"};
  std::smatch match;
  if (!std::regex_match(str, match, re)) {
    return std::nullopt;
  }
  if (match.size() != 6) {
    return std::nullopt;
  }
  int minute = std::stoi(match[1].str());  // Let it throw...
  if (match[3] == "falls asleep") {
    return FallAsleep{minute};
  }
  if (match[4] == "wakes up") {
    return WakeUp{minute};
  }
  int gid = std::stoi(match[5]);  // Let it throw...
  return ShiftChange{gid};
}

class ShiftPlanBuilder {
 public:
  ShiftPlanBuilder() : cur_{.guard = kInvalidGid} {}

  std::vector<Shift> Get() {
    FinishCurrent(kInvalidGid);
    std::vector<Shift> result;
    std::swap(result, shifts_);
    return result;
  }

  void HandleEvent(const Event& event) {
    // Is there a better way to reference a member function?
    std::visit([this](auto&& evt) { Handle(evt); }, event);
  }

 private:
  static constexpr int kInvalidGid = -1;
  void FillUpTo(int upper) {
    for (int min{minute_}; min < upper; ++min) {
      // Can probably be optimized...
      cur_.asleep_on_minute[min] = asleep_;
    }
    minute_ = upper;
  }

  void FinishCurrent(int newgid) {
    if (cur_.guard != kInvalidGid) {
      FillUpTo(60);
      shifts_.push_back(cur_);
    }
    cur_ = Shift{.guard = newgid};
    minute_ = 0;
    asleep_ = 0;
  }

  void Handle(const ShiftChange& sc) { FinishCurrent(sc.guard); }

  void Handle(const FallAsleep& fa) {
    FillUpTo(fa.minute);
    asleep_ = true;
  }

  void Handle(const WakeUp& wa) {
    FillUpTo(wa.minute);
    asleep_ = false;
  }

  std::vector<Shift> shifts_;
  Shift cur_;
  int minute_;
  bool asleep_;
};

}  // namespace

std::optional<ShiftPlan> ParseShiftPlan(std::vector<std::string> lines) {
  std::sort(lines.begin(), lines.end());

  ShiftPlanBuilder spb;
  for (const std::string& line : lines) {
    auto maybe_event = ParseEvent(line);
    if (!maybe_event.has_value()) {
      return std::nullopt;
    }
    spb.HandleEvent(maybe_event.value());
  }

  return spb.Get();
}

std::unordered_map<int, std::vector<Minutes>> GroupShiftsByGuard(
    const ShiftPlan& sp) {
  std::unordered_map<int, std::vector<Minutes>> grouped;
  for (const Shift& shift : sp) {
    grouped[shift.guard].push_back(shift.asleep_on_minute);
  }
  return grouped;
}

std::tuple<int, int> FindSleepiestMinute(const std::vector<Minutes>& shifts) {
  int sleepiest_minute = 0;
  int sleepiest_count = 0;
  for (int minute{0}; minute < 60; ++minute) {
    int count = std::accumulate(shifts.begin(), shifts.end(), 0,
                                [minute](int acc, const Minutes& minutes) {
                                  return acc + minutes[minute];
                                });
    if (count > sleepiest_count) {
      sleepiest_minute = minute;
      sleepiest_count = count;
    }
  }
  return std::make_tuple(sleepiest_minute, sleepiest_count);
}

// Find the guard that has the most minutes asleep. What minute does
// that guard spend asleep the most?
int SolvePart1(const ShiftPlan& sp) {
  auto grouped = GroupShiftsByGuard(sp);

  // Find sleepiest guard.
  int sleepiest_guard{0};
  int most_minutes_asleep{0};
  for (const auto& kv : grouped) {
    int minutes_asleep = std::accumulate(
        kv.second.begin(), kv.second.end(), 0,
        [](int acc, const Minutes& minutes) { return acc + minutes.count(); });
    if (minutes_asleep > most_minutes_asleep) {
      sleepiest_guard = kv.first;
      most_minutes_asleep = minutes_asleep;
    }
  }

  int minute;
  int _;
  std::tie(minute, _) = FindSleepiestMinute(grouped[sleepiest_guard]);

  return sleepiest_guard * minute;
}

int SolvePart2(const ShiftPlan& sp) {
  auto grouped = GroupShiftsByGuard(sp);

  int sleepiest_guard = 0;
  int sleepiest_minute = 0;
  int sleepiest_count = 0;
  for (const auto& kv : grouped) {
    int minute;
    int count;
    std::tie(minute, count) = FindSleepiestMinute(kv.second);
    if (count > sleepiest_count) {
      sleepiest_guard = kv.first;
      sleepiest_minute = minute;
      sleepiest_count = count;
    }
  }

  return sleepiest_guard * sleepiest_minute;
}

}  // namespace day04
}  // namespace aoc18
