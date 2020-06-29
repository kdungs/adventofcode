package main

import (
	"fmt"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
)

var (
	reDate  = regexp.MustCompile(`\d{4}-\d{2}-\d{2} \d{2}:\d{2}`)
	reGuard = regexp.MustCompile(`Guard #(\d+) begins shift`)
)

const (
	minutes = 60
)

type activity struct {
	Guard         int
	MinutesAsleep [minutes]bool
}

func (a *activity) asleep(minute int) {
	for m := minute; m < minutes; m++ {
		a.MinutesAsleep[m] = true
	}
}

func (a *activity) awake(minute int) {
	for m := minute; m < minutes; m++ {
		a.MinutesAsleep[m] = false
	}
}

type schedule struct {
	guards     []int
	activities []activity
}

func (s *schedule) String() string {
	var sb strings.Builder
	for _, act := range s.activities {
		sb.WriteString(fmt.Sprintf("%4d: ", act.Guard))
		for _, asleep := range act.MinutesAsleep {
			if asleep {
				sb.WriteString("X")
			} else {
				sb.WriteString(" ")
			}
		}
		sb.WriteString("\n")
	}
	return sb.String()
}

func readSchedule(input string) (*schedule, error) {
	lines := strings.Split(strings.TrimSuffix(input, "\n"), "\n")
	// This makes sure events are sorted by datetime
	sort.Strings(lines)
	gs := make(map[int]bool)
	as := make([]activity, 0)
	var a activity
	for _, line := range lines {
		datestr := reDate.FindString(line)
		t, err := time.Parse("2006-01-02 15:04", datestr)
		if err != nil {
			return nil, err
		}
		switch line[19] {
		case 'w':
			// wakes up
			a.awake(t.Minute())
		case 'f':
			// falls asleep
			a.asleep(t.Minute())
		case 'G':
			// guard change
			gmatch := reGuard.FindStringSubmatch(line)
			if len(gmatch) != 2 {
				return nil, fmt.Errorf("could not match guard: %s", line)
			}
			g, err := strconv.Atoi(gmatch[1])
			if err != nil {
				return nil, err
			}
			gs[g] = true
			as = append(as, a)
			a = activity{Guard: g}
		}
	}
	as = append(as, a)
	gids := make([]int, 0, len(gs))
	for gid := range gs {
		gids = append(gids, gid)
	}
	return &schedule{
		guards:     gids,
		activities: as,
	}, nil
}

func totalMinutesAsleep(as []activity) map[int]int {
	res := make(map[int]int)
	for _, a := range as {
		for _, asleep := range a.MinutesAsleep {
			if asleep {
				res[a.Guard]++
			}
		}
	}
	return res
}

func sleepiestMinute(g int, as []activity) (int, int) {
	minutes := make(map[int]int)
	for _, a := range as {
		if a.Guard != g {
			continue
		}
		for min, asleep := range a.MinutesAsleep {
			if asleep {
				minutes[min]++
			}
		}
	}

	maxmin := 0
	maxslp := 0
	for min, slp := range minutes {
		if slp > maxslp {
			maxmin = min
			maxslp = slp
		}
	}
	return maxmin, maxslp
}

// D04Part1 solves part 1 of day 4.
// https://adventofcode.com/2018/day/4
func D04Part1(input string) int {
	sched, err := readSchedule(input)
	if err != nil {
		panic(err)
	}

	minsAsleep := totalMinutesAsleep(sched.activities)
	maxguard := 0
	maxmins := 0
	for guard, mins := range minsAsleep {
		if mins > maxmins {
			maxguard = guard
			maxmins = mins
		}
	}
	min, _ := sleepiestMinute(maxguard, sched.activities)
	return maxguard * min
}

// D04Part2 solves part 2 of day 4.
// https://adventofcode.com/2018/day/4
func D04Part2(input string) int {
	sched, err := readSchedule(input)
	if err != nil {
		panic(err)
	}

	maxguard := 0
	maxmin := 0
	maxslp := 0
	for _, guard := range sched.guards {
		min, slp := sleepiestMinute(guard, sched.activities)
		if slp > maxslp {
			maxguard = guard
			maxmin = min
			maxslp = slp
		}
	}
	return maxguard * maxmin
}
