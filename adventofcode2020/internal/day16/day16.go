package day16

import (
	"errors"
	"flag"
	"fmt"
	"log"
	"sort"
	"strconv"
	"strings"

	"../io"
)

var dbg = flag.Bool("dbg", false, "Print debug info.")

func init() {
	flag.Parse()
}

type interval struct {
	min, max int
}

func (i *interval) Contains(n int) bool {
	return n >= i.min && n <= i.max
}

func (i *interval) Equals(o *interval) bool {
	if i == nil || o == nil {
		return i == nil && o == nil
	}
	return i.min == o.min && i.max == o.max
}

type union []interval

func newUnion() union {
	return make([]interval, 0)
}

func (u *union) Len() int {
	return len(*u)
}
func (u *union) Less(i, j int) bool {
	return (*u)[i].min < (*u)[j].min
}
func (u *union) Swap(i, j int) {
	(*u)[i], (*u)[j] = (*u)[j], (*u)[i]
}

func parseUnion(s string) (union, error) {
	intervals := strings.Split(s, ",")
	r := newUnion()
	for _, i := range intervals {
		if i == "" {
			continue
		}
		bounds := strings.Split(i, "-")
		if len(bounds) != 2 {
			return nil, fmt.Errorf("invalid interval: %s", i)
		}
		min, err := strconv.Atoi(bounds[0])
		if err != nil {
			return nil, fmt.Errorf("invalid interval: %s", i)
		}
		max, err := strconv.Atoi(bounds[1])
		if err != nil {
			return nil, fmt.Errorf("invalid interval: %s", i)
		}
		r = append(r, interval{min: min, max: max})
	}
	sort.Sort(&r)
	r.compact()
	return r, nil
}

func (u *union) compact() {
	if !sort.IsSorted(u) {
		log.Fatal("can't compact unsorted union")
	}
	i := 0
	for j, curr := range *u {
		back := &(*u)[i]
		if curr.min > back.max {
			i++
			if i != j {
				(*u)[i] = curr
			}
		} else if curr.max > back.max {
			back.max = curr.max
		}
	}
	if i < len(*u) {
		*u = (*u)[:i+1]
	}
}

func (s union) findEntry(i int) (int, *interval) {
	loc := 0
	for min, max := 0, len(s); min < max; {
		guess := min + (max-min)/2
		if s[guess].max < i {
			min = guess + 1
			loc = min
		} else if s[guess].min > i {
			max = guess - 1
			loc = guess
		} else {
			return guess, &s[guess]
		}
	}
	return loc, nil
}

func (s union) String() string {
	var b strings.Builder
	for i, interval := range s {
		fmt.Fprintf(&b, "%d-%d", interval.min, interval.max)
		if i != len(s)-1 {
			fmt.Fprintf(&b, ",")
		}
	}
	return b.String()
}

func (u *union) AddUnion(o union) {
	*u = append(*u, o...)
	sort.Sort(u)
	u.compact()
}

func (u union) Contains(n int) bool {
	for _, i := range u {
		if i.Contains(n) {
			return true
		}
	}
	return false
}

type Input struct {
	fieldReq map[string]union
	yours    []int
	nearby   [][]int
	allReqs  union
}

type scanner struct {
	in        *Input
	numBlanks int
}

func (s *scanner) parseField(l string) error {
	parts := strings.Split(l, ": ")
	if len(parts) != 2 {
		return fmt.Errorf("malformed field line: %s", l)
	}
	req := strings.ReplaceAll(parts[1], " or ", ",")
	u, err := parseUnion(req)
	if err != nil {
		return fmt.Errorf("%s, in field %s", err, l)
	}
	s.in.fieldReq[parts[0]] = u
	s.in.allReqs.AddUnion(u)
	return nil
}

func parseTicket(l string) ([]int, error) {
	vals := strings.Split(l, ",")
	r := make([]int, len(vals))
	for i, val := range vals {
		var err error
		r[i], err = strconv.Atoi(val)
		if err != nil {
			return nil, fmt.Errorf("invalid ticket value: %s", val)
		}
	}
	return r, nil
}

func (s *scanner) ScanLine(l string) (bool, error) {
	if l == "" {
		s.numBlanks++
		return false, nil
	}
	switch s.numBlanks {
	case 0:
		if err := s.parseField(l); err != nil {
			return false, err
		}
		return false, nil
	case 1:
		if l == "your ticket:" {
			return false, nil
		}
		ticket, err := parseTicket(l)
		if err != nil {
			return false, err
		}
		s.in.yours = ticket
		return false, nil
	case 2:
		if l == "nearby tickets:" {
			return false, nil
		}
		ticket, err := parseTicket(l)
		if err != nil {
			return false, err
		}
		s.in.nearby = append(s.in.nearby, ticket)
		return false, nil
	}
	return true, nil
}

func ParseInput(fname string) (*Input, error) {
	s := &scanner{in: &Input{
		fieldReq: make(map[string]union),
		yours:    make([]int, 0),
		nearby:   make([][]int, 0),
		allReqs:  newUnion(),
	}}
	if err := io.Scan(s, fname); err != nil {
		return nil, err
	}
	return s.in, nil
}

func (in *Input) errorRate(ticket []int) (rate int) {
	for _, val := range ticket {
		if !in.allReqs.Contains(val) {
			rate += val
		}
	}
	return rate
}

func ErrorRate(in *Input) (rate int) {
	for _, ticket := range in.nearby {
		rate += in.errorRate(ticket)
	}
	return
}

type track struct {
	possible       []bool
	numPoss, known int
	name           string
	dbg            func(int) string
}

func (t track) String() string {
	parts := make([]string, len(t.possible))
	for i, poss := range t.possible {
		if poss {
			parts[i] = t.dbg(i)
		} else {
			parts[i] = "!" + t.dbg(i)
		}
	}
	return fmt.Sprintf("[%s]", strings.Join(parts, ","))
}

func newTrack(n int, name string, dbg func(int) string) *track {
	r := &track{possible: make([]bool, n), known: -1, numPoss: n, name: name, dbg: dbg}
	r.fill(true)
	return r
}

func (t *track) fill(possible bool) {
	for i, _ := range t.possible {
		t.possible[i] = possible
	}
}

func (t *track) MarkImp(imp int) (int, error) {
	if *dbg {
		fmt.Printf("%s != %s; ", t.name, t.dbg(imp))
	}
	if !t.possible[imp] {
		return -1, nil
	}
	if t.IsKnown() {
		return -1, errors.New("impossible track")
	}

	t.possible[imp] = false
	t.numPoss--

	if t.IsKnown() {
		for idx, poss := range t.possible {
			if poss {
				t.SetKnown(idx)
				return idx, nil
			}
		}
	}
	return -1, nil
}

func (t *track) SetKnown(known int) {
	t.fill(false)
	t.possible[known] = true
	t.known = known
	t.numPoss = 1
}

func (t *track) IsKnown() bool {
	return t.numPoss == 1
}

type tracks []*track

func newTracks(num int, rdbg, cdbg func(int) string) tracks {
	r := make([]*track, num)
	for i, _ := range r {
		r[i] = newTrack(num, rdbg(i), cdbg)
	}
	return r
}

// Mark that pidx is known index for tidx.
// Mark pidx as impossible for all other tracks.  This may make some tracks
// known.  The mapping from newly known track to it's pidx is returned.
func (t tracks) SetKnown(tidx, pidx int) (map[int]int, error) {
	newKnown := make(map[int]int)
	for i, track := range t {
		if i == tidx {
			track.SetKnown(pidx)
		} else {
			known, err := track.MarkImp(pidx)
			if err != nil {
				return newKnown, fmt.Errorf("track %d -> poss %d implies track %d != %d: %s", tidx, pidx, i, pidx, err)
			}
			if known != -1 {
				newKnown[i] = known
			}
		}
	}
	return newKnown, nil
}

type tracker struct {
	fieldTracks tracks
	indexTracks tracks

	fieldToID map[string]int
	idToField map[int]string
}

func newTracker(fields []string) *tracker {
	r := &tracker{
		fieldToID: make(map[string]int),
		idToField: make(map[int]string),
	}
	sort.Strings(fields)
	for i, field := range fields {
		r.fieldToID[field] = i
		r.idToField[i] = field
	}
	indexDbg := func(i int) string { return strconv.Itoa(i) }
	fieldDbg := func(i int) string {
		str, ok := r.idToField[i]
		if ok {
			return str
		}
		return "unk"
	}
	r.indexTracks = newTracks(len(fields), indexDbg, fieldDbg)
	r.fieldTracks = newTracks(len(fields), fieldDbg, indexDbg)
	if *dbg {
		fmt.Printf("%d fields: %v\n", len(fields), fields)
	}
	return r
}

func (t *tracker) GetMapping() map[string]int {
	r := make(map[string]int)
	for fieldID, track := range t.fieldTracks {
		r[t.idToField[fieldID]] = track.known
	}
	return r
}

func (t *tracker) MarkImp(field string, index int) error {
	fieldID := t.fieldToID[field]
	fieldToIndex := make(map[int]int)
	newIndex, err := t.fieldTracks[fieldID].MarkImp(index)
	if err != nil {
		return fmt.Errorf("field [%s] != %d: %s", field, index, err)
	}
	if newIndex != -1 {
		fieldToIndex[fieldID] = newIndex
	}
	newFieldID, err := t.indexTracks[index].MarkImp(fieldID)
	if err != nil {
		return fmt.Errorf("field [%s] != %d: %s", field, index, err)
	}
	if newFieldID != -1 {
		fieldToIndex[newFieldID] = index
	}
	if len(fieldToIndex) != 0 {
		err := t.setKnown(fieldToIndex)
		if err != nil {
			return fmt.Errorf("field [%s] != %d: %s", field, index, err)
		}
	}
	return nil
}

func (t *tracker) setKnown(fieldToIndex map[int]int) error {
	for len(fieldToIndex) != 0 {
		nextFieldToIndex := make(map[int]int)
		for field, index := range fieldToIndex {
			fieldKnown, err := t.fieldTracks.SetKnown(field, index)
			if err != nil {
				return fmt.Errorf("field %s -> index %d: %s", t.idToField[field], index, err)
			}
			for field, index := range fieldKnown {
				nextFieldToIndex[field] = index
			}
			indexKnown, err := t.indexTracks.SetKnown(index, field)
			if err != nil {
				return fmt.Errorf("field %s -> index %d: %s", t.idToField[field], index, err)
			}
			for index, field := range indexKnown {
				nextFieldToIndex[field] = index
			}
		}
		fieldToIndex = nextFieldToIndex
	}
	return nil
}

func (in *Input) fieldIndices() map[string]int {
	fields := make([]string, 0, len(in.fieldReq))
	for field, _ := range in.fieldReq {
		fields = append(fields, field)
	}
	t := newTracker(fields)
	for _, ticket := range in.nearby {
		if in.errorRate(ticket) > 0 {
			continue
		}
		for idx, val := range ticket {
			for field, req := range in.fieldReq {
				if !req.Contains(val) {
					if err := t.MarkImp(field, idx); err != nil {
						log.Fatal(err)
					}
					fmt.Printf("\n")
				}
			}
		}
	}
	return t.GetMapping()
}

func YourDepartureProduct(in *Input) int {
	r := 1
	indices := in.fieldIndices()
	for field, _ := range in.fieldReq {
		if strings.HasPrefix(field, "departure ") {
			r *= in.yours[indices[field]]
		}
	}
	return r
}
