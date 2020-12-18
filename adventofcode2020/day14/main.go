package main

import (
	"bufio"
	"flag"
	"fmt"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
)

var in = flag.String("in", "./input.txt", "File to read.")
var dbg = flag.Bool("dbg", false, "Print debug info.")

func init() {
	flag.Parse()
}

type Processor interface {
	ProcessLine(l string) (done bool, err error)
	Close() error
}

func Process(p Processor) error {
	f, err := os.Open(*in)
	if err != nil {
		return fmt.Errorf("opening %s: %s", *in, err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		if done, err := p.ProcessLine(scanner.Text()); err != nil || done {
			return err
		}
	}
	return p.Close()
}

type exec interface {
	SetMask(mask string) error
	SetMem(addr, value uint64)
	Mem() map[uint64]uint64
}

type parser struct {
	e exec
}

func (p *parser) Close() error {
	return nil
}

var memRe = regexp.MustCompile(`^mem\[([0-9]+)\] = ([0-9]+)$`)

func (p *parser) ProcessLine(l string) (bool, error) {
	if strings.HasPrefix(l, "mask = ") {
		mask := strings.TrimPrefix(l, "mask = ")
		if len(mask) != 36 {
			return false, fmt.Errorf("mask too long (%d > 36)", len(mask))
		}
		p.e.SetMask(mask)
		return false, nil
	}
	matches := memRe.FindStringSubmatch(l)
	if matches != nil || len(matches) != 3 {
		addr, err := strconv.ParseUint(matches[1], 10, 36)
		if err != nil {
			return false, fmt.Errorf("couldn't parse address: %s", err)
		}
		val, err := strconv.ParseUint(matches[2], 10, 36)
		if err != nil {
			return false, fmt.Errorf("couldn't parse address: %s", err)
		}
		p.e.SetMem(addr, val)
		return false, nil
	}
	return false, fmt.Errorf("unexpected line: %s", l)
}

type maskMem struct {
	andMask      uint64
	orMask       uint64
	mem          map[uint64]uint64
	maxXs, minXs int
}

func newMaskMem() *maskMem {
	return &maskMem{andMask: math.MaxUint64, mem: make(map[uint64]uint64), minXs: 9}
}

func (m *maskMem) Mem() map[uint64]uint64 {
	return m.mem
}

func (m *maskMem) SetMask(mask string) (err error) {
	orMask := strings.ReplaceAll(mask, "X", "0")
	andMask := strings.ReplaceAll(mask, "X", "1")
	if m.andMask, err = strconv.ParseUint(andMask, 2, 36); err != nil {
		return
	}
	m.andMask |= 0xfffffff000000000
	if m.orMask, err = strconv.ParseUint(orMask, 2, 36); err != nil {
		return
	}
	numXs := strings.Count(mask, "X")
	if numXs > m.maxXs {
		m.maxXs = numXs
	}
	if numXs < m.minXs {
		m.minXs = numXs
	}
	return
}

func (m *maskMem) SetMem(addr uint64, val uint64) {
	val |= m.orMask
	val &= m.andMask
	m.mem[addr] = val
}

func run(e exec) (uint64, error) {
	p := &parser{e: e}
	if err := Process(p); err != nil {
		return 0, err
	}
	sum := uint64(0)
	for _, v := range e.Mem() {
		sum += v
	}
	return sum, nil
}

type addrMaskMem struct {
	andMasks []uint64
	orMasks  []uint64
	mem      map[uint64]uint64
}

func newAddrMaskMem() *addrMaskMem {
	return &addrMaskMem{andMasks: make([]uint64, 0), orMasks: make([]uint64, 0), mem: make(map[uint64]uint64)}
}

func makeMaskList(num, size int) [][]rune {
	r := make([][]rune, num)
	for i, _ := range r {
		r[i] = make([]rune, size)
	}
	return r
}

func convMaskList(in [][]rune) (r []uint64, err error) {
	r = make([]uint64, len(in))
	for i, mask := range in {
		if r[i], err = strconv.ParseUint(string(mask), 2, 36); err != nil {
			return
		}
	}
	return
}

func (m *addrMaskMem) SetMask(mask string) (err error) {
	xs := strings.Count(mask, "X")
	numCombos := 1 << xs
	andMasks := makeMaskList(numCombos, len(mask))
	orMasks := makeMaskList(numCombos, len(mask))
	runes := []rune(mask)
	for i, x := 0, 0; i < len(runes); i++ {
		for j := 0; j < len(andMasks); j++ {
			switch runes[i] {
			case '0':
				andMasks[j][i] = '1'
				orMasks[j][i] = '0'
			case '1':
				andMasks[j][i] = '1'
				orMasks[j][i] = '1'
			case 'X':
				force := (j >> x) & 1
				if force == 1 {
					andMasks[j][i] = '1'
					orMasks[j][i] = '1'
				} else if force == 0 {
					andMasks[j][i] = '0'
					orMasks[j][i] = '0'
				} else {
					err = fmt.Errorf("I can't do math: (%d >> %d) & 1 = %d", j, x, force)
					return
				}
			}
		}
		if runes[i] == 'X' {
			x++
		}
		// NB: This doesn't handle the case of 0 X's
	}
	m.orMasks, err = convMaskList(orMasks)
	if err != nil {
		return
	}
	m.andMasks, err = convMaskList(andMasks)
	return
}

func (m *addrMaskMem) SetMem(addr uint64, val uint64) {
	for i, andMask := range m.andMasks {
		orMask := m.orMasks[i]
		andMask |= 0xfffffff000000000
		addr |= orMask
		addr &= andMask
		if *dbg {
			fmt.Printf("mem[%d] = %d\n", addr, val)
		}
		m.mem[addr] = val
	}
}

func (m *addrMaskMem) Mem() map[uint64]uint64 {
	return m.mem
}

func main() {
	sum1, err := run(newMaskMem())
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	fmt.Printf("Part 1: %d\n", sum1)
	sum2, err := run(newAddrMaskMem())
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	fmt.Printf("Part 2: %d\n", sum2)
}
