package day16

import (
	"bufio"
	"fmt"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type pktType uint8

const (
	sumPkt pktType = iota
	productPkt
	minPkt
	maxPkt
	literalPkt
	gtPkt
	ltPkt
	eqPkt
)

func (p pktType) numArgs() int {
	switch {
	case p < literalPkt:
		return -1
	case p > literalPkt:
		return 2
	default:
		return 0
	}
}

type pkt struct {
	ver   uint8
	typ   pktType
	lit   uint64
	args  []*pkt
	depth int
}

func (p *pkt) eval() (r uint64) {
	if p.typ.numArgs() != -1 && p.typ.numArgs() != len(p.args) {
		panic("invalid packet")
	}
	switch p.typ {
	case sumPkt:
		for _, arg := range p.args {
			r += arg.eval()
		}
	case productPkt:
		r = 1
		for _, arg := range p.args {
			r *= arg.eval()
		}
	case minPkt:
		r = p.args[0].eval()
		for _, arg := range p.args[1:] {
			o := arg.eval()
			if o < r {
				r = o
			}
		}
	case maxPkt:
		r = p.args[0].eval()
		for _, arg := range p.args[1:] {
			o := arg.eval()
			if o > r {
				r = o
			}
		}
	case literalPkt:
		r = p.lit
	case ltPkt:
		if p.args[0].eval() < p.args[1].eval() {
			r = 1
		}
	case gtPkt:
		if p.args[0].eval() > p.args[1].eval() {
			r = 1
		}
	case eqPkt:
		if p.args[0].eval() == p.args[1].eval() {
			r = 1
		}
	default:
		panic("unknown packet type")
	}
	return
}

type dec struct {
	buf   []byte
	pos   int
	bit   uint8
	depth int
}

const byteLen = 8
const maxByte = uint8(255)

// Read bits [from, to) from in, numbered left to right.
func readBits(in byte, from, to uint8) uint8 {
	mask := maxByte >> (byteLen - (to - from))
	align := in >> (byteLen - to)
	return align & mask
}

// Read the next n bits.
func (d *dec) readBits(n uint8) uint64 {
	var r uint64
	// Sync to byte boundary, if possible.
	if d.bit+n > byteLen {
		r = uint64(readBits(d.buf[d.pos], d.bit, byteLen))
		n -= (byteLen - d.bit)
		d.bit = 0
		d.pos++
	}
	// Read whole bytes, if possible.
	for ; n > byteLen; n -= byteLen {
		r <<= byteLen
		r += uint64(d.buf[d.pos])
		d.pos++
	}
	// Read excess bits, or bits within a byte.
	if d.bit+n <= byteLen {
		r <<= n
		r += uint64(readBits(d.buf[d.pos], d.bit, d.bit+n))
		d.bit += n
	}
	if d.bit == byteLen {
		d.bit = 0
		d.pos++
	}
	return r
}

func (d *dec) posString() string {
	return fmt.Sprintf("d%dp%db%d", d.depth, d.pos, d.bit)
}

func (d *dec) check(cond bool, format string, a ...interface{}) {
	if !cond {
		panic(fmt.Sprintf(d.posString()+format, a...))
	}
}

func (d *dec) dbg(format string, a ...interface{}) {
	lib.Dbg(d.posString()+": "+format+"\n", a...)
}

func (d *dec) decodeString(in string) *pkt {
	*d = dec{buf: make([]byte, 0, len(in)/2)}
	if _, err := fmt.Sscanf(in, "%X", &d.buf); err != nil {
		panic(err)
	}
	return d.nextPkt()
}

func (d *dec) readLiteral() (r uint64) {
	for {
		more := d.readBits(1)
		r <<= 4
		r += d.readBits(4)
		if more != 1 {
			break
		}
	}
	return
}

func (d *dec) readType0Args() []*pkt {
	r := []*pkt{}
	toRead := int(d.readBits(15))
	d.dbg("reading %d, %d of packets", toRead/8, toRead%8)
	end := d.pos + (int(d.bit)+toRead)/8
	endBit := uint8((int(d.bit) + toRead) % 8)
	for d.pos < end || (d.pos == end && d.bit < endBit) {
		d.dbg("next packet")
		r = append(r, d.nextPkt())
	}
	d.check(d.bit == endBit, "packet ended early (got %d, %d; want %d, %d)", d.pos, d.bit, end, endBit)
	return r
}

func (d *dec) readType1Args() []*pkt {
	num := int(d.readBits(11))
	r := make([]*pkt, num)
	for i := 0; i < num; i++ {
		r[i] = d.nextPkt()
	}
	return r
}

func (d *dec) nextPkt() *pkt {
	r := &pkt{depth: d.depth}
	r.ver, r.typ = uint8(d.readBits(3)), pktType(d.readBits(3))
	d.check(r.typ <= eqPkt, "unknown packet type %d", r.typ)
	d.dbg("got %#v packet", r.typ)
	if r.typ == literalPkt {
		r.lit = d.readLiteral()
		return r
	}
	d.depth++
	lengthType := d.readBits(1)
	if lengthType == 1 {
		r.args = d.readType1Args()
	} else {
		r.args = d.readType0Args()
	}
	d.depth--
	return r
}

func (p *pkt) Part1() int {
	sum := 0
	pkts := []*pkt{p}
	for len(pkts) != 0 {
		todo := pkts[len(pkts)-1]
		pkts = pkts[0 : len(pkts)-1]

		sum += int(todo.ver)
		pkts = append(pkts, todo.args...)
	}
	return sum
}

func (p *pkt) Part2() int {
	return int(p.eval())
}

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	scanner.Scan()
	d := &dec{}
	return d.decodeString(scanner.Text()), nil
}
