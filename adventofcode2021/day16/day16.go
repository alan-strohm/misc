package day16

import (
	"bufio"
	"fmt"

	"github.com/alan-strohm/misc/adventofcode2021/lib"
)

type pktType uint8

const (
	literalPkt pktType = 4
)

type pkt struct {
	ver  uint8
	typ  pktType
	lit  int
	args []*pkt
}

type dec struct {
	buf []byte
	pos int
	bit uint8
}

func readBits(in byte, from, to uint8) uint8 {
	mask := uint8(255) >> (8 - (to - from))
	align := ((in << from) >> (from + (8 - to)))
	return align & mask
}

func (d *dec) readBits(n uint8) uint64 {
	lib.Dbg("reading %d bits from %d, %d\n", n, d.pos, d.bit)
	var r uint64
	if d.bit+n > 8 {
		r = uint64(readBits(d.buf[d.pos], d.bit, 8))
		n -= (8 - d.bit)
		d.bit = 0
		d.pos++
	}
	for ; n > 8; n -= 8 {
		r <<= 8
		r += uint64(d.buf[d.pos])
		d.pos++
	}
	if d.bit+n <= 8 {
		r <<= n
		r += uint64(readBits(d.buf[d.pos], d.bit, d.bit+n))
		d.bit += n
	}
	return r
}

func (d *dec) decodeString(in string) (*pkt, error) {
	*d = dec{buf: make([]byte, 0, len(in)/2)}
	if _, err := fmt.Sscanf(in, "%X", &d.buf); err != nil {
		return nil, err
	}
	return d.nextPkt(), nil
}

func (d *dec) readLiteral() {
	for {
		more := d.readBits(1)
		d.readBits(4)
		if more != 1 {
			break
		}
	}
}

func (d *dec) readType0Args() []*pkt {
	r := []*pkt{}
	toRead := int(d.readBits(15))
	end := d.pos + (int(d.bit)+toRead)/8
	endBit := uint8((int(d.bit) + toRead) % 8)
	lib.Dbg("stopping at byte %d, bit %d\n", end, endBit)
	for d.pos < end || d.pos == end && d.bit < endBit {
		r = append(r, d.nextPkt())
	}
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
	r := &pkt{}
	r.ver, r.typ = uint8(d.readBits(3)), pktType(d.readBits(3))
	if r.typ == literalPkt {
		d.readLiteral()
		return r
	}
	lengthType := d.readBits(1)
	if lengthType == 1 {
		r.args = d.readType1Args()
	} else {
		r.args = d.readType0Args()
	}
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

func (p *pkt) Part2() int { return 0 }

func New(scanner *bufio.Scanner) (lib.Solution, error) {
	scanner.Scan()
	d := &dec{}
	return d.decodeString(scanner.Text())
}
