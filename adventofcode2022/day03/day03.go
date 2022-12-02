package day03

import (
	"bufio"
	"unicode"
)

type Bucket int

const (
	noBucket    Bucket = 0
	bothBuckets Bucket = 3
)

func priorityIndex(r rune) int {
	if unicode.IsUpper(r) {
		return int(r-'A') + 26
	}
	return int(r - 'a')
}

type typeInfo struct {
	type_  rune
	count  int
	bucket Bucket
}

type bucketInfo struct {
	bucket Bucket
	types  []*typeInfo
}

type rucksack struct {
	types   [52]*typeInfo
	buckets [4]*bucketInfo
}

func newRucksack(items []rune) *rucksack {
	r := &rucksack{}
	// Init
	for i, _ := range r.buckets {
		r.buckets[i] = &bucketInfo{bucket: Bucket(i)}
	}
	for i, _ := range r.types {
		type_ := 'a' + i
		if i >= 26 {
			type_ = 'A' + i - 26
		}
		r.types[i] = &typeInfo{type_: rune(type_)}
	}

	// Fill
	for i, item := range items {
		bucket := Bucket(1)
		if i >= len(items)/2 {
			bucket = Bucket(2)
		}
		info := r.types[priorityIndex(item)]
		if info.bucket == noBucket {
			info.bucket = bucket
		} else if info.bucket != bucket {
			info.bucket = bothBuckets
		}
		info.count += 1
	}
	for _, info := range r.types {
		bucket := r.buckets[info.bucket]
		bucket.types = append(bucket.types, info)
	}
	return r
}

func Run(s *bufio.Scanner, part1 bool) (int, error) {
	part1r := 0
	for s.Scan() {
		sack := newRucksack([]rune(s.Text()))
		for _, info := range sack.buckets[bothBuckets].types {
			part1r += priorityIndex(info.type_) + 1
		}
	}
	return part1r, nil
}
