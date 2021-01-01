package day25

import "log"

func loop(subject, value int) int {
	value *= subject
	value %= 20201227
	return value
}

func getKey(subject, pk1, pk2 int) int {
	value := 1
	for i := 0; ; i++ {
		if value == pk1 {
			log.Printf("Key %d has loop size %d\n", pk1, i)
			return transform(pk2, i)
		} else if value == pk2 {
			log.Printf("Key %d has loop size %d\n", pk2, i)
			return transform(pk1, i)
		}
		value = loop(subject, value)
	}
}

func transform(subject, loopSize int) int {
	value := 1
	for i := 0; i < loopSize; i++ {
		value = loop(subject, value)
	}
	return value
}
