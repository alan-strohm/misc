package day25

import "testing"

func TestGetKey(t *testing.T) {
	testCases := []struct {
		pk1, pk2, want int
	}{
		{pk1: 5764801, pk2: 17807724, want: 14897079},
		{pk1: 14205034, pk2: 18047856, want: 297257},
	}
	for _, tc := range testCases {
		got := getKey(7, tc.pk1, tc.pk2)
		if got != tc.want {
			t.Errorf("getKey(7, %d, %d) = %d, want %d", tc.pk1, tc.pk2, got, tc.want)
		}
	}
}
