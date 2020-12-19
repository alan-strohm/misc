package main

import (
	"fmt"
	"os"

	"../../internal/day16"
)

func main() {
	in, err := day16.ParseInput("../../assets/day16.txt")
	if err != nil {
		fmt.Printf("Error: %s\n", err)
		os.Exit(1)
	}
	fmt.Printf("Error Rate: %d\n", day16.ErrorRate(in))
	fmt.Printf("Your Departure Product: %d\n", day16.YourDepartureProduct(in))
}
