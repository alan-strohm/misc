package day21

import (
	"flag"
	"fmt"
	"regexp"
	"sort"
	"strings"

	"../io"
)

var dbgFlag = flag.Bool("dbg", false, "Print debug info.")

func dbg(in string) {
	if *dbgFlag {
		fmt.Println(in)
	}
}

type item struct {
	id          int
	ingredients []string
	allergens   []string
}

var itemRE = regexp.MustCompile(`^(.*) \(contains (.*)\)$`)

func parseLine(l string) (*item, error) {
	matches := itemRE.FindStringSubmatch(l)
	if len(matches) != 3 {
		return nil, fmt.Errorf("malformed item line: %s", l)
	}
	ingStr, allStr := matches[1], matches[2]
	r := &item{ingredients: strings.Split(ingStr, " "), allergens: strings.Split(allStr, ", ")}
	sort.Strings(r.ingredients)
	sort.Strings(r.allergens)
	return r, nil
}

type items []*item

func (is *items) ScanLine(l string) (done bool, err error) {
	i, err := parseLine(l)
	if err != nil {
		return
	}
	i.id = len(*is)
	*is = append(*is, i)
	return
}

func readItems(fname string) ([]*item, error) {
	is := &items{}
	if err := io.Scan(is, fname); err != nil {
		return nil, err
	}
	return []*item(*is), nil
}

type itemIndex struct {
	ingIndex, allIndex map[string][]*item

	allergenToIng map[string]string
	ingToAllergen map[string]string
}

func insertItem(m map[string][]*item, k string, i *item) {
	if _, ok := m[k]; !ok {
		m[k] = make([]*item, 0)
	}
	m[k] = append(m[k], i)
}

func indexItems(items []*item) *itemIndex {
	r := &itemIndex{
		ingIndex:      make(map[string][]*item),
		allIndex:      make(map[string][]*item),
		allergenToIng: make(map[string]string),
		ingToAllergen: make(map[string]string),
	}
	for _, i := range items {
		for _, ing := range i.ingredients {
			insertItem(r.ingIndex, ing, i)
		}
		for _, all := range i.allergens {
			insertItem(r.allIndex, all, i)
		}
	}
	dbg(fmt.Sprintf("%d allergens, %d ingredients", len(r.allIndex), len(r.ingIndex)))
	return r
}

func intersectStrings(lists ...[]string) []string {
	if len(lists) == 1 {
		return lists[0]
	}
	if len(lists) == 2 {
		r := make([]string, 0)
		a, b := lists[0], lists[1]
		for i, j := 0, 0; i < len(a) && j < len(b); {
			ai, bj := a[i], b[j]
			if ai == bj {
				r = append(r, ai)
				i++
				j++
				continue
			}
			if ai < bj {
				i++
			}
			if bj < ai {
				j++
			}
		}
		return r
	}
	r := intersectStrings(lists[0], lists[1])
	for i := 2; i < len(lists); i++ {
		r = intersectStrings(r, lists[i])
	}
	return r
}

func filterFound(keys []string, idx map[string]string) []string {
	r := make([]string, 0, len(keys))
	for _, k := range keys {
		if _, ok := idx[k]; !ok {
			r = append(r, k)
		}
	}
	return r
}

func (idx *itemIndex) recordFound(allergen, ingredient string) {
	idx.ingToAllergen[ingredient] = allergen
	idx.allergenToIng[allergen] = ingredient
	dbg(fmt.Sprintf("found %s -> %s", allergen, ingredient))
}

func (idx *itemIndex) maybeIdentify(all string, items []*item, round int) (newlyFound, alreadyFound bool) {
	if _, ok := idx.allergenToIng[all]; ok {
		alreadyFound = true
		return
	}
	if len(items) == 1 {
		unfound := filterFound(items[0].ingredients, idx.ingToAllergen)
		if len(unfound) == 1 {
			idx.recordFound(all, unfound[0])
			newlyFound = true
			return
		}
	}

	allIngredients, allAllergens := make([][]string, len(items)), make([][]string, len(items))
	for i, it := range items {
		allIngredients[i] = it.ingredients
		allAllergens[i] = it.allergens
	}
	commonIngredients := filterFound(intersectStrings(allIngredients...), idx.ingToAllergen)
	commonAllergens := filterFound(intersectStrings(allAllergens...), idx.allergenToIng)
	if len(commonIngredients) == 1 && len(commonAllergens) == 1 {
		idx.recordFound(commonAllergens[0], commonIngredients[0])
		newlyFound = true
		return
	}
	return
}

// Do one round of allergen identification.  Return the number of allergens
// identified and whether we finished identifying all alergens
func (idx *itemIndex) identRound(round int) (int, bool) {
	totalFound := 0
	newlyFound := 0
	for all, items := range idx.allIndex {
		wasNewlyFound, wasAlreadyFound := idx.maybeIdentify(all, items, round)
		if wasNewlyFound {
			newlyFound++
		}
		if wasAlreadyFound {
			totalFound++
		}
	}
	return newlyFound, totalFound == len(idx.allIndex)
}

func (idx *itemIndex) identifyAll() error {
	for round, newlyFound, done := 0, 1, false; newlyFound > 0 && !done; round++ {
		newlyFound, done = idx.identRound(round)
		if !done && newlyFound == 0 {
			return fmt.Errorf("no progress after %d rounds", round+1)
		}
	}
	return nil
}

func run(fname string) (part1 int, part2 string, err error) {
	items, err := readItems(fname)
	if err != nil {
		return
	}
	idx := indexItems(items)
	if err = idx.identifyAll(); err != nil {
		return
	}
	for ing, items := range idx.ingIndex {
		if _, ok := idx.ingToAllergen[ing]; ok {
			continue
		}
		part1 += len(items)
	}
	allAllergens := make([]string, 0, len(idx.allergenToIng))
	for allergen, _ := range idx.allergenToIng {
		allAllergens = append(allAllergens, allergen)
	}
	sort.Strings(allAllergens)
	dangerousIngredients := make([]string, len(allAllergens))
	for i, allergen := range allAllergens {
		dangerousIngredients[i] = idx.allergenToIng[allergen]
	}
	part2 = strings.Join(dangerousIngredients, ",")
	return
}
