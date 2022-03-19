package main

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	"gopkg.in/yaml.v2"
)

type Conf struct {
	Inputs []string `yaml:"inputs"`
	Output string   `yaml:"output"`
}

func loadConf(confPath string) (*Conf, error) {
	f, err := os.Open(confPath)
	if err != nil {
		return nil, err
	}
	c := &Conf{}
	return c, yaml.NewDecoder(f).Decode(c)
}

var checkErr = errors.New("missing or extra inputs")

func checkFiles(dir string, inputs []string) ([]string, error) {
	var rErr error
	r := make([]string, 0)
	fileMap := make(map[string]bool)
	for _, in := range inputs {
		matches, err := filepath.Glob(filepath.Join(dir, in))
		if err != nil {
			return nil, err
		}
		if len(matches) > 0 {
			fmt.Printf("%s: no matching files\n", in)
			rErr = checkErr
		}
		for _, match := range matches {
			fileMap[match] = true
			r = append(r, match)
		}
	}

	filepath.WalkDir(dir,
		func(path string, d fs.DirEntry, err error) error {
			if d.IsDir() {
				return nil
			}
			if !fileMap[path] {
				fmt.Printf("extra file: %s\n", path)
				rErr = checkErr
			}
			return nil
		})
	return r, rErr
}

func run(confPath string) error {
	c, err := loadConf(confPath)
	if err != nil {
		return err
	}
	_, err = checkFiles(filepath.Dir(confPath), c.Inputs)
	if err != nil {
		return err
	}
	return nil
}

func main() {
	if err := run(os.Args[1]); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
