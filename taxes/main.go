package main

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	pdfcpu "github.com/pdfcpu/pdfcpu/pkg/api"
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

func checkFiles(confPath string, conf *Conf) ([]string, error) {
	var rErr error
	r := make([]string, 0)
	fileMap := map[string]bool{confPath: true, conf.Output: true}
	dir := filepath.Dir(confPath)
	for _, in := range conf.Inputs {
		matches, err := filepath.Glob(filepath.Join(dir, in))
		if err != nil {
			return nil, err
		}
		if len(matches) == 0 {
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
	conf, err := loadConf(confPath)
	if err != nil {
		return err
	}
	files, err := checkFiles(confPath, conf)
	if err != nil {
		return err
	}
	out := filepath.Join(filepath.Dir(confPath), conf.Output)
	return pdfcpu.MergeCreateFile(files, out, nil)
}

func main() {
	if err := run(os.Args[1]); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
