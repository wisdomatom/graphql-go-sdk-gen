package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"github.com/wisdomatom/graphql-go-sdk-gen/generator"
	"os"
)

var (
	useOmitEmp bool
)

func main() {
	conf := &generator.GenerateConfig{
		Schema:    generator.Introspection{},
		OutPath:   "",
		GoPkgName: "",
		ScalarMap: map[string]generator.GoType{},
	}
	flag.Usage = func() {
		fmt.Println("Usage:")
		fmt.Println("  go run gen.go --schema schema.json --out model_gen.go [--scalars scalars.json] [--omit-empty]")
		fmt.Println()
		flag.PrintDefaults()
	}
	schemaPath := flag.String("schema", "", "path to introspection json (required)")
	pkg := flag.String("pkg", "client", "generate golang package name")
	outPath := flag.String("out", "./", "output file")
	scalarsPath := flag.String("scalars", "", "optional scalars.json mapping")
	flag.BoolVar(&useOmitEmp, "omit-empty", false, "append ,omitempty to json tags")
	flag.Parse()

	conf.OutPath = *outPath
	conf.JsonOmitEmpty = useOmitEmp
	conf.GoPkgName = *pkg

	if *schemaPath == "" {
		flag.Usage()
		os.Exit(1)
	} else {
		bts, err := os.ReadFile(*schemaPath)
		if err != nil {
			os.Exit(1)
		}
		err = json.Unmarshal(bts, &conf.Schema)
		if err != nil {
			os.Exit(1)
		}
	}

	if *scalarsPath != "" {
		bts, err := os.ReadFile(*scalarsPath)
		if err != nil {
			os.Exit(1)
		}
		err = json.Unmarshal(bts, &conf.ScalarMap)
		if err != nil {
			os.Exit(1)
		}
	}

	if err := generator.Generate(conf); err != nil {
		generator.Log.Error(err)
		os.Exit(2)
	}
	fmt.Println("✅ generated:", *outPath)
}
