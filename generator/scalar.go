package generator

import (
	_ "embed"
	"encoding/json"
)

var (
	//go:embed scalar.json
	scalarFile       string
	defaultScalarMap = map[string]GoType{}
)

func init() {
	err := json.Unmarshal([]byte(scalarFile), &defaultScalarMap)
	if err != nil {
		panic(err)
	}
}
