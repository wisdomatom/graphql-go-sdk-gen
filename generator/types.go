package generator

type OperationType string

const (
	OperationTypeQuery    OperationType = "query"
	OperationTypeMutation OperationType = "mutation"
)

type Kind string

const (
	KindEnum        Kind = "ENUM"
	KindObject      Kind = "OBJECT"
	KindInterface   Kind = "INTERFACE"
	KindInputObject Kind = "INPUT_OBJECT"
)

func (r OperationType) String() string {
	return string(r)
}

func (r Kind) String() string {
	return string(r)
}

type Introspection struct {
	Data struct {
		Schema struct {
			Types []TypeDef `json:"types"`
		} `json:"__schema"`
	} `json:"data"`
}

type TypeDef struct {
	Kind        string      `json:"kind"`
	Name        string      `json:"name"`
	Fields      []FieldDef  `json:"fields"`
	InputFields []FieldDef  `json:"inputFields"`
	EnumValues  []EnumValue `json:"enumValues"`
	Interfaces  []NamedType `json:"interfaces"`
}

type FieldDef struct {
	Name string   `json:"name"`
	Type GQLType  `json:"type"`
	Args []ArgDef `json:"args"`
}

type ArgDef struct {
	Name string  `json:"name"`
	Type GQLType `json:"type"`
}

type EnumValue struct {
	Name string `json:"name"`
}

type NamedType struct {
	Name string `json:"name"`
}

type GQLType struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name"`
	OfType *GQLType `json:"ofType"`
}

// GoType used to map GraphQL scalar to Go type info
type GoType struct {
	Type string `json:"type"` // type name (e.g. "Time", "Decimal", or "string")
	Pkg  string `json:"pkg"`  // package path (e.g. "time", "github.com/shopspring/decimal")
	// Note: If you supply "time.Time" in scalars.json, loader will split pkg/time automatically
	IsList   bool `json:"is_list"`
	IsObject bool `json:"is_object"`
	IsPtr    bool `json:"is_ptr"`
}

type GenerateConfig struct {
	Schema        Introspection     `json:"schema"`
	OutPath       string            `json:"out_path"`
	GoPkgName     string            `json:"go_pkg_name"`
	ScalarMap     map[string]GoType `json:"scalar_map"`
	JsonOmitEmpty bool              `json:"json_omit_empty"`
}
