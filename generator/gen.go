// gen.go
package generator

import (
	"bytes"
	"fmt"
	"github.com/dave/jennifer/jen"
	"os"
	"path"
	"sort"
	"strings"
)

/*
A GraphQL introspection -> Go model + type-safe DSL generator (v9.1)
- Reads introspection JSON
- Emits model types (OBJECT, INPUT_OBJECT, ENUM, INTERFACE)
- Emits selectors for each object type: <Type>Selector + <Type>Select()
- Emits operation builders for Query/Mutation root fields:
  * constructor: <OpName>()
  * param setters based on args (names taken from introspection)
  * type-safe Select(...) (string + child selectors)
  * Build() that outputs "query($var: Type, ...){...}" with vars numbered like type_1, type_2...
  * Do(endpoint, token) that executes request, unmarshals into typed response and returns error if response.errors non-empty
- Supports scalar mapping via scalars.json and --omit-empty
*/

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

// ---------- scalar loader ----------
func loadScalars(conf *GenerateConfig) {
	if len(conf.ScalarMap) == 0 {
		conf.ScalarMap = defaultScalarMap
	}
}

func isSelectorType(t TypeDef) bool {
	if t.Name == "Mutation" && t.Kind == KindObject.String() {
		return false
	}
	if t.Name == "Query" && t.Kind == KindObject.String() {
		return false
	}
	if t.Kind == KindInputObject.String() {
		return false
	}
	if t.Kind == KindEnum.String() {
		return false
	}
	if t.Kind == "SCALAR" {
		return false
	}
	if t.Name == "__Directive" {
		return false
	}
	if t.Name == "__Type" {
		return false
	}
	if t.Name == "__InputValue" {
		return false
	}
	if t.Name == "__EnumValue" {
		return false
	}
	if t.Name == "__Field" {
		return false
	}
	if t.Name == "__Schema" {
		return false
	}
	return true
}

// ---------- Generate ----------
func Generate(conf *GenerateConfig) error {

	loadScalars(conf)

	var intros = conf.Schema
	// find Query/Mutation root
	var queryRoot *TypeDef
	var mutationRoot *TypeDef
	var typeSelectors []TypeDef
	var typeEnum []TypeDef
	var typeInterface []TypeDef
	var typeInputObject []TypeDef
	var typeObject []TypeDef
	// build type map & object name list
	typeMap := map[string]TypeDef{}
	objectMap := map[string]struct{}{}
	for _, t := range intros.Data.Schema.Types {
		typeMap[t.Name] = t
		if t.Kind == "OBJECT" && !strings.HasPrefix(t.Name, "__") {
			objectMap[t.Name] = struct{}{}
		}

		if t.Name == "Query" {
			tr := t
			queryRoot = &tr
		}
		if t.Name == "Mutation" {
			tr := t
			mutationRoot = &tr
		}

		if isSelectorType(t) {
			typeSelectors = append(typeSelectors, t)
		}

		exclude := strings.HasPrefix(t.Name, "__") || t.Name == ""
		if t.Kind == KindEnum.String() && !exclude {
			typeEnum = append(typeEnum, t)
		}
		if t.Kind == KindInterface.String() && !exclude {
			typeInterface = append(typeInterface, t)
		}
		if t.Kind == KindInputObject.String() && !exclude {
			typeInputObject = append(typeInputObject, t)
		}
		if t.Kind == KindObject.String() && !exclude {
			typeObject = append(typeObject, t)
		}
	}

	comment := "Code generated. DO NOT EDIT."

	f := jen.NewFile(conf.GoPkgName)
	f.HeaderComment(comment)
	fSelector := jen.NewFile(conf.GoPkgName)
	fSelector.HeaderComment(comment)
	fClient := jen.NewFile(conf.GoPkgName)
	fClient.HeaderComment(comment)

	// generate types
	sort.Slice(typeEnum, func(i, j int) bool {
		return typeEnum[i].Name < typeEnum[j].Name
	})
	sort.Slice(typeInterface, func(i, j int) bool {
		return typeInterface[i].Name < typeInterface[j].Name
	})
	sort.Slice(typeInputObject, func(i, j int) bool {
		return typeInputObject[i].Name < typeInputObject[j].Name
	})
	sort.Slice(typeObject, func(i, j int) bool {
		return typeObject[i].Name < typeObject[j].Name
	})
	sort.Slice(typeSelectors, func(i, j int) bool {
		return typeSelectors[i].Name < typeSelectors[j].Name
	})

	for _, t := range typeEnum {
		genEnum(f, t)
	}
	for _, t := range typeInterface {
		genInterface(conf, f, t)
	}
	for _, t := range typeInputObject {
		genInput(conf, f, t)
	}
	for _, t := range typeObject {
		genObject(conf, f, t)
	}

	// Field AST helpers
	genFieldHelpers(f)

	// generate selectors for object types
	for _, t := range typeSelectors {
		genSelector(conf, fSelector, t, objectMap)
	}

	genGraphQLError(fClient)
	genClient(fClient)
	genDo(fClient)
	genNewField(fClient)
	genBuildFunc(fClient)
	genPtrFunc(fClient)

	if queryRoot != nil {
		genOperations(conf, fClient, *queryRoot, "Query", typeMap, objectMap)
	}
	if mutationRoot != nil {
		genOperations(conf, fClient, *mutationRoot, "Mutation", typeMap, objectMap)
	}

	// 正确的方式：使用 Render
	var buf bytes.Buffer
	err := f.Render(&buf)
	if err != nil {
		return err
	}
	err = os.WriteFile(path.Join(conf.OutPath, "model.go"), buf.Bytes(), 0644)
	if err != nil {
		return err
	}
	buf.Reset()

	err = fSelector.Render(&buf)
	if err != nil {
		return err
	}
	err = os.WriteFile(path.Join(conf.OutPath, "selector.go"), buf.Bytes(), 0644)
	if err != nil {
		return err
	}
	buf.Reset()

	err = fClient.Render(&buf)
	if err != nil {
		return err
	}
	err = os.WriteFile(path.Join(conf.OutPath, "client.go"), buf.Bytes(), 0644)
	if err != nil {
		return err
	}
	buf.Reset()

	return nil
}

func genNewField(f *jen.File) {
	f.Func().Id("newField").
		Params(jen.Id("name").Id("string")).
		Params(jen.Op("*").Id("Field")).Block(
		jen.Return(
			jen.Op("&").Id("Field").Values(
				jen.Dict{
					jen.Id("Name"):     jen.Id("name"),
					jen.Id("Args"):     jen.Make(jen.Map(jen.String()).Op("*").Id("FieldArg")),
					jen.Id("Children"): jen.Index().Op("*").Id("Field").Values(),
				},
			),
		),
	)
}

func genDo(f *jen.File) {
	f.Func().Params(jen.Id("c").Op("*").Id("Client")).Id("Do").
		Params(jen.Id("ctx").Qual("context", "Context"), jen.Id("query").String(), jen.Id("vars").Map(jen.String()).Interface(), jen.Id("resp").Interface()).
		Params(jen.Error()).Block(
		jen.Id("bts").Op(",").Id("err").Op(":=").Id("doGraphQLRequest").Call(jen.Id("ctx"), jen.Id("c").Dot("HTTPClient"), jen.Id("c").Dot("Endpoint"), jen.Id("query"), jen.Id("vars")),
		jen.If(jen.Id("bts").Op("==").Nil()).Block(
			jen.Return(jen.Id("fmt").Dot("Errorf").Call(jen.Lit("doGraphQLRequest failed: %w"), jen.Id("err"))),
		),
		jen.Id("err").Op("=").Qual("encoding/json", "Unmarshal").Call(jen.Id("bts"), jen.Op("&").Id("resp")),
		jen.Return(jen.Id("err")),
	)
}

func genGraphQLError(f *jen.File) {
	f.Type().Id("GraphqlError").Struct(
		jen.Id("Message").Id("string").Tag(map[string]string{"json": "message"}),
		jen.Id("Locations").Index().Struct(
			jen.Id("Line").Id("int").Tag(map[string]string{"json": "line"}),
			jen.Id("Column").Id("int").Tag(map[string]string{"json": "column"}),
		).Tag(map[string]string{"json": "locations"}),
	)
	f.Func().Id("hasError").
		Params(jen.Id("err").Index().Id("GraphqlError")).
		Params(jen.Id("error")).
		Block(
			jen.If(jen.Id("len").Call(jen.Id("err")).Op(">").Lit(0)).Block(
				jen.Return(jen.Id("fmt").Dot("Errorf").Call(jen.Lit("graphql errors: %v"), jen.Id("err"))),
			),
			jen.Return(jen.Nil()),
		)
}

func genClient(f *jen.File) {
	f.Comment("Client is the default client for GraphQL.")
	f.Type().Id("Client").Struct(
		jen.Id("Endpoint").String(),
		jen.Id("HTTPClient").Op("*").Qual("net/http", "Client"),
	)
	f.Line()
	f.Comment("NewClient creates a new GraphQL client.")
	f.Func().Id("NewClient").
		Params(
			jen.Id("endpoint").String(),
			jen.Id("httpClient").Op("*").Qual("net/http", "Client"),
		).
		Op("*").Id("Client").
		Block(
			jen.If(jen.Id("httpClient").Op("==").Nil()).Block(
				jen.Id("httpClient").Op("=").Qual("net/http", "DefaultClient"),
			),
			jen.Return(jen.Op("&").Id("Client").Values(
				jen.Dict{
					jen.Id("Endpoint"):   jen.Id("endpoint"),
					jen.Id("HTTPClient"): jen.Id("httpClient"),
				},
			)),
		)
}

func genPtrFunc(f *jen.File) {
	// 生成 Ptr 泛型函数
	f.Func().Id("Ptr").Types(jen.Id("T").Any()).
		Params(jen.Id("v").Id("T")).
		Op("*").Id("T").
		Block(
			jen.Return(jen.Op("&").Id("v")),
		)
}

func genBuildFunc(f *jen.File) {
	f.Func().Id("build").
		Params(jen.Id("field").Op("*").Id("Field"), jen.Id("kind").Id("string")).
		Params(jen.String(), jen.Map(jen.String()).Interface()).Block(
		jen.Id("vars").Op(":=").Map(jen.String()).Interface().Values(),
		jen.Id("varTypes").Op(":=").Map(jen.String()).String().Values(),
		jen.Id("nameMap").Op(":=").Map(jen.String()).String().Values(),
		jen.Id("counter").Op(":=").Lit(0),
		// collect vars
		jen.Id("fieldCollectVars").Call(jen.Id("field"), jen.Id("vars"), jen.Id("varTypes"), jen.Id("nameMap"), jen.Op("&").Id("counter")),
		// build ordered decls: use stable order by sorting keys (but we might prefer appearance order later)
		jen.Id("keys").Op(":=").Make(jen.Index().String(), jen.Len(jen.Id("varTypes"))),
		jen.Id("i").Op(":=").Lit(0),
		jen.For(jen.Id("k").Op(",").Op("_").Op(":=").Range().Op(jen.Id("varTypes").GoString())).Block(
			jen.Id("keys").Index(jen.Id("i")).Op("=").Id("k"),
			jen.Id("i").Op("++"),
		),
		jen.Qual("sort", "Strings").Call(jen.Id("keys")),
		jen.Id("parts").Op(":=").Index().Op(jen.String().GoString()).Values(),
		jen.For(jen.Id("_").Op(",").Id("k").Op(":=").Range().Op(jen.Id("keys").GoString())).Block(
			jen.Id("parts").Op("=").Append(jen.Id("parts"), jen.Qual("fmt", "Sprintf").Call(jen.Lit("$%s: %s"), jen.Id("k"), jen.Id("varTypes").Index(jen.Id("k")))),
		),
		jen.Id("decl").Op(":=").Qual("strings", "Join").Call(jen.Id("parts"), jen.Lit(", ")),
		jen.If(jen.Id("decl").Op("!=").Lit("")).Block(jen.Id("decl").Op("=").Qual("fmt", "Sprintf").Call(jen.Lit("(%s)"), jen.Id("decl"))),
		jen.Id("body").Op(":=").Qual("strings", "TrimSpace").Call(jen.Id("fieldToGraphQL").Call(jen.Id("field"), jen.Lit("  "), jen.Id("nameMap"))),
		jen.Id("query").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%s%s{\n%s\n}"), jen.Id("strings").Dot("ToLower").Call(jen.Id("kind")), jen.Id("decl"), jen.Id("body")),
		jen.Return(jen.Id("query"), jen.Id("vars")),
	)
}

// ================= helpers for base types =================
func genEnum(f *jen.File, t TypeDef) {
	if len(t.EnumValues) == 0 {
		return
	}
	f.Commentf("%s is enum", t.Name)
	f.Type().Id(t.Name).String()
	f.Line()
	f.Const().DefsFunc(func(g *jen.Group) {
		sort.Slice(t.EnumValues, func(i, j int) bool {
			return t.EnumValues[i].Name < t.EnumValues[j].Name
		})
		for _, ev := range t.EnumValues {
			g.Id(fmt.Sprintf("%s%s", t.Name, toExported(ev.Name))).Id(t.Name).Op("=").Lit(ev.Name)
		}
	})
	f.Line()
}

func genInterface(conf *GenerateConfig, f *jen.File, t TypeDef) {
	sort.Slice(t.Fields, func(i, j int) bool {
		return t.Fields[i].Name < t.Fields[j].Name
	})
	fields := buildFields(conf, KindInterface, t.Name, t.Fields)
	f.Commentf("%s interface base", t.Name)
	f.Type().Id(t.Name).Struct(fields...)
	f.Line()
	f.Type().Id(t.Name + "Type").Interface(jen.Id("Is" + t.Name + "()"))

	// gen struct field enum
	enumType := fmt.Sprintf("%vField", t.Name)
	typeDefEnum := TypeDef{
		Name:       enumType,
		EnumValues: []EnumValue{},
	}
	// sort fields
	sort.Slice(t.Fields, func(i, j int) bool {
		return t.Fields[i].Name < t.Fields[j].Name
	})
	for _, tf := range t.Fields {
		if !isGraphqlScalarType(tf.Type) {
			continue
		}
		typeDefEnum.EnumValues = append(typeDefEnum.EnumValues, EnumValue{
			Name: tf.Name,
		})
	}
	genEnum(f, typeDefEnum)

	//func (r UserField) GetField() *Field {
	//	return &Field{Name: string(r)}
	//}
	f.Func().Params(jen.Id("q").Id(enumType)).Id("GetField").Params().Op("*").Id("Field").Block(
		jen.Return(
			jen.Op("&").Id("Field").Values(
				jen.Dict{
					jen.Id("Name"): jen.Id("string").Call(jen.Id("q")),
				},
			),
		),
	)

	f.Line()
}

func genInput(conf *GenerateConfig, f *jen.File, t TypeDef) {
	// sort fields
	sort.Slice(t.InputFields, func(i, j int) bool {
		return t.InputFields[i].Name < t.InputFields[j].Name
	})
	fields := buildFields(conf, KindInputObject, t.Name, t.InputFields)
	f.Commentf("%s input", t.Name)
	f.Type().Id(t.Name).Struct(fields...)
	f.Line()
}

func genObject(conf *GenerateConfig, f *jen.File, t TypeDef) {
	if len(t.Fields) == 0 {
		return
	}
	// sort fields
	sort.Slice(t.Fields, func(i, j int) bool {
		return t.Fields[i].Name < t.Fields[j].Name
	})
	fields := buildFields(conf, KindObject, t.Name, t.Fields)
	// embed interfaces
	sort.Slice(t.Interfaces, func(i, j int) bool {
		return t.Interfaces[i].Name < t.Interfaces[j].Name
	})
	for _, iface := range t.Interfaces {
		fields = append([]jen.Code{jen.Id(iface.Name)}, fields...)
	}
	f.Commentf("%s object", t.Name)
	f.Type().Id(t.Name).Struct(fields...)

	// gen struct field enum
	enumType := fmt.Sprintf("%vField", t.Name)
	typeDefEnum := TypeDef{
		Name:       enumType,
		EnumValues: []EnumValue{},
	}
	for _, tf := range t.Fields {
		if !isGraphqlScalarType(tf.Type) {
			continue
		}
		typeDefEnum.EnumValues = append(typeDefEnum.EnumValues, EnumValue{
			Name: tf.Name,
		})
	}
	genEnum(f, typeDefEnum)

	//func (r UserField) GetField() *Field {
	//	return &Field{Name: string(r)}
	//}
	f.Func().Params(jen.Id("q").Id(enumType)).Id("GetField").Params().Op("*").Id("Field").Block(
		jen.Return(
			jen.Op("&").Id("Field").Values(
				jen.Dict{
					jen.Id("Name"): jen.Id("string").Call(jen.Id("q")),
				},
			),
		),
	)

	// implement marker methods for interfaces
	for _, iface := range t.Interfaces {
		f.Func().Params(jen.Id(t.Name)).Id("Is" + iface.Name).Params().Block()
	}
	f.Line()
}

// buildFields: create jen.Code slice for struct fields
func buildFields(conf *GenerateConfig, kind Kind, structName string, defs []FieldDef) []jen.Code {
	var out []jen.Code
	for _, d := range defs {
		sta := &jen.Statement{}
		if structName == sta.GoString() ||
			(!isGraphqlScalarType(d.Type) && !isGraphqlCompoundType(d.Type)) ||
			(kind == KindInputObject && !isGraphqlCompoundType(d.Type)) {
			sta.Op("*")
		}
		extractGoType(conf, d.Type, sta)
		tag := d.Name
		if conf.JsonOmitEmpty {
			tag = tag + ",omitempty"
		}
		out = append(out, jen.Id(safeFieldName(d.Name)).Add(sta).Tag(map[string]string{"json": tag}))
	}
	return out
}

// ================= Field AST helpers =================
func genFieldHelpers(f *jen.File) {
	// FieldArg Struct
	f.Type().Id("FieldArg").Struct(
		jen.Id("Arg").Interface(),
		jen.Id("ArgType").String(),
	)
	// Field struct
	f.Comment("Field is a selection node")
	f.Type().Id("Field").Struct(
		jen.Id("Name").String(),
		jen.Id("Args").Map(jen.String()).Op("*").Id("FieldArg"),
		jen.Id("Children").Index().Op("*").Id("Field"),
	)
	f.Line()

	// fieldCollectVars: collect variables, varTypes and nameMap using counter
	// func fieldCollectVars(f *Field, vars map[string]interface{}, varTypes map[string]string, nameMap map[string]string, counter *int)
	f.Func().Id("fieldCollectVars").Params(
		jen.Id("f").Op("*").Id("Field"),
		jen.Id("vars").Map(jen.String()).Interface(),
		jen.Id("varTypes").Map(jen.String()).String(),
		jen.Id("nameMap").Map(jen.String()).String(),
		jen.Id("counter").Op("*").Int(),
	).Block(
		jen.If(jen.Id("f").Op("==").Nil()).Block(jen.Return()),
		jen.For(jen.Id("k").Op(",").Id("v").Op(":=").Range().Op(jen.Id("f").Dot("Args").GoString())).Block(
			jen.If(jen.Id("v").Op("==").Nil()).Block(jen.Continue()),

			jen.Op("*").Id("counter").Op("++"),
			// varName like "userwhere_1"
			jen.Id("varName").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%s_%d"), jen.Qual("strings", "ToLower").Call(jen.Id("k")), jen.Id("*counter")),
			jen.Id("nameMap").Index(jen.Id("f").Dot("Name").Op("+").Lit(".").Op("+").Id("k")).Op("=").Id("varName"),
			jen.Id("varTypes").Index(jen.Id("varName")).Op("=").Id("v").Dot("ArgType"),
			jen.Id("vars").Index(jen.Id("varName")).Op("=").Id("v").Dot("Arg"),
		),
		jen.For(jen.Id("_").Op(",").Id("c").Op(":=").Range().Op(jen.Id("f").Dot("Children").GoString())).Block(
			jen.Id("fieldCollectVars").Call(jen.Id("c"), jen.Id("vars"), jen.Id("varTypes"), jen.Id("nameMap"), jen.Id("counter")),
		),
	)
	f.Line()

	// fieldToGraphQL(f *Field, indent string, nameMap map[string]string) string
	f.Func().Id("fieldToGraphQL").Params(jen.Id("f").Op("*").Id("Field"), jen.Id("indent").String(), jen.Id("nameMap").Map(jen.String()).String()).String().Block(
		jen.If(jen.Id("f").Op("==").Nil()).Block(jen.Return(jen.Lit(""))),
		jen.Id("var").Id("b").Qual("strings", "Builder"),
		jen.Id("b").Dot("WriteString").Call(jen.Id("indent").Op("+").Id("f").Dot("Name")),
		jen.If(jen.Len(jen.Id("f").Dot("Args")).Op(">").Lit(0)).Block(
			jen.Id("b").Dot("WriteString").Call(jen.Lit("(")),
			jen.Id("i").Op(":=").Lit(0),
			jen.For(jen.Id("k").Op(",").Op("_").Op(":=").Range().Op(jen.Id("f").Dot("Args").GoString())).Block(
				jen.If(jen.Id("i").Op(">").Lit(0)).Block(jen.Id("b").Dot("WriteString").Call(jen.Lit(", "))),
				jen.Id("varName").Op(":=").Id("nameMap").Index(jen.Id("f").Dot("Name").Op("+").Lit(".").Op("+").Id("k")),
				jen.If(jen.Id("varName").Op("==").Lit("")).Block(jen.Id("varName").Op("=").Id("f").Dot("Name").Op("+").Lit("_").Op("+").Id("k")),
				jen.Id("b").Dot("WriteString").Call(jen.Id("k").Op("+").Lit(":$").Op("+").Id("varName")),
				jen.Id("i").Op("++"),
			),
			jen.Id("b").Dot("WriteString").Call(jen.Lit(")")),
		),
		jen.If(jen.Len(jen.Id("f").Dot("Children")).Op("==").Lit(0)).Block(
			jen.Id("b").Dot("WriteString").Call(jen.Lit("\n")),
			jen.Return(jen.Id("b").Dot("String").Call()),
		),
		jen.Id("b").Dot("WriteString").Call(jen.Lit(" {\n")),
		jen.For(jen.Id("_").Op(",").Id("c").Op(":=").Range().Op(jen.Id("f").Dot("Children").GoString())).Block(
			jen.Id("b").Dot("WriteString").Call(jen.Id("fieldToGraphQL").Call(jen.Id("c"), jen.Id("indent").Op("+").Lit("  "), jen.Id("nameMap"))),
		),
		jen.Id("b").Dot("WriteString").Call(jen.Id("indent").Op("+").Lit("}\n")),
		jen.Return(jen.Id("b").Dot("String").Call()),
	)
	f.Line()

	// doGraphQLRequest helper
	f.Func().Id("doGraphQLRequest").Params(jen.Id("ctx").Qual("context", "Context"), jen.Id("client").Op("*").Qual("net/http", "Client"), jen.Id("endpoint").String(), jen.Id("query").String(), jen.Id("variables").Map(jen.String()).Interface()).Params(jen.Index().Byte(), jen.Error()).Block(
		jen.Id("payload").Op(":=").Map(jen.String()).Interface().Values(
			jen.Dict{
				jen.Lit("query"):     jen.Id("query"),
				jen.Lit("variables"): jen.Id("variables"),
			},
		),
		jen.Id("bts").Op(",").Id("err").Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("payload")),
		jen.If(jen.Id("err").Op("!=").Nil()).Block(jen.Return(jen.Nil(), jen.Id("err"))),
		jen.Id("req").Op(",").Id("err").Op(":=").Qual("net/http", "NewRequestWithContext").Call(jen.Id("ctx"), jen.Qual("net/http", "MethodPost"), jen.Id("endpoint"), jen.Qual("bytes", "NewBuffer").Call(jen.Id("bts"))),
		jen.If(jen.Id("err").Op("!=").Nil()).Block(jen.Return(jen.Nil(), jen.Id("err"))),
		jen.Id("req").Dot("Header").Dot("Set").Call(jen.Lit("Content-Type"), jen.Lit("application/json")),
		jen.Id("resp").Op(",").Id("err").Op(":=").Id("client").Dot("Do").Call(jen.Id("req")),
		jen.If(jen.Id("err").Op("!=").Nil()).Block(jen.Return(jen.Nil(), jen.Id("err"))),
		jen.Defer().Id("resp").Dot("Body").Dot("Close").Call(),
		jen.Id("body").Op(",").Id("err").Op(":=").Qual("io", "ReadAll").Call(jen.Id("resp").Dot("Body")),
		jen.If(jen.Id("err").Op("!=").Nil()).Block(jen.Return(jen.Nil(), jen.Id("err"))),
		jen.Return(jen.Id("body"), jen.Nil()),
	)
	f.Line()
}

// ==================== selectors ====================
func genSelector(conf *GenerateConfig, f *jen.File, tp TypeDef, objectMap map[string]struct{}) {
	sel := fmt.Sprintf("Selector%v", tp.Name)
	ctor := fmt.Sprintf("Select%v", tp.Name)
	f.Commentf("%s selector", sel)
	f.Type().Id(sel).Struct(jen.Id("field").Op("*").Id("Field"))
	f.Line()
	// constructor
	f.Func().Id(ctor).Params(jen.Id("parent").Op("string")).Op("*").Id(sel).Block(
		jen.Return(jen.Op("&").Id(sel).Values(
			jen.Dict{
				jen.Id("field"): jen.Op("&").Id("Field").Values(
					jen.Dict{
						jen.Id("Name"):     jen.Id("parent"), // parent will set actual field name on attach
						jen.Id("Args"):     jen.Make(jen.Map(jen.String()).Op("*").Id("FieldArg")),
						jen.Id("Children"): jen.Index().Op("*").Id("Field").Values(),
					},
				),
			},
		)),
	)

	f.Func().
		Params(jen.Id("q").Op("*").Id(sel)).
		Id("Select").
		Params(jen.Id("fields").Op("...").Id(fmt.Sprintf("%vField", tp.Name))).
		Op("*").Id(sel).
		BlockFunc(func(body *jen.Group) {
			body.For(jen.List(jen.Id("_"), jen.Id("f")).Op(":=").Range().Id("fields")).BlockFunc(func(loop *jen.Group) {

				loop.Id("q").Dot("field").Dot("Children").
					Op("=").Append(
					jen.Id("q").Dot("field").Dot("Children"),
					jen.Id("f").Dot("GetField").Call(),
				)
			})
			body.Return(jen.Id("q"))
		})

	for _, child := range tp.Fields {
		tpN, _ := extractGraphqlTypeName(child.Type)
		_, ok := objectMap[tpN]
		if !ok {
			continue
		}
		var selectFnParam []jen.Code
		selName := fmt.Sprintf("Selector%v", tpN)

		for _, childArg := range child.Args {
			var paramType = &jen.Statement{}
			extractGoType(conf, childArg.Type, paramType)
			pt := paramType.GoString()
			selectFnParam = append(selectFnParam, jen.Id(childArg.Name).Id(pt))
		}

		selectFnParam = append(selectFnParam, jen.Id("fn").Func().Params(jen.Id("q").Op("*").Id(selName)))
		f.Func().
			Params(jen.Id("q").Op("*").Id(sel)).
			Id(fmt.Sprintf("Select%s", toExported(child.Name))).
			Params(selectFnParam...).
			Op("*").Id(sel).
			BlockFunc(func(body *jen.Group) {
				body.Id("selector").Op(":=").Id(fmt.Sprintf("Select%s", tpN)).Call(jen.Lit(child.Name))
				for _, childArg := range child.Args {
					body.Id("selector").Dot("field").Dot("Args").Index(jen.Lit(childArg.Name)).Op("=").Op("&").Id("FieldArg").Values(
						jen.Dict{
							jen.Id("Arg"):     jen.Id(childArg.Name),
							jen.Id("ArgType"): jen.Lit(extractGraphqlType(childArg.Type)),
						},
					)
				}
				body.Id("fn").Call(jen.Id("selector"))
				body.Id("q").Dot("field").Dot("Children").
					Op("=").Append(jen.Id("q").Dot("field").Dot("Children"), jen.Id("selector").Dot("GetField").Call())
				body.Return(jen.Id("q"))
			})
	}

	f.Line()
	// GetField to support static Select injection
	f.Func().Params(jen.Id("q").Op("*").Id(sel)).Id("GetField").Params().Op("*").Id("Field").Block(
		jen.Return(jen.Id("q").Dot("field")),
	)
	f.Line()
}

// ==================== operations generation ====================
func genOperations(conf *GenerateConfig, f *jen.File, root TypeDef, kind string, typeMap map[string]TypeDef, objectMap map[string]struct{}) {
	sort.Slice(root.Fields, func(i, j int) bool {
		return root.Fields[i].Name < root.Fields[j].Name
	})
	for _, op := range root.Fields {
		structName := kind + toExported(op.Name) // e.g., QueryUsers -> Query + UsersPascal
		// builder struct
		f.Commentf("%s builder for %s", structName, op.Name)
		f.Type().Id(structName).Struct(
			jen.Id("field").Op("*").Id("Field"),
		)
		f.Line()

		f.Func().Params(jen.Id("q").Op("*").Id(structName)).Id("Kind").Params().Id("string").BlockFunc(func(g *jen.Group) {
			g.Return(jen.Lit(kind))
		})

		f.Line()

		// New constructor
		newName := "New" + structName
		f.Func().Id(newName).Params().Op("*").Id(structName).BlockFunc(
			func(group *jen.Group) {
				group.Id("q").Op(":=").Op("&").Id(structName).Values(
					jen.Dict{
						jen.Id("field"): jen.Id("newField").Call(jen.Lit(op.Name)),
					},
				)
				group.Return(jen.Id("q"))
			},
		)

		sort.Slice(op.Args, func(i, j int) bool {
			return op.Args[i].Name < op.Args[j].Name
		})

		f.Line()

		// generate param setter methods based on op.Args
		sort.Slice(op.Args, func(i, j int) bool {
			return op.Args[i].Name < op.Args[j].Name
		})
		for _, a := range op.Args {
			method := toExported(a.Name)
			sta := &jen.Statement{}
			extractGoType(conf, a.Type, sta)

			f.Func().Params(jen.Id("q").Op("*").Id(structName)).Id(method).
				Params(jen.Id("v").Add(sta)).Op("*").Id(structName).Block(
				jen.Id("q").Dot("field").Dot("Args").Index(jen.Lit(a.Name)).Op("=").Op("&").Id("FieldArg").Values(
					jen.Dict{
						jen.Id("Arg"):     jen.Id("v"),
						jen.Id("ArgType"): jen.Lit(extractGraphqlType(a.Type)),
					},
				),
				jen.Return(jen.Id("q")),
			)
			f.Line()
		}

		// Build Select method with type-safe cases:
		// Determine return type and child object fields to know which selector types to allow
		retTypeName, retIsList := extractGraphqlTypeName(op.Type)
		_ = retIsList
		childFields := []FieldDef{}
		if td, ok := typeMap[retTypeName]; ok {
			childFields = td.Fields
		}
		// Build a set of child object type names present in this return type's immediate fields
		childObjectSet := map[string]struct{}{}
		for _, cf := range childFields {
			n, _ := extractGraphqlTypeName(cf.Type)
			// if n appears in objectNames, it's an object; include
			if _, ok := objectMap[n]; ok {
				childObjectSet[n] = struct{}{}
				break
			}
		}
		childObjectList := []string{}
		for k := range childObjectSet {
			childObjectList = append(childObjectList, k)
		}
		sort.Strings(childObjectList)

		retName, retList := extractGraphqlTypeName(op.Type)
		retType, ok := typeMap[retName]
		if ok && retType.Kind != "SCALAR" {
			// Select implementation
			f.Func().
				Params(jen.Id("q").Op("*").Id(structName)).
				Id("Select").
				Params(jen.Id("fn").Func().Params(jen.Id("s").Op("*").Id(fmt.Sprintf("Selector%v", retName)))).
				Op("*").Id(structName).
				BlockFunc(func(body *jen.Group) {
					body.Id("sel").Op(":=").Id(fmt.Sprintf("Select%v", retName)).Call(jen.Lit(""))
					body.Id("fn").Call(jen.Id("sel"))
					body.Id("q").Dot("field").Dot("Children").
						Op("=").Append(jen.Id("q").Dot("field").Dot("Children"),
						jen.Id("sel").Dot("GetField").Call().Dot("Children").Op("..."))
					body.Return(jen.Id("q"))
				})
		}

		f.Line()

		// Build() -> returns (query string, variables map)
		f.Func().Params(jen.Id("q").Op("*").Id(structName)).Id("Build").
			Params().
			Params(jen.String(), jen.Map(jen.String()).Interface()).
			Block(
				jen.Return(jen.Id("build").Call(jen.Id("q").Dot("field"), jen.Lit(kind))),
			)
		f.Line()

		// Do(endpoint, token) -> typed result, error
		// Build response unmarshal type based on return type
		// decide return signature
		// if list of object: return ([]<retName>, error)
		// if single object: return (*<retName>, error)
		// else return (interface{}, error)

		foundObj := false
		doRet := jen.Interface()
		doResp := jen.Id(toExported(op.Name)).Interface()
		if _, ok = objectMap[retName]; ok {
			foundObj = true
		}
		if foundObj {
			if retList {
				doRet = jen.Index().Id(retName)
				doResp = jen.Id(toExported(op.Name)).Index().Id(retName)
			} else {
				doRet = jen.Op("*").Id(retName)
				doResp = jen.Id(toExported(op.Name)).Op("*").Id(retName)
			}
		}
		if !foundObj {
			goType, ok := conf.ScalarMap[retName]
			if ok {
				if retList {
					if goType.Pkg != "" {
						doRet = jen.Index().Qual(goType.Pkg, goType.Type)
						doResp = jen.Id(toExported(op.Name)).Index().Qual(goType.Pkg, goType.Type)
					} else {
						doRet = jen.Index().Qual(goType.Pkg, goType.Type)
						doResp = jen.Id(toExported(op.Name)).Index().Id(goType.Type)
					}
				} else {
					if goType.Pkg != "" {
						doRet = jen.Qual(goType.Pkg, goType.Type)
						doResp = jen.Id(toExported(op.Name)).Qual(goType.Pkg, goType.Type)
					} else {
						doRet = jen.Id(goType.Type)
						doResp = jen.Id(toExported(op.Name)).Id(goType.Type)
					}
				}
			}
		}

		doResp.Tag(map[string]string{"json": op.Name})
		doRetZero := zeroValue(doRet.GoString())

		// []RetType
		f.Func().Params(jen.Id("q").Op("*").Id(structName)).Id("Do").
			Params(jen.Id("ctx").Qual("context", "Context"), jen.Id("client").Op("*").Id("Client")).
			Params(doRet, jen.Error()).Block(
			jen.Id("query").Op(",").Id("vars").Op(":=").Id("q").Dot("Build").Call(),
			// response type
			jen.Id("var").Id("resp").Struct(
				jen.Id("Data").Struct(doResp).Tag(map[string]string{"json": "data"}),
				jen.Id("Errors").Index().Id("GraphqlError").Tag(map[string]string{"json": "errors"}),
			),
			jen.Id("err").Op(":=").Id("client").Dot("Do").Call(jen.Id("ctx"), jen.Id("query"), jen.Id("vars"), jen.Op("&").Id("resp")),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(jen.Return(doRetZero, jen.Id("err"))),
			jen.Return(jen.Id("resp").Dot("Data").Dot(toExported(op.Name)), jen.Id("hasError").Call(jen.Id("resp").Dot("Errors"))),
		)
		f.Line()
	}
}

func zeroValue(typeName string) jen.Code {
	switch typeName {
	case "string":
		return jen.Lit("")
	case "bool":
		return jen.False()
	case "int", "int8", "int16", "int32", "int64",
		"uint", "uint8", "uint16", "uint32", "uint64",
		"float32", "float64":
		return jen.Lit(0)
	case "error":
		return jen.Nil()
	}

	if strings.HasPrefix(typeName, "*") || strings.HasPrefix(typeName, "[]") ||
		strings.HasPrefix(typeName, "map[") || strings.HasPrefix(typeName, "chan ") {
		return jen.Nil()
	}

	// 对于 struct 或 interface 类型，使用类型字面量零值
	if strings.Contains(typeName, "interface") {
		return jen.Nil()
	}
	return jen.Parens(jen.Id(typeName)).Block()
}

func basicGraphqlTypeToGoType(conf *GenerateConfig, gql string) *jen.Statement {
	// scalar mapping
	cd := &jen.Statement{}

	if g, ok := conf.ScalarMap[gql]; ok {
		//if g.IsPtr {
		//	cd.Op("*")
		//}
		if g.IsList {
			cd.Index()
		}
		if g.Pkg != "" {
			cd.Qual(g.Pkg, g.Type)
		} else {
			cd.Id(g.Type)
		}
		return cd
	}

	// builtin scalars
	switch gql {
	case "Int":
		cd.Id("int")
	case "Float":
		cd.Id("float64")
	case "Boolean":
		cd.Id("bool")
	case "String", "ID":
		cd.Id("string")
	case "DateTime":
		cd.Qual("time", "Time")
	default:
		cd.Id(gql)
	}
	return cd
}

// extract named type name from GQLType (walk down)
func extractGraphqlType(t GQLType) string {
	switch t.Kind {
	case "NON_NULL":
		if t.OfType != nil {
			return extractGraphqlType(*t.OfType) + "!"
		}
	case "LIST":
		if t.OfType != nil {
			return "[" + extractGraphqlType(*t.OfType) + "]"
		}
	default:
		if t.OfType != nil {
			return extractGraphqlType(*t.OfType)
		}
		if t.Name != "" {
			return t.Name
		}
	}
	return ""
}

func isGraphqlScalarType(t GQLType) bool {
	switch t.Kind {
	case "SCALAR":
		return true
	case "NON_NULL":
		if t.OfType != nil {
			return isGraphqlScalarType(*t.OfType)
		}
	case "LIST":
		if t.OfType != nil {
			return isGraphqlScalarType(*t.OfType)
		}
	default:
		if t.OfType != nil {
			return isGraphqlScalarType(*t.OfType)
		}
	}
	return false
}

func isGraphqlCompoundType(t GQLType) bool {
	switch t.Kind {
	//case "NON_NULL":
	//	return true
	case "LIST":
		return true
	default:
		if t.OfType != nil {
			return isGraphqlScalarType(*t.OfType)
		}
	}
	return false
}

func extractGoType(conf *GenerateConfig, t GQLType, sta *jen.Statement) {
	switch t.Kind {
	//case "NON_NULL":
	//	if t.OfType != nil {
	//		sta.Op("*")
	//		extractGoType(conf, *t.OfType, sta)
	//		return
	//	}
	case "LIST":
		if t.OfType != nil {
			sta.Index()
			extractGoType(conf, *t.OfType, sta)
			return
		}
	default:
		if t.OfType != nil {
			extractGoType(conf, *t.OfType, sta)
			return
		}
		if t.Name != "" {
			sta.Add(basicGraphqlTypeToGoType(conf, t.Name))
		}
	}
	return
}

// extract return type name and whether it's list
func extractGraphqlTypeName(t GQLType) (string, bool) {
	switch t.Kind {
	case "LIST":
		if t.OfType != nil {
			name, _ := extractGraphqlTypeName(*t.OfType)
			return name, true
		}
	case "NON_NULL":
		if t.OfType != nil {
			return extractGraphqlTypeName(*t.OfType)
		}
	default:
		return t.Name, false
	}
	return "", false
}

// ---------------- utils ----------------

var goKeywords = map[string]bool{
	"break": true, "default": true, "func": true, "interface": true,
	"select": true, "case": true, "defer": true, "go": true, "map": true,
	"struct": true, "chan": true, "else": true, "goto": true, "package": true,
	"switch": true, "const": true, "fallthrough": true, "if": true, "range": true,
	"type": true, "continue": true, "for": true, "import": true, "return": true,
	"var": true,
}

func safeFieldName(name string) string {
	if name == "" {
		return "Field"
	}
	parts := strings.FieldsFunc(name, func(r rune) bool {
		return !(r >= '0' && r <= '9' || r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z')
	})
	for i := range parts {
		if parts[i] == "" {
			continue
		}
		parts[i] = strings.ToUpper(parts[i][:1]) + parts[i][1:]
	}
	res := strings.Join(parts, "")
	if res == "" {
		res = name
	}
	if res[0] >= '0' && res[0] <= '9' {
		res = "_" + res
	}
	if goKeywords[strings.ToLower(res)] {
		res = res + "_"
	}
	return res
}

func toExported(name string) string {
	if name == "" {
		return ""
	}
	parts := strings.FieldsFunc(name, func(r rune) bool {
		return !(r >= '0' && r <= '9' || r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z')
	})
	for i := range parts {
		if parts[i] == "" {
			continue
		}
		parts[i] = strings.ToUpper(parts[i][:1]) + parts[i][1:]
	}
	res := strings.Join(parts, "")
	if res == "" {
		res = name
	}
	if res[0] >= '0' && res[0] <= '9' {
		res = "_" + res
	}
	if goKeywords[strings.ToLower(res)] {
		res = res + "_"
	}
	return res
}
