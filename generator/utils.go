package generator

import (
	"github.com/dave/jennifer/jen"
	"strings"
)

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
	if strings.HasPrefix(t.Name, "__") {
		return false
	}
	return true
}

func zeroValue(typeName string) jen.Code {
	switch typeName {
	case "int", "int64", "float64":
		return jen.Lit(0)
	case "string":
		return jen.Lit("")
	case "bool":
		return jen.Lit(false)
	}
	if strings.HasPrefix(typeName, "[]") {
		return jen.Nil()
	}
	if strings.HasPrefix(typeName, "*") {
		return jen.Nil()
	}
	if strings.Contains(typeName, "interface") {
		return jen.Nil()
	}
	return jen.Parens(jen.Id(typeName)).Block()
}

func basicGraphqlTypeToGoType(conf *GenerateConfig, gql string) *jen.Statement {
	cd := &jen.Statement{}

	if g, ok := conf.ScalarMap[gql]; ok {
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
	case "SCALAR", "ENUM":
		return true
	case "NON_NULL", "LIST":
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
	case "LIST":
		return true
	case "NON_NULL":
		if t.OfType != nil {
			return isGraphqlCompoundType(*t.OfType)
		}
	}
	return false
}

func extractGoType(conf *GenerateConfig, t GQLType, sta *jen.Statement) {
	switch t.Kind {
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
}

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
