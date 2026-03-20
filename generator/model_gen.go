package generator

import (
	"fmt"
	"github.com/dave/jennifer/jen"
	"sort"
)

func toExported(name string) string {
	return safeFieldName(name)
}

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

	enumType := fmt.Sprintf("%vField", t.Name)
	typeDefEnum := TypeDef{
		Name:       enumType,
		EnumValues: []EnumValue{},
	}
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
	sort.Slice(t.Fields, func(i, j int) bool {
		return t.Fields[i].Name < t.Fields[j].Name
	})
	fields := buildFields(conf, KindObject, t.Name, t.Fields)
	sort.Slice(t.Interfaces, func(i, j int) bool {
		return t.Interfaces[i].Name < t.Interfaces[j].Name
	})
	for _, iface := range t.Interfaces {
		fields = append([]jen.Code{jen.Id(iface.Name)}, fields...)
	}
	f.Commentf("%s object", t.Name)
	f.Type().Id(t.Name).Struct(fields...)

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

	f.Func().Params(jen.Id("q").Id(enumType)).Id("GetField").Params().Op("*").Id("Field").Block(
		jen.Return(
			jen.Op("&").Id("Field").Values(
				jen.Dict{
					jen.Id("Name"): jen.Id("string").Call(jen.Id("q")),
				},
			),
		),
	)

	for _, iface := range t.Interfaces {
		f.Func().Params(jen.Id(t.Name)).Id("Is" + iface.Name).Params().Block()
	}
	f.Line()
}

func buildFields(conf *GenerateConfig, kind Kind, structName string, defs []FieldDef) []jen.Code {
	var out []jen.Code
	for _, d := range defs {
		sta := &jen.Statement{}

		typeName, _ := extractGraphqlTypeName(d.Type)
		isSelfReference := structName == typeName
		isCompound := isGraphqlCompoundType(d.Type)
		isScalar := isGraphqlScalarType(d.Type)

		// We only add a pointer if:
		// 1. It is NOT a compound type (LIST already acts as a reference)
		// 2. AND (it's a self-reference OR it's a non-scalar object OR it's an InputObject field for nullability)
		if !isCompound {
			if isSelfReference || !isScalar || kind == KindInputObject {
				sta.Op("*")
			}
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
