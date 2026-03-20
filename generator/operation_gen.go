package generator

import (
	"fmt"
	"github.com/dave/jennifer/jen"
	"sort"
)

func genOperations(conf *GenerateConfig, f *jen.File, root TypeDef, kind string, typeMap map[string]TypeDef, objectMap map[string]struct{}) {
	sort.Slice(root.Fields, func(i, j int) bool {
		return root.Fields[i].Name < root.Fields[j].Name
	})
	for _, op := range root.Fields {
		structName := kind + toExported(op.Name)
		f.Commentf("%s builder for %s", structName, op.Name)
		f.Type().Id(structName).Struct(
			jen.Id("field").Op("*").Id("Field"),
		)
		f.Line()

		f.Func().Params(jen.Id("q").Op("*").Id(structName)).Id("Kind").Params().Id("string").BlockFunc(func(g *jen.Group) {
			g.Return(jen.Lit(kind))
		})
		f.Line()

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

		retName, _ := extractGraphqlTypeName(op.Type)
		retType, ok := typeMap[retName]
		if ok && retType.Kind != "SCALAR" {
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

		f.Func().Params(jen.Id("q").Op("*").Id(structName)).Id("Build").
			Params().
			Params(jen.String(), jen.Map(jen.String()).Interface()).
			Block(
				jen.Return(jen.Id("build").Call(jen.Id("q").Dot("field"), jen.Lit(kind))),
			)
		f.Line()

		foundObj := false
		doRet := jen.Interface()
		doResp := jen.Id(toExported(op.Name)).Interface()
		retList := false
		_, retList = extractGraphqlTypeName(op.Type)

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
		} else {
			goType, ok := conf.ScalarMap[retName]
			if ok {
				if retList {
					if goType.Pkg != "" {
						doRet = jen.Index().Qual(goType.Pkg, goType.Type)
						doResp = jen.Id(toExported(op.Name)).Index().Qual(goType.Pkg, goType.Type)
					} else {
						doRet = jen.Index().Id(goType.Type)
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

		f.Func().Params(jen.Id("q").Op("*").Id(structName)).Id("Do").
			Params(jen.Id("ctx").Qual("context", "Context"), jen.Id("client").Op("*").Id("Client")).
			Params(doRet, jen.Error()).Block(
			jen.Id("query").Op(",").Id("vars").Op(":=").Id("q").Dot("Build").Call(),
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
