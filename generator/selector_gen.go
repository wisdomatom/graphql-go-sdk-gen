package generator

import (
	"fmt"
	"github.com/dave/jennifer/jen"
	"sort"
)

func genSelector(conf *GenerateConfig, f *jen.File, tp TypeDef, objectMap map[string]struct{}) {
	sel := fmt.Sprintf("Selector%v", tp.Name)
	ctor := fmt.Sprintf("Select%v", tp.Name)
	f.Commentf("%s selector", sel)
	f.Type().Id(sel).Struct(jen.Id("field").Op("*").Id("Field"))
	f.Line()

	f.Func().Id(ctor).Params(jen.Id("parent").Op("string")).Op("*").Id(sel).Block(
		jen.Return(jen.Op("&").Id(sel).Values(
			jen.Dict{
				jen.Id("field"): jen.Op("&").Id("Field").Values(
					jen.Dict{
						jen.Id("Name"):     jen.Id("parent"),
						jen.Id("Args"):     jen.Make(jen.Map(jen.String()).Op("*").Id("FieldArg")),
						jen.Id("Children"): jen.Index().Op("*").Id("Field").Values(),
					},
				),
			},
		)),
	)
	f.Line()

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
	f.Line()

	for _, child := range tp.Fields {
		tpN, _ := extractGraphqlTypeName(child.Type)
		_, ok := objectMap[tpN]
		if !ok {
			continue
		}
		var selectFnParam []jen.Code
		selName := fmt.Sprintf("Selector%v", tpN)

		sort.Slice(child.Args, func(i, j int) bool {
			return child.Args[i].Name > child.Args[j].Name
		})
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
		f.Line()
	}

	f.Line()
	f.Func().Params(jen.Id("q").Op("*").Id(sel)).Id("GetField").Params().Op("*").Id("Field").Block(
		jen.Return(jen.Id("q").Dot("field")),
	)
	f.Line()
}
