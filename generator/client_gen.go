package generator

import (
	"github.com/dave/jennifer/jen"
)

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

func genBuildFunc(f *jen.File) {
	f.Func().Id("build").
		Params(jen.Id("field").Op("*").Id("Field"), jen.Id("kind").Id("string")).
		Params(jen.String(), jen.Map(jen.String()).Interface()).Block(
		jen.Id("vars").Op(":=").Map(jen.String()).Interface().Values(),
		jen.Id("varTypes").Op(":=").Map(jen.String()).String().Values(),
		jen.Id("nameMap").Op(":=").Map(jen.String()).String().Values(),
		jen.Id("counter").Op(":=").Lit(0),
		jen.Id("fieldCollectVars").Call(jen.Id("field"), jen.Id("vars"), jen.Id("varTypes"), jen.Id("nameMap"), jen.Op("&").Id("counter")),
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
		jen.Id("counter").Op("=").Lit(0),
		jen.Id("body").Op(":=").Qual("strings", "TrimSpace").Call(jen.Id("fieldToGraphQL").Call(jen.Id("field"), jen.Lit("  "), jen.Id("nameMap"), jen.Op("&").Id("counter"))),
		jen.Id("query").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%s%s{\n%s\n}"), jen.Id("strings").Dot("ToLower").Call(jen.Id("kind")), jen.Id("decl"), jen.Id("body")),
		jen.Return(jen.Id("query"), jen.Id("vars")),
	)
}

func genPtrFunc(f *jen.File) {
	f.Func().Id("Ptr").Types(jen.Id("T").Any()).
		Params(jen.Id("v").Id("T")).
		Op("*").Id("T").
		Block(
			jen.Return(jen.Op("&").Id("v")),
		)
}

func genFieldHelpers(f *jen.File) {
	f.Type().Id("FieldArg").Struct(
		jen.Id("Arg").Interface(),
		jen.Id("ArgType").String(),
	)
	f.Comment("Field is a selection node")
	f.Type().Id("Field").Struct(
		jen.Id("Name").String(),
		jen.Id("Args").Map(jen.String()).Op("*").Id("FieldArg"),
		jen.Id("Children").Index().Op("*").Id("Field"),
	)
	f.Line()

	f.Func().Id("fieldCollectVars").Params(
		jen.Id("f").Op("*").Id("Field"),
		jen.Id("vars").Map(jen.String()).Interface(),
		jen.Id("varTypes").Map(jen.String()).String(),
		jen.Id("nameMap").Map(jen.String()).String(),
		jen.Id("counter").Op("*").Int(),
	).Block(
		jen.If(jen.Id("f").Op("==").Nil()).Block(jen.Return()),
		jen.Id("var").Id("args").Index().Id("string"),
		jen.For(jen.Id("k").Op(",").Id("_").Op(":=").Range().Op(jen.Id("f").Dot("Args").GoString())).Block(
			jen.Id("args").Op("=").Append(jen.Id("args").Op(",").Id("k")),
		),
		jen.Qual("sort", "Strings").Call(jen.Id("args")),
		jen.For(jen.Id("_").Op(",").Id("k").Op(":=").Range().Op(jen.Id("args").GoString())).Block(
			jen.Id("v").Op(":=").Id("f").Dot("Args").Index(jen.Id("k")),
			jen.If(jen.Id("v").Op("==").Nil()).Block(jen.Continue()),
			jen.Op("*").Id("counter").Op("++"),
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

	f.Func().Id("fieldToGraphQL").
		Params(jen.Id("f").Op("*").Id("Field"), jen.Id("indent").String(), jen.Id("nameMap").Map(jen.String()).String(), jen.Id("counter").Op("*").Id("int")).
		String().Block(
		jen.If(jen.Id("f").Op("==").Nil()).Block(jen.Return(jen.Lit(""))),
		jen.Id("var").Id("b").Qual("strings", "Builder"),
		jen.Id("b").Dot("WriteString").Call(jen.Id("indent").Op("+").Id("f").Dot("Name")),
		jen.If(jen.Len(jen.Id("f").Dot("Args")).Op(">").Lit(0)).Block(
			jen.Id("b").Dot("WriteString").Call(jen.Lit("")),
			jen.Id("b").Dot("WriteString").Call(jen.Lit("(")),
			jen.Id("i").Op(":=").Lit(0),
			jen.Id("var").Id("args").Index().Id("string"),
			jen.For(jen.Id("k").Op(",").Id("_").Op(":=").Range().Op(jen.Id("f").Dot("Args").GoString())).Block(
				jen.Id("args").Op("=").Append(jen.Id("args").Op(",").Id("k")),
			),
			jen.Qual("sort", "Strings").Call(jen.Id("args")),
			jen.For(jen.Id("_").Op(",").Op("k").Op(":=").Range().Op(jen.Id("args").GoString())).Block(
				jen.If(jen.Id("i").Op(">").Lit(0)).Block(jen.Id("b").Dot("WriteString").Call(jen.Lit(", "))),
				jen.Op("*").Id("counter").Op("++"),
				jen.Id("varName").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v_%v"), jen.Id("k"), jen.Op("*").Id("counter")),
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
			jen.Id("b").Dot("WriteString").Call(jen.Id("fieldToGraphQL").Call(jen.Id("c"), jen.Id("indent").Op("+").Lit("  "), jen.Id("nameMap"), jen.Id("counter"))),
		),
		jen.Id("b").Dot("WriteString").Call(jen.Id("indent").Op("+").Lit("}\n")),
		jen.Return(jen.Id("b").Dot("String").Call()),
	)
	f.Line()

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
