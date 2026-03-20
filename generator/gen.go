package generator

import (
	"bytes"
	"github.com/dave/jennifer/jen"
	"os"
	"path"
	"sort"
	"strings"
)

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

	// Output files
	var buf bytes.Buffer
	
	// model.go
	if err := f.Render(&buf); err != nil {
		return err
	}
	if err := os.WriteFile(path.Join(conf.OutPath, "model.go"), buf.Bytes(), 0644); err != nil {
		return err
	}
	buf.Reset()

	// selector.go
	if err := fSelector.Render(&buf); err != nil {
		return err
	}
	if err := os.WriteFile(path.Join(conf.OutPath, "selector.go"), buf.Bytes(), 0644); err != nil {
		return err
	}
	buf.Reset()

	// client.go
	if err := fClient.Render(&buf); err != nil {
		return err
	}
	if err := os.WriteFile(path.Join(conf.OutPath, "client.go"), buf.Bytes(), 0644); err != nil {
		return err
	}

	return nil
}
