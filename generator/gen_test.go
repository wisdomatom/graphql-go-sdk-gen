package generator

import (
	"fmt"
	"github.com/dave/jennifer/jen"
	"strings"
	"testing"
)

func TestExtractGoType(t *testing.T) {
	conf := &GenerateConfig{
		ScalarMap: map[string]GoType{
			"DateTime": {Pkg: "time", Type: "Time"},
		},
	}

	tests := []struct {
		name     string
		gqlType  GQLType
		expected string
	}{
		{
			name: "Simple Scalar",
			gqlType: GQLType{
				Kind: "SCALAR",
				Name: "String",
			},
			expected: "string",
		},
		{
			name: "List of Scalars",
			gqlType: GQLType{
				Kind: "LIST",
				OfType: &GQLType{
					Kind: "SCALAR",
					Name: "String",
				},
			},
			expected: "[]string",
		},
		{
			name: "NonNull Scalar (Current Behavior)",
			gqlType: GQLType{
				Kind: "NON_NULL",
				OfType: &GQLType{
					Kind: "SCALAR",
					Name: "String",
				},
			},
			expected: "string",
		},
		{
			name: "List of NonNull Scalars",
			gqlType: GQLType{
				Kind: "LIST",
				OfType: &GQLType{
					Kind: "NON_NULL",
					OfType: &GQLType{
						Kind: "SCALAR",
						Name: "String",
					},
				},
			},
			expected: "[]string",
		},
		{
			name: "Custom Scalar with Package",
			gqlType: GQLType{
				Kind: "SCALAR",
				Name: "DateTime",
			},
			expected: "time.Time",
		},
		{
			name: "Nested List",
			gqlType: GQLType{
				Kind: "LIST",
				OfType: &GQLType{
					Kind: "LIST",
					OfType: &GQLType{
						Kind: "SCALAR",
						Name: "Int",
					},
				},
			},
			expected: "[][]int",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sta := &jen.Statement{}
			extractGoType(conf, tt.gqlType, sta)
			got := fmt.Sprintf("%#v", sta)
			if got != tt.expected {
				t.Errorf("extractGoType() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestBuildFields(t *testing.T) {
	conf := &GenerateConfig{
		JsonOmitEmpty: true,
		ScalarMap:     map[string]GoType{},
	}

	defs := []FieldDef{
		{
			Name: "user_name",
			Type: GQLType{Kind: "SCALAR", Name: "String"},
		},
	}

	codes := buildFields(conf, KindObject, "User", defs)
	
	f := jen.NewFile("test")
	f.Type().Id("User").Struct(codes...)
	
	got := fmt.Sprintf("%#v", f)
	if !strings.Contains(got, "json:\"user_name,omitempty\"") {
		t.Errorf("buildFields() tag not found, got: %v", got)
	}
}

func TestSafeFieldName(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{"Normal", "user_name", "UserName"},
		{"With Numbers", "field2", "Field2"},
		{"Go Keyword", "func", "Func_"},
		{"Empty", "", "Field"},
		{"Special Chars", "my-field.name", "MyFieldName"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := safeFieldName(tt.input); got != tt.expected {
				t.Errorf("safeFieldName(%v) = %v, want %v", tt.input, got, tt.expected)
			}
		})
	}
}
