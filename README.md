# GraphQL Go SDK Generator

This tool generates a type-safe Go SDK from a GraphQL introspection schema. It creates Go models for your schema types and a fluent, type-safe DSL to build and execute queries and mutations.

## Features

- **Model Generation**: Automatically generates Go structs for all `OBJECT`, `INPUT_OBJECT`, `ENUM`, and `INTERFACE` types from your GraphQL schema.
- **Type-Safe Query Builder**: Creates `Selector` types for each object, enabling you to build complex GraphQL queries in a type-safe manner, with autocompletion support in your IDE.
- **Flexible Client**: Generates a `Client` object that can be configured with a custom `http.Client`, allowing for flexible authentication, middleware, and transport-level logic.
- **Fluent Operation Builders**: For each `Query` and `Mutation`, it generates a builder struct that provides:
    - Methods for setting operation arguments.
    - A `Select()` method to specify which fields to retrieve, using the generated selectors.
    - A `Build()` method to get the final GraphQL query string and variables map.
    - A `Do()` method to execute the request using a `context.Context` and a configured `Client` instance, unmarshalling the response into strongly-typed Go structs.
- **Custom Scalar Mapping**: Allows you to map any GraphQL scalar to a specific Go type via a configuration file.
- **JSON Tag Customization**: Supports adding `omitempty` to JSON tags for all struct fields.

## Usage

You can run the generator using the `go run` command.

```bash
go run main.go --schema <path_to_schema.json> --out <output_directory> [--pkg <package_name>] [--scalars <path_to_scalars.json>] [--omit-empty]
```

### Command-Line Flags

- `--schema` (Required): Path to the GraphQL introspection JSON file. You can get this by querying a GraphQL endpoint's `__schema`.
- `--out` (Default: `./`): The output directory for the generated Go files.
- `--pkg` (Default: `client`): The Go package name for the generated files.
- `--scalars` (Optional): Path to a JSON file for mapping custom GraphQL scalars to Go types.
- `--omit-empty` (Optional): If specified, adds `,omitempty` to the JSON tags in the generated model structs.

## Generated Files

The generator produces three files in the specified output directory:

1.  `model.go`: Contains all the generated Go structs corresponding to your GraphQL schema's `OBJECT`, `INPUT_OBJECT`, `ENUM`, and `INTERFACE` types.
2.  `selector.go`: Contains the `Selector` types (e.g., `UserSelector`) and their methods, which form the basis of the type-safe query builder.
3.  `client.go`: Contains the `Client` object, the high-level operation builders (e.g., `QueryUsers`, `MutationCreateUser`), and other client-side helper code.

## Example

Let's assume you have a simple GraphQL schema to query users.

### 1. Generate the SDK

First, run the generator with your schema:

```bash
go run main.go --schema ./schema.json --out ./sdk --pkg sdk
```

### 2. Use the Generated SDK

Now you can use the generated SDK in your Go application to build and execute a query. The new design allows you to configure a client with custom authentication.

```go
package main

import (
	"context"
	"fmt"
	"net/http"
	"yourapp/sdk" // Import the generated package
)

// authTransport is a custom http.RoundTripper that adds an Authorization header.
type authTransport struct {
	transport http.RoundTripper
	token     string
}

// RoundTrip injects the authentication token into each request.
func (t *authTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	transport := t.transport
	if transport == nil {
		transport = http.DefaultTransport
	}
	newReq := req.Clone(req.Context())
	newReq.Header.Set("Authorization", "Bearer "+t.token)
	return transport.RoundTrip(newReq)
}

func main() {
	// 1. Create a custom http.Client for authentication.
	// This gives you full control over headers, timeouts, and transport-level logic.
	customHTTPClient := &http.Client{
		Transport: &authTransport{
			token: "YOUR_SECRET_TOKEN",
		},
	}

	// 2. Create a new GraphQL client with the endpoint and your custom http.Client.
	endpoint := "https://api.example.com/graphql"
	client := sdk.NewClient(endpoint, customHTTPClient)

	// 3. Create a new builder for the "users" query.
	query := sdk.NewQueryUsers()

	// 4. Set arguments for the query (if any).
	query.Where(sdk.UserWhereInput{
		Name: "John Doe",
	})

	// 5. Select the fields you want in the response.
	query.Select(func(s *sdk.SelectorUser) {
		s.Select(
			sdk.UserFieldId,
			sdk.UserFieldName,
			sdk.UserFieldEmail,
		)
		s.SelectPosts(func(p *sdk.SelectorPost) {
			p.Select(
				sdk.PostFieldId,
				sdk.PostFieldTitle,
			)
		})
	})

	// 6. Execute the query using the client.
	// Pass a context and the configured client to the Do method.
	users, err := query.Do(context.Background(), client)
	if err != nil {
		panic(err)
	}

	// 7. Use the typed result.
	fmt.Printf("Successfully fetched %d users.\n", len(users))
	for _, user := range users {
		fmt.Printf("- User ID: %s, Name: %s\n", user.Id, user.Name)
		for _, post := range user.Posts {
			fmt.Printf("  - Post: %s\n", post.Title)
		}
	}
}
```

## Custom Scalar Mapping

If your schema uses custom scalars like `DateTime` or `Decimal`, you can map them to specific Go types.

Create a `scalars.json` file:

```json
{
  "DateTime": {
    "type": "Time",
    "pkg": "time"
  },
  "Decimal": {
    "type": "Decimal",
    "pkg": "github.com/shopspring/decimal"
  },
  "ObjectID": {
    "type": "string"
  }
}
```

Then, pass it to the generator using the `--scalars` flag:

```bash
go run main.go --schema schema.json --out ./sdk --scalars scalars.json
```

The generator will now use `time.Time` for `DateTime` fields and `decimal.Decimal` for `Decimal` fields in the generated models.