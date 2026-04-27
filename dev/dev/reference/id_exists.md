# Check whether scholarly identifiers exist

Check whether scholarly identifiers are found in their respective
registries.

## Usage

``` r
id_exists(
  x,
  type = c("auto", scholidonline_types()),
  provider = c("auto", .scholidonline_providers()),
  ...,
  quiet = FALSE
)
```

## Arguments

- x:

  A character vector of identifiers.

- type:

  A single identifier type string, or `"auto"` to infer the type for
  each element of `x`. See
  [`scholidonline_types()`](https://thomas-rauter.github.io/scholidonline/dev/reference/scholidonline_types.md)
  for supported values.

- provider:

  A single provider string specifying which online service to use for
  the lookup. Use `"auto"` to use the default provider for the resolved
  identifier type. In most cases, `"auto"` is appropriate.

- ...:

  Reserved for future provider-specific arguments.

- quiet:

  A single logical value; if `TRUE`, suppress provider warnings and
  messages where possible.

## Value

A logical vector. `TRUE` indicates that the identifier was found,
`FALSE` indicates that it was not found, and `NA` indicates that the
input could not be identified, normalized, or checked reliably.

## Examples

``` r
# \donttest{
  id_exists("10.1038/nature12373", type = "doi")
#> [1] TRUE
  id_exists(c("31452104", "PMC6784763"))
#> [1] TRUE TRUE
# }
```
