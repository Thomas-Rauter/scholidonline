# Convert scholarly identifiers across systems

Convert scholarly identifiers across registries, for example from PMID
to DOI.

## Usage

``` r
id_convert(
  x,
  to = scholidonline_types(),
  from = NULL,
  provider = c("auto", .scholidonline_providers()),
  ...,
  quiet = FALSE
)
```

## Arguments

- x:

  A character vector of scholarly identifiers.

- to:

  A single target identifier type string, such as `"doi"` or `"pmid"`.
  See
  [`scholidonline_types()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_types.md)
  for all supported values.

- from:

  A single source identifier type string, or `NULL` to infer the source
  type for each element of `x`.

- provider:

  A single provider string specifying which online service to use for
  the conversion. Use `"auto"` to use the default provider for the
  requested conversion. In most cases, `"auto"` is appropriate.

- ...:

  Reserved for future provider-specific arguments.

- quiet:

  A single logical value; if `TRUE`, suppress provider warnings and
  messages where possible.

## Value

A character vector of converted identifiers. Elements that cannot be
identified, normalized, or converted return `NA_character_`.

## Details

Only some source/target type pairs are supported. Use
[`scholidonline_capabilities()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_capabilities.md)
with `operation = "convert"` (or filter the returned table) to see which
conversions are available and which providers implement them.

## Examples

``` r
# \donttest{
  id_convert("12345678", to = "doi", from = "pmid")
#> [1] "10.1234/2013/999990"
  id_convert("10.1038/nature12373", to = "pmid", from = "doi")
#> [1] "23903748"
# }
```
