# Convert scholarly identifiers across systems

Convert scholarly identifiers across registries, for example PMID -\>
DOI.

`id_convert()` is vectorized over `x`. If `from = NULL`, the source
identifier type is inferred per element using
[`scholid::detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.html).
Inputs that cannot be classified or normalized yield `NA_character_`.

Provider-/ID-specific logic lives in internal helpers named
`.convert_<from>_to_<to>()` (e.g. `.convert_pmid_to_doi()`), which are
dispatched to from this front-door function.

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

  A character vector of identifiers.

- to:

  A single target identifier type string. See
  [`scholidonline_types()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_types.md)
  for supported values.

- from:

  A single source identifier type string, or `NULL` to infer the source
  type for each element of `x`.

- provider:

  A single provider string. Use `"auto"` to use the default provider for
  the resolved conversion pair.

- ...:

  Reserved for future provider-specific arguments.

- quiet:

  A single logical value; if `TRUE`, suppress provider warnings/messages
  where possible.

## Value

A character vector of converted identifiers. Inputs that cannot be
classified, normalized, or converted yield `NA_character_`.

## Examples

``` r
if (FALSE) { # \dontrun{
id_convert("12345678", to = "doi", from = "pmid")
id_convert(c("10.1000/182", "PMC12345"), to = "pmid")
} # }
```
