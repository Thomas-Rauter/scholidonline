# Convert scholarly identifiers across systems

Convert identifiers across registries (crosswalk), e.g. PMID -\> DOI.

`id_convert()` is vectorized over `x`. If `from` is `NULL`, the
identifier type is inferred per element using
[`scholid::detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.html)
after normalizing (where possible). Inputs that cannot be classified or
normalized yield `NA_character_`.

Provider-/ID-specific logic lives in internal helpers named
`convert_<from>_to_<to>()` (e.g., `convert_pmid_to_doi()`), which are
dispatched to from this front-door function.

## Usage

``` r
id_convert(x, to, from = NULL, provider = "auto", ..., quiet = FALSE)
```

## Arguments

- x:

  A character vector of identifiers.

- to:

  A single string giving the target identifier type. See
  [`scholid::scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.html)
  for supported values.

- from:

  A single string giving the source identifier type, or `NULL` to infer
  per element.

- provider:

  Provider to use (e.g. "auto", "ncbi", "epmc", ...).

- ...:

  Passed to provider-specific implementations.

- quiet:

  Logical; if `TRUE`, suppress provider warnings/messages where
  possible.

## Value

A character vector of converted identifiers. Unconvertible or
unclassified inputs yield `NA_character_`.

## Examples

``` r
if (FALSE) { # \dontrun{
id_convert("12345678", to = "doi", from = "pmid")
id_convert(c("10.1000/182", "PMC12345"), to = "pmid") # infer `from`
} # }
```
