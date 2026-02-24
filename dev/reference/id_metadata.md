# Fetch metadata for scholarly identifiers

Fetch structured metadata for identifiers using online registries.

`id_metadata()` is vectorized over `x` and returns a data.frame with one
row per input identifier. If `type` is `NULL`, the identifier type is
inferred per element using
[`scholid::detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.html)
after normalization (where possible). Inputs that cannot be classified
or normalized yield one row with `NA` metadata fields.

Provider-/ID-specific logic lives in internal helpers named
`metadata_<type>()` (e.g., `metadata_doi()`), which are dispatched to
from this front-door function.

## Usage

``` r
id_metadata(
  x,
  type = NULL,
  provider = "auto",
  fields = NULL,
  ...,
  quiet = FALSE
)
```

## Arguments

- x:

  A character vector of identifiers.

- type:

  A single string giving the identifier type, or `NULL` to infer per
  element. See
  [`scholid::scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.html)
  for supported values.

- provider:

  Provider to use (e.g. "auto", "crossref", "doi.org", "ncbi", "epmc",
  "orcid", "arxiv").

- fields:

  Optional character vector of fields to return. If `NULL`, returns a
  stable default set.

- ...:

  Passed to provider-specific implementations.

- quiet:

  Logical; if `TRUE`, suppress provider warnings/messages where
  possible.

## Value

A data.frame with one row per input, containing at least: `input`,
`type`, `provider`, `title`, `year`, `authors`, `container`, `doi`,
`pmid`, `pmcid`, `url`.

## Examples

``` r
if (FALSE) { # \dontrun{
id_metadata("10.1000/182", type = "doi")
id_metadata(c("12345678", "PMC12345"))  # infer type
} # }
```
