# Retrieve scholarly metadata

Retrieve structured metadata for scholarly identifiers from external
registries.

`id_metadata()` is vectorized over `x` and returns a data.frame with one
row per input element.

The function returns a stable cross-provider subset of record-level
metadata for the queried identifier. It is intended to expose core
bibliographic fields such as title, publication year, container title,
linked DOI, PMID, and PMCID when available, and a canonical URL.

If `type = "auto"`, the identifier type is inferred per element using
[`scholid::detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.html).
Inputs that cannot be classified or normalized are returned as rows with
`NA` metadata fields.

Provider-/ID-specific logic lives in internal helpers named
`.meta_<type>()` (e.g. `.meta_pmid()`), which are dispatched to from
this front-end function.

## Usage

``` r
id_metadata(
  x,
  type = c("auto", scholidonline_types()),
  provider = c("auto", .scholidonline_providers()),
  fields = NULL,
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
  [`scholidonline_types()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_types.md)
  for supported values.

- provider:

  A single provider string. Use `"auto"` to use the default provider for
  the resolved identifier type.

- fields:

  An optional character vector of column names to return. If `NULL`, all
  default metadata columns are returned.

- ...:

  Reserved for future provider-specific arguments.

- quiet:

  A single logical value; if `TRUE`, suppress provider warnings/messages
  where possible.

## Value

A data.frame with one row per input identifier. By default, the returned
columns are `input`, `type`, `provider`, `title`, `year`, `container`,
`doi`, `pmid`, `pmcid`, and `url`.

## Examples

``` r
if (FALSE) { # \dontrun{
id_metadata("10.1038/nature12373", type = "doi")
id_metadata(c("31452104", "PMC6821181"))
id_metadata("10.1038/nature12373", fields = c("title", "year", "doi"))
} # }
```
