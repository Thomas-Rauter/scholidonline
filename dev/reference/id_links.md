# Return linked scholarly identifiers

Return “all linked identifiers we can find” for each input identifier.

`id_links()` is vectorized over `x` and returns a long data.frame with
one row per (input, linked_type, linked_value).

If `type` is `NULL`, the identifier type is inferred per element using
[`scholid::detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.html)
after normalization (where possible). Inputs that cannot be classified
or normalized yield zero rows.

Provider-/ID-specific logic lives in internal helpers named
`links_<type>()` (e.g., `links_pmid()`), which are dispatched to from
this front-door function.

## Usage

``` r
id_links(x, type = NULL, provider = "auto", ..., quiet = FALSE)
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

  Provider to use (e.g. "auto", "crossref", "ncbi", "epmc", "orcid",
  "arxiv").

- ...:

  Passed to provider-specific implementations.

- quiet:

  Logical; if `TRUE`, suppress provider warnings/messages where
  possible.

## Value

A data.frame with columns: `input`, `input_type`, `linked_type`,
`linked_value`, `provider`.

## Examples

``` r
if (FALSE) { # \dontrun{
id_links("10.1000/182", type = "doi")
id_links(c("12345678", "PMC12345"))  # infer type
} # }
```
