# Check whether scholarly identifiers exist

Check whether identifiers resolve or are found in their respective
registries.

`id_exists()` is vectorized over `x`. If `type` is `NULL`, the
identifier type is inferred per element using
[`scholid::detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.html)
after normalization (where possible). Inputs that cannot be classified
or normalized yield `NA`.

Provider-/ID-specific logic lives in internal helpers named
`exists_<type>()` (e.g., `exists_doi()`), which are dispatched to from
this front-door function.

## Usage

``` r
id_exists(x, type = NULL, provider = "auto", ..., quiet = FALSE)
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

  Provider to use (e.g. "auto", "doi.org", "crossref", "ncbi", "epmc",
  "orcid", "arxiv").

- ...:

  Passed to provider-specific implementations.

- quiet:

  Logical; if `TRUE`, suppress provider warnings/messages where
  possible.

## Value

A logical vector. `TRUE` if the identifier exists, `FALSE` if it is
confirmed not found, and `NA` if the input cannot be classified or
normalized.

## Examples

``` r
if (FALSE) { # \dontrun{
id_exists("10.1000/182", type = "doi")
id_exists(c("12345678", "PMC12345"))  # infer type
} # }
```
