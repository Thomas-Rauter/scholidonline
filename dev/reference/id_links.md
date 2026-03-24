# Return linked scholarly identifiers

Return identifier links that external registries associate with the same
scholarly object or a directly corresponding manifestation.

`id_links()` is vectorized over `x` and returns a long data.frame with
one row per discovered identifier link.

The function is intended to expose cross-registry identifier links such
as DOI ↔ PMID, DOI ↔ PMCID, PMID ↔ PMCID, arXiv ID ↔ DOI, and ORCID →
DOI for works recorded in ORCID.

Only identifier links explicitly exposed by the queried provider are
returned. `id_links()` is not a general metadata retrieval function and
does not attempt to return broader related records unless the provider
represents them as direct identifier links for the same object or
directly corresponding manifestation.

If `type = "auto"`, the identifier type is inferred per element using
[`scholid::detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.html).
Inputs that cannot be classified or normalized yield zero rows.

Provider-/ID-specific logic lives in internal helpers named
`.links_<type>()` (e.g. `.links_pmid()`), which are dispatched to from
this front-end function.

## Usage

``` r
id_links(
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
  [`scholidonline_types()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_types.md)
  for supported values.

- provider:

  A single provider string. Use `"auto"` to use the default provider for
  the resolved identifier type.

- ...:

  Reserved for future provider-specific arguments.

- quiet:

  A single logical value; if `TRUE`, suppress provider warnings/messages
  where possible.

## Value

A data.frame with columns `input`, `input_type`, `linked_type`,
`linked_value`, and `provider`.

## Examples

``` r
if (FALSE) { # \dontrun{
id_links("10.1000/182", type = "doi")
id_links(c("12345678", "PMC12345"))
} # }
```
