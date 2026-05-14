# Return identifiers linked to the same scholarly record

Return identifiers that external registries link to the same scholarly
record or to a closely corresponding version of it.

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

  A single provider string specifying which online service to use. Use
  `"auto"` to use the default provider for the resolved identifier type.
  In most cases, `"auto"` is appropriate.

- ...:

  Reserved for future provider-specific arguments.

- quiet:

  A single logical value; if `TRUE`, suppress provider warnings and
  messages where possible.

## Value

A data.frame with columns `query`, `query_type`, `linked_type`,
`linked_id`, and `provider`. If no links are found, a zero-row
data.frame with these columns is returned.

## Details

`id_links()` is vectorized over `x` and returns a long data.frame with
one row per discovered identifier link.

Typical links include DOI \<-\> PMID, DOI \<-\> PMCID, PMID \<-\> PMCID,
arXiv ID \<-\> DOI, and ORCID -\> DOI for works recorded in ORCID.

Only identifier links explicitly exposed by the queried provider are
returned. `id_links()` does not retrieve general metadata or broader
related records unless the provider represents them as direct identifier
links.

Trivial self-links are excluded from the result.

## Examples

``` r
# \donttest{
  out <- id_links("31452104", provider = "epmc")
  knitr::kable(out)
#> 
#> 
#> |   |query    |query_type |linked_type |linked_id                    |provider |
#> |:--|:--------|:----------|:-----------|:----------------------------|:--------|
#> |2  |31452104 |pmid       |doi         |10.1007/978-1-4939-9752-7_10 |epmc     |
# }
```
