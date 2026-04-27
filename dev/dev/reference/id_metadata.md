# Retrieve scholarly metadata

Retrieve structured metadata for scholarly identifiers from external
registries.

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
  [`scholidonline_types()`](https://thomas-rauter.github.io/scholidonline/dev/reference/scholidonline_types.md)
  for supported values.

- provider:

  A single provider string specifying which online service to use. Use
  `"auto"` to use the default provider for the resolved identifier type.
  In most cases, `"auto"` is appropriate.

- fields:

  An optional character vector naming the columns to return. If `NULL`,
  all default columns are returned. Unknown field names are ignored.

- ...:

  Reserved for future provider-specific arguments.

- quiet:

  A single logical value; if `TRUE`, suppress provider warnings and
  messages where possible.

## Value

A data.frame with one row per input identifier. By default, the returned
columns are `input`, `type`, `provider`, `title`, `year`, `container`,
`doi`, `pmid`, `pmcid`, and `url`. Inputs that cannot be identified,
normalized, or resolved are returned as rows with missing metadata
fields.

## Details

`id_metadata()` is vectorized over `x` and returns a data.frame with one
row per input identifier.

The function returns a consistent cross-provider subset of core
bibliographic metadata, such as title, publication year, container
title, linked DOI, PMID, PMCID, and a canonical URL when available.

## Examples

``` r
# \donttest{
  out <- id_metadata("10.1038/nature12373", type = "doi")
  knitr::kable(out)
#> 
#> 
#> |input               |type |provider |title                                        | year|container |doi                 |pmid |pmcid |url                                 |
#> |:-------------------|:----|:--------|:--------------------------------------------|----:|:---------|:-------------------|:----|:-----|:-----------------------------------|
#> |10.1038/nature12373 |doi  |crossref |Nanometre-scale thermometry in a living cell | 2013|Nature    |10.1038/nature12373 |NA   |NA    |https://doi.org/10.1038/nature12373 |
  out <- id_metadata(c("31452104", "PMC6821181"))
  knitr::kable(out)
#> 
#> 
#> |input      |type  |provider |title                                                    | year|container        |doi |pmid     |pmcid      |url                                                   |
#> |:----------|:-----|:--------|:--------------------------------------------------------|----:|:----------------|:---|:--------|:----------|:-----------------------------------------------------|
#> |31452104   |pmid  |ncbi     |Molegro Virtual Docker for Docking.                      | 2019|Methods Mol Biol |NA  |31452104 |NA         |https://pubmed.ncbi.nlm.nih.gov/31452104/             |
#> |PMC6821181 |pmcid |ncbi     |Figure and caption extraction from biomedical documents. | 2019|Bioinformatics   |NA  |NA       |PMC6821181 |https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6821181/ |
  out <- id_metadata(
    "10.1038/nature12373",
     fields = c("title", "year", "doi")
     )
  knitr::kable(out)
#> 
#> 
#> |title                                        | year|doi                 |
#> |:--------------------------------------------|----:|:-------------------|
#> |Nanometre-scale thermometry in a living cell | 2013|10.1038/nature12373 |
# }
```
