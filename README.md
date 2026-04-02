
# scholidonline

[![R-CMD-check](https://github.com/Thomas-Rauter/scholidonline/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Thomas-Rauter/scholidonline/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/Thomas-Rauter/scholidonline?branch=main&logo=codecov)](https://app.codecov.io/gh/Thomas-Rauter/scholidonline)

`scholidonline` provides lightweight **online** utilities for working
with scholarly identifiers in R. It builds on
[`scholid`](https://thomas-rauter.github.io/scholid/) for identifier
detection and normalization, and adds minimal-dependency functions to
query external registries.

See the full documentation at the [scholidonline
website](https://thomas-rauter.github.io/scholidonline/).

## Installation

Install the released version from CRAN:

``` r
install.packages("scholidonline")
```

## Scope

The package focuses on online operations for common identifier systems
used in scholarly communication:

- DOI
- ORCID iD
- arXiv
- PubMed (PMID)
- PubMed Central (PMCID)

It provides registry-backed functionality such as:

- Existence checks
- Identifier conversion across systems
- Basic metadata retrieval
- Discovery of linked identifiers

## Interface

User-available functions:

| Function | Purpose |
|----|----|
| `scholidonline_types()` | Supported scholidonline identifier types |
| `scholidonline_capabilities()` | Supported scholidonline capabilities |
| `id_exists()` | Check whether identifiers exist in their respective registries |
| `id_convert()` | Convert identifiers across systems (e.g., PMID → DOI) |
| `id_metadata()` | Retrieve basic structured metadata |
| `id_links()` | Discover identifiers linked to the same scholarly record |

## Examples

``` r
# List supported scholidonline identifier types
scholidonline::scholidonline_types()
```

    ## [1] "arxiv" "doi"   "orcid" "pmcid" "pmid"

``` r
# List scholidonline capabilities
scholidonline::scholidonline_capabilities()
```

    ##     type operation target               providers default_provider
    ## 1  arxiv    exists   <NA>             auto, arxiv            arxiv
    ## 2  arxiv     links   <NA>             auto, arxiv            arxiv
    ## 3  arxiv      meta   <NA>             auto, arxiv            arxiv
    ## 4    doi    exists   <NA> auto, doi.org, crossref          doi.org
    ## 5    doi     links   <NA>          auto, crossref         crossref
    ## 6    doi      meta   <NA> auto, crossref, doi.org         crossref
    ## 7    doi   convert   pmid        auto, ncbi, epmc             ncbi
    ## 8    doi   convert  pmcid        auto, ncbi, epmc             ncbi
    ## 9  orcid    exists   <NA>             auto, orcid            orcid
    ## 10 orcid     links   <NA>             auto, orcid            orcid
    ## 11 orcid      meta   <NA>             auto, orcid            orcid
    ## 12 pmcid    exists   <NA>        auto, ncbi, epmc             ncbi
    ## 13 pmcid     links   <NA>        auto, ncbi, epmc             ncbi
    ## 14 pmcid      meta   <NA>        auto, ncbi, epmc             ncbi
    ## 15 pmcid   convert   pmid        auto, ncbi, epmc             ncbi
    ## 16 pmcid   convert    doi        auto, ncbi, epmc             ncbi
    ## 17  pmid    exists   <NA>        auto, ncbi, epmc             ncbi
    ## 18  pmid     links   <NA>        auto, ncbi, epmc             ncbi
    ## 19  pmid      meta   <NA>        auto, ncbi, epmc             ncbi
    ## 20  pmid   convert    doi        auto, ncbi, epmc             ncbi
    ## 21  pmid   convert  pmcid        auto, ncbi, epmc             ncbi

``` r
# Check if an ID exists online
scholidonline::id_exists(
  "10.1000/182",
  type = "doi"
  )
```

    ## [1] TRUE

``` r
# Convert identifiers across systems
scholidonline::id_convert(
  "12345678",
  to = "doi",
  from = "pmid"
  )
```

    ## [1] "10.1234/2013/999990"

``` r
# Retrieve scholarly metadata
out <- scholidonline::id_metadata(
  "10.1038/nature12373",
  type = "doi"
  )

# Show key fields
knitr::kable(out)
```

| input | type | provider | title | year | container | doi | pmid | pmcid | url |
|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|
| 10.1038/nature12373 | doi | crossref | Nanometre-scale thermometry in a living cell | 2013 | Nature | 10.1038/nature12373 | NA | NA | <https://doi.org/10.1038/nature12373> |

``` r
# Return identifiers linked to the same scholarly record
out <- scholidonline::id_links(
  "31452104",
  provider = "epmc"
  )

knitr::kable(out)
```

|     | query    | query_type | linked_type | linked_id                    | provider |
|:----|:---------|:-----------|:------------|:-----------------------------|:---------|
| 2   | 31452104 | pmid       | doi         | 10.1007/978-1-4939-9752-7_10 | epmc     |

## Relationship to scholid

[`scholid`](https://thomas-rauter.github.io/scholid/) provides
dependency-free utilities for detecting, normalizing, classifying, and
extracting scholarly identifiers.

`scholidonline` builds on that foundation and adds online registry
queries.

## License

MIT
