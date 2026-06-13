
# scholidonline

[![R-CMD-check](https://github.com/Thomas-Rauter/scholidonline/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Thomas-Rauter/scholidonline/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/Thomas-Rauter/scholidonline?branch=main&logo=codecov)](https://app.codecov.io/gh/Thomas-Rauter/scholidonline)
[![CRAN
since](https://img.shields.io/badge/CRAN%20since-April%202026-blue)](https://CRAN.R-project.org/package=scholidonline)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/scholidonline)](https://CRAN.R-project.org/package=scholidonline)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-month/scholidonline)](https://CRAN.R-project.org/package=scholidonline)

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

The package provides registry-backed online operations where supported
for these identifier groups:

- **Bibliographic core:** DOI, PMID, PMCID, arXiv
- **Graph and people:** OpenAlex, ORCID
- **Organizations:** ROR
- **Life science:** UniProt; NCBI accessions (GEO, BioProject, RefSeq,
  SRA, genome assembly)

Operations include existence checks, metadata retrieval,
linked-identifier discovery, and cross-system conversion. Not every type
supports every operation; use `scholidonline_capabilities()` to inspect
what is available for a given type.

## Interface

User-available functions:

| Function | Purpose |
|----|----|
| `scholidonline_types()` | Supported scholidonline identifier types |
| `scholidonline_capabilities()` | Supported scholidonline capabilities |
| `id_exists()` | Check whether identifiers exist in their registries |
| `id_convert()` | Convert identifiers across systems (e.g., PMID → DOI) |
| `id_metadata()` | Retrieve basic structured metadata |
| `id_links()` | Discover identifiers linked to the same scholarly record when exposed by the provider |

## Examples

``` r
# List supported scholidonline identifier types
scholidonline::scholidonline_types()
```

    ##  [1] "arxiv"      "assembly"   "bioproject" "doi"        "geo"       
    ##  [6] "openalex"   "orcid"      "pmcid"      "pmid"       "refseq"    
    ## [11] "ror"        "sra"        "uniprot"

``` r
# List scholidonline capabilities
scholidonline::scholidonline_capabilities()
```

    ##          type operation target               providers default_provider
    ## 1       arxiv    exists   <NA>             auto, arxiv            arxiv
    ## 2       arxiv     links   <NA>             auto, arxiv            arxiv
    ## 3       arxiv      meta   <NA>             auto, arxiv            arxiv
    ## 4    assembly    exists   <NA>              auto, ncbi             ncbi
    ## 5    assembly      meta   <NA>              auto, ncbi             ncbi
    ## 6  bioproject    exists   <NA>              auto, ncbi             ncbi
    ## 7  bioproject      meta   <NA>              auto, ncbi             ncbi
    ## 8         doi    exists   <NA> auto, doi.org, crossref          doi.org
    ## 9         doi     links   <NA>          auto, crossref         crossref
    ## 10        doi      meta   <NA> auto, crossref, doi.org         crossref
    ## 11        doi   convert   pmid        auto, ncbi, epmc             ncbi
    ## 12        doi   convert  pmcid        auto, ncbi, epmc             ncbi
    ## 13        geo    exists   <NA>              auto, ncbi             ncbi
    ## 14        geo      meta   <NA>              auto, ncbi             ncbi
    ## 15   openalex    exists   <NA>          auto, openalex         openalex
    ## 16   openalex     links   <NA>          auto, openalex         openalex
    ## 17   openalex      meta   <NA>          auto, openalex         openalex
    ## 18   openalex   convert    doi          auto, openalex         openalex
    ## 19   openalex   convert   pmid          auto, openalex         openalex
    ## 20      orcid    exists   <NA>             auto, orcid            orcid
    ## 21      orcid     links   <NA>             auto, orcid            orcid
    ## 22      orcid      meta   <NA>             auto, orcid            orcid
    ## 23      pmcid    exists   <NA>        auto, ncbi, epmc             ncbi
    ## 24      pmcid     links   <NA>        auto, ncbi, epmc             ncbi
    ## 25      pmcid      meta   <NA>        auto, ncbi, epmc             ncbi
    ## 26      pmcid   convert   pmid        auto, ncbi, epmc             ncbi
    ## 27      pmcid   convert    doi        auto, ncbi, epmc             ncbi
    ## 28       pmid    exists   <NA>        auto, ncbi, epmc             ncbi
    ## 29       pmid     links   <NA>        auto, ncbi, epmc             ncbi
    ## 30       pmid      meta   <NA>        auto, ncbi, epmc             ncbi
    ## 31       pmid   convert    doi        auto, ncbi, epmc             ncbi
    ## 32       pmid   convert  pmcid        auto, ncbi, epmc             ncbi
    ## 33     refseq    exists   <NA>              auto, ncbi             ncbi
    ## 34     refseq      meta   <NA>              auto, ncbi             ncbi
    ## 35        ror    exists   <NA>               auto, ror              ror
    ## 36        ror      meta   <NA>               auto, ror              ror
    ## 37        sra    exists   <NA>              auto, ncbi             ncbi
    ## 38        sra      meta   <NA>              auto, ncbi             ncbi
    ## 39    uniprot    exists   <NA>           auto, uniprot          uniprot
    ## 40    uniprot      meta   <NA>           auto, uniprot          uniprot

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
# Return identifiers linked to the same scholarly record when the provider
# exposes them. Returns an empty table when no linked identifiers are found.
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
extracting scholarly identifiers (including types without online support
here, such as ISBN, ISSN, and bibcode).

`scholidonline` builds on that foundation and adds online registry
queries for a subset of those types.

## License

MIT
