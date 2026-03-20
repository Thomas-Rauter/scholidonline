
# scholidonline

[![R-CMD-check](https://github.com/Thomas-Rauter/scholidonline/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Thomas-Rauter/scholidonline/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/Thomas-Rauter/scholidonline?branch=main&logo=codecov)](https://app.codecov.io/gh/Thomas-Rauter/scholidonline)

`scholidonline` provides lightweight **online** utilities for working
with scholarly identifiers in R. It builds on `scholid` for identifier
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
| `id_exists(x, type = NULL)` | Check whether identifiers exist in their respective registries |
| `id_convert(x, to, from = NULL)` | Convert identifiers across systems (e.g., PMID → DOI) |
| `id_metadata(x, type = NULL)` | Retrieve basic structured metadata |
| `id_links(x, type = NULL)` | Discover linked identifiers |

All functions are vectorized and return predictable base R objects
(logical vectors or data.frames).

Identifier detection and normalization are delegated to `scholid`.

## Relationship to scholid

`scholid` provides dependency-free utilities for detecting, normalizing,
classifying, and extracting scholarly identifiers.

`scholidonline` builds on that foundation and adds online registry
queries, while keeping dependencies minimal (`httr2`, `jsonlite`) and
maintaining CRAN-friendly behavior.

## License

MIT
