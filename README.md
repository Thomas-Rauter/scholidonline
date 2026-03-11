
# scholid

[![R-CMD-check](https://github.com/Thomas-Rauter/scholid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Thomas-Rauter/scholid/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/Thomas-Rauter/scholid?branch=main&logo=codecov)](https://app.codecov.io/gh/Thomas-Rauter/scholid)

`scholid` provides lightweight, dependency-free utilities for working
with scholarly identifiers in R. The package is designed as a small,
well-tested foundation that can be safely reused by other packages and
data workflows.

See the full documentation at the [scholid
website](https://thomas-rauter.github.io/scholid/).

## Installation

Install the released version from CRAN:

``` r
install.packages("scholid")
```

## Scope

The package focuses on common identifier systems used in scholarly
communication:

- DOI
- ORCID iD
- ISBN
- ISSN
- arXiv
- PubMed (PMID)
- PubMed Central (PMCID)

## Interface

User-available functions:

| Function | Purpose |
|----|----|
| `scholid_types()` | List supported scholarly identifier types |
| `is_scholid(x, type)` | Test whether values conform to a given identifier type |
| `normalize_scholid(x, type)` | Normalize identifiers to canonical form |
| `extract_scholid(text, type)` | Extract identifiers of a given type from free text |
| `classify_scholid(x)` | Guess the identifier type of each input value |
| `detect_scholid_type(x)` | Detect identifier types from canonical or wrapped input values |

## Examples

``` r
# list supported scholarly identifier types
scholid::scholid_types()
```

    ## [1] "arxiv" "doi"   "isbn"  "issn"  "orcid" "pmcid" "pmid"

``` r
# test whether values match a given identifier type
scholid::is_scholid(
  x    = "10.1000/182",
  type = "doi"
)
```

    ## [1] TRUE

``` r
# normalize identifiers to canonical form
scholid::normalize_scholid(
  x    = "https://doi.org/10.1000/182",
  type = "doi"
)
```

    ## [1] "10.1000/182"

``` r
# extract identifiers of a given type from free text
scholid::extract_scholid(
  text = "See https://doi.org/10.1000/182 for details.",
  type = "doi"
)
```

    ## [[1]]
    ## [1] "10.1000/182"

``` r
# classify the identifier type of each input value
scholid::classify_scholid(
  x = c(
    "10.1000/182",
    "0000-0002-1825-0097",
    "not an id"
  )
)
```

    ## [1] "doi"   "orcid" NA

``` r
# detect identifier types from canonical or wrapped input values
scholid::detect_scholid_type(
  x = c(
    "https://doi.org/10.1000/182",
    "ORCID: 0000-0002-1825-0097",
    "arXiv:2101.00001",
    "not an id"
  )
)
```

    ## [1] "doi"   "orcid" "arxiv" NA

For more detailed usage patterns check out the **Get started** vignette.

## License

MIT
