# scholidonline identifier registry

Internal registry defining the identifier types supported by the
scholidonline package and their associated metadata.

The registry is the single source of truth for identifier capabilities,
including:

- existence-check providers

- default providers

- supported identifier conversions

- conversion providers

Helper functions in this file expose registry metadata used by the
exported front-end functions (e.g.
[`id_exists()`](https://thomas-rauter.github.io/scholidonline/reference/id_exists.md),
[`id_convert()`](https://thomas-rauter.github.io/scholidonline/reference/id_convert.md)).
Supported scholidonline identifier types

Return the set of identifier types supported by the scholidonline
package.

This is the set of identifier types for which scholidonline provides
registry-backed functionality, including existence checks, identifier
conversion, metadata retrieval, and link discovery.

## Usage

``` r
scholidonline_types()
```

## Value

A character vector of supported identifier type strings.

## Examples

``` r
scholidonline_types()
#> [1] "arxiv" "doi"   "orcid" "pmcid" "pmid" 
"doi" %in% scholidonline_types()
#> [1] TRUE
```
