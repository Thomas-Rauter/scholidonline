# Supported scholidonline identifier types

Return the set of identifier types supported by the scholidonline
package.

This is the set of identifier types for which scholidonline provides
registry-backed functionality. Available operations vary by type; use
[`scholidonline_capabilities()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_capabilities.md)
to see which of existence checks, metadata retrieval, link discovery,
and identifier conversion are supported for each type.

## Usage

``` r
scholidonline_types()
```

## Value

A character vector of supported identifier type strings.

## Examples

``` r
scholidonline_types()
#>  [1] "arxiv"      "assembly"   "bioproject" "doi"        "geo"       
#>  [6] "openalex"   "orcid"      "pmcid"      "pmid"       "refseq"    
#> [11] "ror"        "sra"        "uniprot"   
"doi" %in% scholidonline_types()
#> [1] TRUE
```
