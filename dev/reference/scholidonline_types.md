# Supported scholidonline identifier types

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
#>  [1] "arxiv"      "assembly"   "bioproject" "doi"        "geo"       
#>  [6] "openalex"   "orcid"      "pmcid"      "pmid"       "refseq"    
#> [11] "ror"        "sra"        "uniprot"   
"doi" %in% scholidonline_types()
#> [1] TRUE
```
