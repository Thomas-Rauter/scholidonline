# Getting started with scholidonline

`scholidonline` provides online utilities for working with scholarly
identifiers. It builds on `scholid` for structural detection and
normalization, and adds registry-backed functionality such as:

- Existence checks
- Identifier conversion across systems
- Metadata retrieval
- Discovery of linked identifiers

This vignette introduces the interface and typical workflows when
working with registry-connected identifier data.

## Installation

``` r
install.packages("scholidonline")
```

## Interface

`scholidonline` exposes a small set of user-facing functions:

- `id_exists(x, type = NULL)`
- `id_convert(x, to, from = NULL)`
- `id_metadata(x, type = NULL)`
- `id_links(x, type = NULL)`

All functions are:

- Vectorized
- Type-stable
- Designed for use in pipelines
- Backed by external registries

Identifier detection and normalization are delegated internally to
`scholid`.

## Supported identifier types

You can inspect which identifier types are supported:

``` r
scholidonline::scholidonline_types()
```

    ## [1] "arxiv" "doi"   "orcid" "pmcid" "pmid"

## Inspecting capabilities

`scholidonline` is registry-driven. You can inspect all supported
operations, conversions, and providers:

``` r
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

## Existence checks: `id_exists()`

[`id_exists()`](https://thomas-rauter.github.io/scholidonline/reference/id_exists.md)
verifies whether identifiers exist in their respective registries.

``` r
scholidonline::id_exists(
  x    = "10.1000/182",
  type = "doi"
)
```

    ## [1] TRUE

If `type = NULL`, the type is inferred automatically:

``` r
scholidonline::id_exists(
  x = c(
    "10.1000/182",
    "12345678"
  )
)
```

    ## [1] TRUE TRUE

Return values:

- TRUE → confirmed by registry
- FALSE → confirmed not found
- NA → cannot be classified or normalized

## Conversion: `id_convert()`

Many scholarly identifiers are cross-linked across systems.

Common examples:

- PMID → DOI
- PMCID → PMID
- DOI → PMCID

``` r
scholidonline::id_convert(
  x    = "12345678",
  from = "pmid",
  to   = "doi"
)
```

    ## [1] "10.1234/2013/999990"

If `from = NULL`, the source type is inferred per element:

``` r
scholidonline::id_convert(
  x = c("12345678", "PMC1234567"),
  to = "doi"
)
```

    ## [1] "10.1234/2013/999990"              "10.1097/00000658-199503000-00007"

Unresolvable mappings return `NA_character_`.

## Metadata retrieval: `id_metadata()`

[`id_metadata()`](https://thomas-rauter.github.io/scholidonline/reference/id_metadata.md)
retrieves harmonized metadata from external registries.

``` r
scholidonline::id_metadata(
  x    = "10.1000/182",
  type = "doi"
)
```

    ##         input type provider title year container  doi pmid pmcid  url
    ## 1 10.1000/182  doi     <NA>  <NA>   NA      <NA> <NA> <NA>  <NA> <NA>

Returned metadata is registry-derived and minimally harmonized across
providers.

Typical columns include:

- input
- type
- provider
- title
- year
- container
- doi
- pmid
- pmcid
- url

Metadata completeness depends on the external authority.

You can restrict returned fields:

``` r
scholidonline::id_metadata(
  x = "10.1000/182",
  fields = c("title", "year", "doi")
)
```

    ##   title year  doi
    ## 1  <NA>   NA <NA>

## Linked identifiers: `id_links()`

[`id_links()`](https://thomas-rauter.github.io/scholidonline/reference/id_links.md)
returns related identifiers discovered via registry queries.

``` r
scholidonline::id_links(
  x    = "PMC1234567",
  type = "pmcid"
)
```

    ##        input input_type linked_type                     linked_value provider
    ## 1 PMC1234567      pmcid        pmid                          7717779     ncbi
    ## 3 PMC1234567      pmcid         doi 10.1097/00000658-199503000-00007     ncbi

This may reveal:

- DOI
- PMID
- PMCID
- Other linked identifiers

The result is a long data.frame with one row per link.

## Working with mixed data

A common workflow for messy identifier columns:

1.  Detect identifier types (via `scholid`)
2.  Normalize identifiers
3.  Check registry existence
4.  Convert or enrich with metadata

Example:

``` r
x <- c(
  "https://doi.org/10.1000/182",
  "PMID: 12345678",
  "not an id"
)

types <- scholid::detect_scholid_type(x)

x_norm <- rep(NA_character_, length(x))

for (i in seq_along(x)) {
  if (is.na(types[i])) {
    next
  }

  x_norm[i] <- scholid::normalize_scholid(
    x = x[i],
    type = types[i]
  )
}

types
```

    ## [1] "doi"  "issn" NA

``` r
x_norm
```

    ## [1] "10.1000/182" "1234-5678"   NA

``` r
scholidonline::id_exists(x)
```

    ## [1] TRUE   NA   NA

This separation keeps structural logic (`scholid`) and registry logic
(`scholidonline`) clearly distinct.

## Provider selection

Most functions accept a `provider` argument.

``` r
scholidonline::id_exists(
  x        = "10.1000/182",
  type     = "doi",
  provider = "crossref"
)
```

    ## [1] FALSE

If `provider = "auto"` (default), a sensible registry is chosen
automatically, potentially with fallback behavior.

Available providers depend on the identifier type and operation. Use
[`scholidonline_capabilities()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_capabilities.md)
to inspect them.

## Network considerations

Because `scholidonline` relies on external services:

- An internet connection is required
- Rate limits may apply
- Results may change over time
- Temporary failures can occur

For reproducible workflows:

- Cache results where appropriate
- Record provider choices
- Handle occasional `NA` values gracefully

## Relationship to scholid

`scholid` handles:

- Detection
- Normalization
- Classification
- Extraction

`scholidonline` handles:

- Registry-backed validation
- Cross-system conversion
- Metadata retrieval
- Link discovery

Together they form a clean two-layer design:

- Layer 1: Syntax and canonical form
- Layer 2: External registry interaction

## Session information

``` r
sessionInfo()
```

    ## R version 4.5.3 (2026-03-11)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] cli_3.6.5           knitr_1.51          rlang_1.1.7        
    ##  [4] xfun_0.57           textshaping_1.0.5   jsonlite_2.0.0     
    ##  [7] glue_1.8.0          htmltools_0.5.9     ragg_1.5.2         
    ## [10] sass_0.4.10         rmarkdown_2.30      rappdirs_0.3.4     
    ## [13] evaluate_1.0.5      jquerylib_0.1.4     fastmap_1.2.0      
    ## [16] yaml_2.3.12         lifecycle_1.0.5     httr2_1.2.2        
    ## [19] compiler_4.5.3      fs_2.0.1            scholid_0.1.0      
    ## [22] scholidonline_0.1.0 systemfonts_1.3.2   digest_0.6.39      
    ## [25] R6_2.6.1            curl_7.0.0          magrittr_2.0.4     
    ## [28] bslib_0.10.0        tools_4.5.3         pkgdown_2.2.0      
    ## [31] cachem_1.1.0        desc_1.4.3
