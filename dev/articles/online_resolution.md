# Online Resolution and Registry Validation

## Introduction

This vignette explains what it means to validate scholarly identifiers
against external registries and how `scholidonline` relates to
`scholid`.

When working with identifiers programmatically, it is essential to
distinguish between three levels of validity:

- Structural validity
- Checksum validity
- Registry validity

`scholid` operates at the structural (and, where applicable, checksum)
level.

`scholidonline` operates at the registry level.

These are fundamentally different layers.

------------------------------------------------------------------------

## Structural vs Registry Validity

### Structural Validity

Structural validity answers:

- Does this string match the formal grammar of an identifier system?

Example:

``` r
scholid::is_scholid("10.1000/182", type = "doi")
```

Structural validation uses regular expressions and, where applicable,
checksum algorithms (e.g., ORCID).

It does **not** require internet access.

It does **not** confirm existence.

------------------------------------------------------------------------

### Registry Validity

Registry validity answers:

- Does this identifier exist in an external authority?

For example:

- Does this DOI resolve via doi.org or Crossref?
- Does this PMID exist in PubMed?
- Is this ORCID iD present in the ORCID registry?

Example:

``` r
scholidonline::id_exists("10.1000/182", type = "doi")
```

Registry validation:

- Requires internet access
- Depends on external APIs
- May be affected by rate limits or temporary outages
- Can change over time

A structurally valid identifier may still fail registry validation.

------------------------------------------------------------------------

## Existence Checks

The function
[`id_exists()`](https://thomas-rauter.github.io/scholidonline/reference/id_exists.md)
performs registry-backed validation.

Example:

``` r
scholidonline::id_exists(
  x    = "12345678",
  type = "pmid"
)
```

Possible return values:

- TRUE → confirmed by registry
- FALSE → registry confirms not found
- NA → input could not be classified or normalized

Importantly, `NA` does not mean “does not exist”. It means “cannot be
evaluated”.

------------------------------------------------------------------------

## Identifier Conversion

Many scholarly identifiers are linked across systems.

Examples:

- PMID → DOI
- PMCID → PMID
- arXiv → DOI (when available)

[`id_convert()`](https://thomas-rauter.github.io/scholidonline/reference/id_convert.md)
performs registry-backed crosswalks.

Example:

``` r
scholidonline::id_convert(
  x    = "12345678",
  from = "pmid",
  to   = "doi"
)
```

Conversions depend on:

- Metadata availability
- Registry completeness
- Provider selection

Not all identifiers can be converted.

Unresolvable mappings return NA.

------------------------------------------------------------------------

## Metadata Retrieval

[`id_metadata()`](https://thomas-rauter.github.io/scholidonline/reference/id_metadata.md)
retrieves structured metadata from registries.

Example:

``` r
scholidonline::id_metadata(
  x    = "10.1000/182",
  type = "doi"
)
```

Returned metadata is:

- Registry-derived
- Minimal and harmonized across providers
- Subject to external data quality

Metadata completeness depends entirely on the external authority.

------------------------------------------------------------------------

## Linked Identifiers

[`id_links()`](https://thomas-rauter.github.io/scholidonline/reference/id_links.md)
returns related identifiers discovered via registry queries.

Example:

``` r
scholidonline::id_links(
  x    = "PMC1234567",
  type = "pmcid"
)
```

This may include:

- DOI
- PMID
- PMCID
- Other linked registry identifiers

Link discovery is registry-dependent and may vary across providers.

------------------------------------------------------------------------

## Provider Selection

Many functions accept a `provider` argument.

Example:

``` r
scholidonline::id_exists(
  x        = "10.1000/182",
  type     = "doi",
  provider = "crossref"
)
```

If `provider = "auto"` (default), `scholidonline` selects a sensible
default and may fall back to alternative providers.

Provider behavior:

- DOI: doi.org or Crossref
- PMID / PMCID: NCBI or Europe PMC
- ORCID: ORCID public API
- arXiv: arXiv API

The exact provider affects:

- Response speed
- Metadata richness
- Crosswalk coverage

------------------------------------------------------------------------

## Network Considerations

Because `scholidonline` relies on external services:

- An internet connection is required.
- Rate limits may apply.
- APIs may change.
- Results may differ over time.

For reproducible pipelines:

- Cache results when appropriate.
- Record provider choices.
- Expect occasional transient failures.

------------------------------------------------------------------------

## Scope of scholidonline

`scholidonline` focuses on identifiers that have:

- Stable public registries
- Accessible APIs
- Meaningful cross-system relationships

Examples:

- DOI
- PMID
- PMCID
- ORCID
- arXiv

Other identifiers (e.g., ISBN, ISSN) are structurally supported by
`scholid`, but do not always have stable, open registry APIs suitable
for lightweight CRAN-friendly integration.

------------------------------------------------------------------------

## Relationship to scholid

`scholid` provides:

- Structural detection
- Normalization
- Classification
- Extraction

`scholidonline` provides:

- Registry-backed validation
- Cross-identifier conversion
- Metadata retrieval
- Link discovery

Together, they form a two-layer design:

Layer 1: Syntax and canonical form (`scholid`) Layer 2: External
registry interaction (`scholidonline`)

This separation keeps both packages small, predictable, and
maintainable.

------------------------------------------------------------------------

## Summary

Structural validity and registry validity are not the same.

`scholid` ensures identifiers are well-formed.

`scholidonline` checks whether they exist, resolve, and connect within
the scholarly registry ecosystem.

Use the appropriate layer depending on whether you need:

- Offline structural guarantees
- Or online registry-backed verification.

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
    ##  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
    ##  [5] xfun_0.57         cachem_1.1.0      knitr_1.51        htmltools_0.5.9  
    ##  [9] rmarkdown_2.30    lifecycle_1.0.5   cli_3.6.5         sass_0.4.10      
    ## [13] pkgdown_2.2.0     textshaping_1.0.5 jquerylib_0.1.4   systemfonts_1.3.2
    ## [17] compiler_4.5.3    tools_4.5.3       ragg_1.5.2        evaluate_1.0.5   
    ## [21] bslib_0.10.0      yaml_2.3.12       jsonlite_2.0.0    rlang_1.1.7      
    ## [25] fs_2.0.0
