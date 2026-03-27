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
scholid::is_scholid(
  "10.1000/182",
  type = "doi"
  )
```

    ## [1] TRUE

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
scholidonline::id_exists(
  "10.1000/182",
  type = "doi"
  )
```

    ## [1] TRUE

Registry validation:

- Requires internet access
- Depends on external APIs
- May be affected by rate limits or temporary outages
- Can change over time

A structurally valid identifier may still fail registry validation.

``` r
sessionInfo()
```

    ## R version 4.5.3 (2026-03-11)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.4 LTS
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
    ## [10] sass_0.4.10         rmarkdown_2.31      rappdirs_0.3.4     
    ## [13] evaluate_1.0.5      jquerylib_0.1.4     fastmap_1.2.0      
    ## [16] yaml_2.3.12         lifecycle_1.0.5     httr2_1.2.2        
    ## [19] compiler_4.5.3      fs_2.0.1            scholid_0.1.0      
    ## [22] scholidonline_0.1.0 systemfonts_1.3.2   digest_0.6.39      
    ## [25] R6_2.6.1            curl_7.0.0          magrittr_2.0.4     
    ## [28] bslib_0.10.0        tools_4.5.3         pkgdown_2.2.0      
    ## [31] cachem_1.1.0        desc_1.4.3
