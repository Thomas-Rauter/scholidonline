# Supported scholidonline capabilities

Return a registry-backed summary of the capabilities supported by the
scholidonline package.

The returned table describes, for each supported identifier type, which
unary operations are available (`exists`, `links`, `meta`), which
identifier conversions are available, which providers support each
capability, and which provider is used by default when
`provider = "auto"`.

This function is useful for discovering what scholidonline can do for a
given identifier type or conversion pair.

## Usage

``` r
scholidonline_capabilities()
```

## Value

A data.frame with one row per supported capability and the following
columns:

- `type`: source identifier type

- `operation`: operation name (`exists`, `links`, `meta`, or `convert`)

- `target`: target identifier type for conversions, otherwise `NA`

- `providers`: comma-separated provider names

- `default_provider`: default provider used when `provider = "auto"`

## Examples

``` r
scholidonline_capabilities()
#>     type operation target               providers default_provider
#> 1  arxiv    exists   <NA>             auto, arxiv            arxiv
#> 2  arxiv     links   <NA>             auto, arxiv            arxiv
#> 3  arxiv      meta   <NA>             auto, arxiv            arxiv
#> 4    doi    exists   <NA> auto, doi.org, crossref          doi.org
#> 5    doi     links   <NA>          auto, crossref         crossref
#> 6    doi      meta   <NA> auto, crossref, doi.org         crossref
#> 7    doi   convert   pmid        auto, ncbi, epmc             ncbi
#> 8    doi   convert  pmcid        auto, ncbi, epmc             ncbi
#> 9  orcid    exists   <NA>             auto, orcid            orcid
#> 10 orcid     links   <NA>             auto, orcid            orcid
#> 11 orcid      meta   <NA>             auto, orcid            orcid
#> 12 pmcid    exists   <NA>        auto, ncbi, epmc             ncbi
#> 13 pmcid     links   <NA>        auto, ncbi, epmc             ncbi
#> 14 pmcid      meta   <NA>        auto, ncbi, epmc             ncbi
#> 15 pmcid   convert   pmid        auto, ncbi, epmc             ncbi
#> 16 pmcid   convert    doi        auto, ncbi, epmc             ncbi
#> 17  pmid    exists   <NA>        auto, ncbi, epmc             ncbi
#> 18  pmid     links   <NA>        auto, ncbi, epmc             ncbi
#> 19  pmid      meta   <NA>        auto, ncbi, epmc             ncbi
#> 20  pmid   convert    doi        auto, ncbi, epmc             ncbi
#> 21  pmid   convert  pmcid        auto, ncbi, epmc             ncbi

subset(
  scholidonline_capabilities(),
  type == "pmid" & operation == "convert"
)
#>    type operation target        providers default_provider
#> 20 pmid   convert    doi auto, ncbi, epmc             ncbi
#> 21 pmid   convert  pmcid auto, ncbi, epmc             ncbi

subset(
  scholidonline_capabilities(),
  type == "doi" & target == "pmcid"
)
#>   type operation target        providers default_provider
#> 8  doi   convert  pmcid auto, ncbi, epmc             ncbi
```
