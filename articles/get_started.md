# Getting started with scholidonline

`scholidonline` provides online utilities for working with scholarly
identifiers. It builds on
[`scholid`](https://thomas-rauter.github.io/scholid/) for structural
detection and normalization, and adds registry-backed functionality such
as:

- Existence checks
- Identifier conversion across systems
- Metadata retrieval
- Retrieval of directly linked identifiers

This vignette introduces the interface and typical workflows when
working with registry-connected identifier data.

## Installation

``` r

install.packages("scholidonline")
```

## Interface

`scholidonline` exposes a small set of user-facing functions:

- [`scholidonline_types()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_types.md)
- [`scholidonline_capabilities()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_capabilities.md)
- [`id_exists()`](https://thomas-rauter.github.io/scholidonline/reference/id_exists.md)
- [`id_convert()`](https://thomas-rauter.github.io/scholidonline/reference/id_convert.md)
- [`id_metadata()`](https://thomas-rauter.github.io/scholidonline/reference/id_metadata.md)
- [`id_links()`](https://thomas-rauter.github.io/scholidonline/reference/id_links.md)

## Supported identifier types

You can inspect which identifier types are supported:

``` r

scholidonline::scholidonline_types()
#>  [1] "arxiv"      "assembly"   "bioproject" "doi"        "geo"       
#>  [6] "openalex"   "orcid"      "pmcid"      "pmid"       "refseq"    
#> [11] "ror"        "sra"        "uniprot"
```

## Inspecting capabilities

`scholidonline` is registry-driven. You can inspect all supported
operations, conversions, and providers:

``` r

out <- scholidonline::scholidonline_capabilities()
knitr::kable(out)
```

| type       | operation | target | providers               | default_provider |
|:-----------|:----------|:-------|:------------------------|:-----------------|
| arxiv      | exists    | NA     | auto, arxiv             | arxiv            |
| arxiv      | links     | NA     | auto, arxiv             | arxiv            |
| arxiv      | meta      | NA     | auto, arxiv             | arxiv            |
| assembly   | exists    | NA     | auto, ncbi              | ncbi             |
| assembly   | meta      | NA     | auto, ncbi              | ncbi             |
| bioproject | exists    | NA     | auto, ncbi              | ncbi             |
| bioproject | meta      | NA     | auto, ncbi              | ncbi             |
| doi        | exists    | NA     | auto, doi.org, crossref | doi.org          |
| doi        | links     | NA     | auto, crossref          | crossref         |
| doi        | meta      | NA     | auto, crossref, doi.org | crossref         |
| doi        | convert   | pmid   | auto, ncbi, epmc        | ncbi             |
| doi        | convert   | pmcid  | auto, ncbi, epmc        | ncbi             |
| geo        | exists    | NA     | auto, ncbi              | ncbi             |
| geo        | meta      | NA     | auto, ncbi              | ncbi             |
| openalex   | exists    | NA     | auto, openalex          | openalex         |
| openalex   | links     | NA     | auto, openalex          | openalex         |
| openalex   | meta      | NA     | auto, openalex          | openalex         |
| openalex   | convert   | doi    | auto, openalex          | openalex         |
| openalex   | convert   | pmid   | auto, openalex          | openalex         |
| orcid      | exists    | NA     | auto, orcid             | orcid            |
| orcid      | links     | NA     | auto, orcid             | orcid            |
| orcid      | meta      | NA     | auto, orcid             | orcid            |
| pmcid      | exists    | NA     | auto, ncbi, epmc        | ncbi             |
| pmcid      | links     | NA     | auto, ncbi, epmc        | ncbi             |
| pmcid      | meta      | NA     | auto, ncbi, epmc        | ncbi             |
| pmcid      | convert   | pmid   | auto, ncbi, epmc        | ncbi             |
| pmcid      | convert   | doi    | auto, ncbi, epmc        | ncbi             |
| pmid       | exists    | NA     | auto, ncbi, epmc        | ncbi             |
| pmid       | links     | NA     | auto, ncbi, epmc        | ncbi             |
| pmid       | meta      | NA     | auto, ncbi, epmc        | ncbi             |
| pmid       | convert   | doi    | auto, ncbi, epmc        | ncbi             |
| pmid       | convert   | pmcid  | auto, ncbi, epmc        | ncbi             |
| refseq     | exists    | NA     | auto, ncbi              | ncbi             |
| refseq     | meta      | NA     | auto, ncbi              | ncbi             |
| ror        | exists    | NA     | auto, ror               | ror              |
| ror        | meta      | NA     | auto, ror               | ror              |
| sra        | exists    | NA     | auto, ncbi              | ncbi             |
| sra        | meta      | NA     | auto, ncbi              | ncbi             |
| uniprot    | exists    | NA     | auto, uniprot           | uniprot          |
| uniprot    | meta      | NA     | auto, uniprot           | uniprot          |

Not every supported type offers every operation. For example, ROR and
UniProt support existence checks and metadata, while DOI and PMID also
support linked identifiers and conversion. To inspect one type:

``` r

out <- scholidonline::scholidonline_capabilities()
knitr::kable(subset(out, type == "openalex"))
```

|     | type     | operation | target | providers      | default_provider |
|:----|:---------|:----------|:-------|:---------------|:-----------------|
| 15  | openalex | exists    | NA     | auto, openalex | openalex         |
| 16  | openalex | links     | NA     | auto, openalex | openalex         |
| 17  | openalex | meta      | NA     | auto, openalex | openalex         |
| 18  | openalex | convert   | doi    | auto, openalex | openalex         |
| 19  | openalex | convert   | pmid   | auto, openalex | openalex         |

## Existence checks: `id_exists()`

[`id_exists()`](https://thomas-rauter.github.io/scholidonline/reference/id_exists.md)
verifies whether identifiers exist in their respective registries.

``` r

scholidonline::id_exists(
  x    = "10.1000/182",
  type = "doi"
)
#> [1] TRUE
```

If `type = NULL`, the type is inferred automatically:

``` r

scholidonline::id_exists(
  x = c(
    "10.1000/182",
    "12345678"
  )
)
#> [1] TRUE TRUE
```

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
#> [1] "10.1234/2013/999990"
```

If `from = NULL`, the source type is inferred per element:

``` r

scholidonline::id_convert(
  x = c("12345678", "PMC1234567"),
  to = "doi"
)
#> [1] "10.1234/2013/999990"              "10.1097/00000658-199503000-00007"
```

Unresolvable mappings return `NA_character_`.

## Metadata retrieval: `id_metadata()`

[`id_metadata()`](https://thomas-rauter.github.io/scholidonline/reference/id_metadata.md)
retrieves harmonized metadata from external registries.

``` r

out <- scholidonline::id_metadata(
  x    = "10.1038/nature12373",
  type = "doi"
)
knitr::kable(out)
```

| input | type | provider | title | year | container | doi | pmid | pmcid | url |
|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|
| 10.1038/nature12373 | doi | crossref | Nanometre-scale thermometry in a living cell | 2013 | Nature | 10.1038/nature12373 | NA | NA | <https://doi.org/10.1038/nature12373> |

Metadata completeness depends on the registry. For NCBI accession types
such as BioProject, `title` is the short registry title from Entrez
ESummary, not the full project description on the NCBI website; use
`url` for the complete record.

You can restrict returned fields:

``` r

out <- scholidonline::id_metadata(
  x = "10.1038/nature12373",
  type = "doi",
  fields = c("title", "year", "doi")
)
knitr::kable(out)
```

| title                                        | year | doi                 |
|:---------------------------------------------|-----:|:--------------------|
| Nanometre-scale thermometry in a living cell | 2013 | 10.1038/nature12373 |

## Linked identifiers: `id_links()`

[`id_links()`](https://thomas-rauter.github.io/scholidonline/reference/id_links.md)
returns related identifiers discovered via registry queries. Returns an
empty table when the provider exposes no linked identifiers for that
record.

``` r

out <- scholidonline::id_links(
  x    = "PMC1234567",
  type = "pmcid"
)
knitr::kable(out)
```

|  | query | query_type | linked_type | linked_id | provider |
|:---|:---|:---|:---|:---|:---|
| 1 | PMC1234567 | pmcid | pmid | 7717779 | ncbi |
| 3 | PMC1234567 | pmcid | doi | 10.1097/00000658-199503000-00007 | ncbi |

The result is a long data.frame with one row per link. When no links are
found, the same columns are returned with zero rows.

## Working with mixed data

A common workflow for messy identifier columns:

1.  Detect identifier types (via `scholid`)
2.  Normalize identifiers
3.  Check registry existence

Example:

``` r

x <- c(
  "https://doi.org/10.1000/182",
  "PMCID: PMC1234567",
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
#> [1] "doi"   "pmcid" NA
x_norm
#> [1] "10.1000/182" "PMC1234567"  NA
```

`id_exists(x)` below uses the default `type = "auto"`, so each element
is classified and normalized automatically. You do not need to pass a
vector `type` argument.

``` r

scholidonline::id_exists(x)
#> [1] TRUE TRUE   NA
```

## Provider selection

Most functions accept a `provider` argument.

``` r

scholidonline::id_exists(
  x        = "10.1000/182",
  type     = "doi",
  provider = "crossref"
)
#> [1] FALSE

scholidonline::id_exists(
  x        = "10.1000/182",
  type     = "doi",
  provider = "doi.org"
)
#> [1] TRUE
```

If `provider = "auto"` (default), a sensible registry is chosen
automatically, potentially with fallback behavior.

Available providers depend on the identifier type and operation. Use
[`scholidonline_capabilities()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_capabilities.md)
to inspect them.

The chosen provider affects:

- Response speed
- Metadata richness
- Crosswalk coverage

## Scope of scholidonline

`scholidonline` focuses on identifier types with stable public
registries and accessible APIs. The package supports online operations
for:

- **Bibliographic core:** DOI, PMID, PMCID, arXiv
- **Graph and people:** OpenAlex, ORCID
- **Organizations:** ROR
- **Life science:** UniProt; NCBI accessions (GEO, BioProject, RefSeq,
  SRA, genome assembly)

Not every type supports every operation. For example, ROR and UniProt
support existence checks and metadata, while DOI and PMID additionally
support linked identifiers and conversion. Use
[`scholidonline_capabilities()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_capabilities.md)
as the authoritative summary.

Many other identifier types (e.g., ISBN, ISSN, bibcode, RRID) are
structurally supported by `scholid`, but are not covered by
`scholidonline` because they lack a stable, open registry API fit for
this package.
