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

### Installation

## install.packages(“scholidonline”)

`scholid` will be installed automatically as a dependency.

### Interface

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

### Existence checks: `id_exists()`

[`id_exists()`](https://thomas-rauter.github.io/scholidonline/reference/id_exists.md)
verifies whether identifiers exist in their respective registries.

``` r
scholidonline::id_exists(
  x    = "10.1000/182",
  type = "doi"
)
```

If `type = NULL`, the type is inferred automatically:

``` r
scholidonline::id_exists(
  x = c(
    "10.1000/182",
    "12345678"
  )
)
```

Return values:

- TRUE → confirmed by registry
- FALSE → confirmed not found
- NA → cannot be classified or normalized

### Conversion: `id_convert()`

Many scholarly identifiers are cross-linked across systems.

Common examples:

- PMID → DOI
- PMCID → PMID
- arXiv → DOI (when available)

``` r
scholidonline::id_convert(
  x    = "12345678",
  from = "pmid",
  to   = "doi"
)
```

If `from = NULL`, the source type is inferred per element.

Unresolvable mappings return `NA_character_`.

### Metadata retrieval: `id_metadata()`

[`id_metadata()`](https://thomas-rauter.github.io/scholidonline/reference/id_metadata.md)
retrieves harmonized metadata from external registries.

``` r
scholidonline::id_metadata(
  x    = "10.1000/182",
  type = "doi"
)
```

Returned metadata is registry-derived and minimally harmonized across
providers.

Typical columns include:

- input
- type
- provider
- title
- year
- authors
- container
- doi
- pmid
- pmcid
- url

Metadata completeness depends on the external authority.

### Linked identifiers: `id_links()`

[`id_links()`](https://thomas-rauter.github.io/scholidonline/reference/id_links.md)
returns related identifiers discovered via registry queries.

``` r
scholidonline::id_links(
  x    = "PMC1234567",
  type = "pmcid"
)
```

This may reveal:

- DOI
- PMID
- PMCID
- Other linked identifiers

The result is a long data.frame with one row per link.

### Working with mixed data

A common workflow for messy identifier columns:

1.  Detect identifier types (via `scholid`).
2.  Normalize identifiers.
3.  Check registry existence.
4.  Convert or enrich with metadata.

Example:

``` r
x <- c(
  "https://doi.org/10.1000/182",
  "PMID: 12345678",
  "not an id"
)

types <- scholid::detect_scholid_type(x)
x_norm <- mapply(
  scholid::normalize_scholid,
  x    = x,
  type = types,
  SIMPLIFY = TRUE
)

scholidonline::id_exists(x_norm, type = types)
```

This separation keeps structural logic (`scholid`) and registry logic
(`scholidonline`) clearly distinct.

### Provider selection

Most functions accept a `provider` argument.

``` r
scholidonline::id_exists(
  x        = "10.1000/182",
  type     = "doi",
  provider = "crossref"
)
```

If `provider = "auto"` (default), a sensible registry is chosen
automatically.

Available providers depend on the identifier type.

### Network considerations

Because `scholidonline` relies on external services:

- An internet connection is required.
- Rate limits may apply.
- Results may change over time.
- Temporary failures can occur.

For reproducible workflows:

- Cache results where appropriate.
- Record provider choices.
- Handle occasional `NA` values gracefully.

### Relationship to scholid

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

### Session information

## sessionInfo()
