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
#> [1] TRUE
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
scholidonline::id_exists(
  "10.1000/182",
  type = "doi"
  )
#> [1] TRUE
```

Registry validation:

- Requires internet access
- Depends on external APIs
- May be affected by rate limits or temporary outages
- Can change over time

A structurally valid identifier may still fail registry validation.
