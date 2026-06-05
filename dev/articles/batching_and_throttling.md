# Provider etiquette: batching and throttling

## Why provider etiquette matters

`scholidonline` queries live external scholarly registries. These
providers are useful public infrastructure, but they are not unlimited
local databases. They may rate-limit requests, slow down responses, or
temporarily refuse access when many requests arrive in a short time.

For this reason, `scholidonline` tries to access providers efficiently
and politely. Two mechanisms are especially important:

- **Batching**, where multiple identifiers are resolved with one
  provider request when the provider supports it.
- **Throttling**, where the package waits between provider requests when
  needed.

Users usually do not need to manage these details manually. The exported
functions remain vectorized, and the return shape is the same regardless
of whether a provider request was scalar or batched internally.

Prefer vectorized calls such as:

``` r

scholidonline::id_exists(
  c("31452104", "999999999"),
  type = "pmid",
  provider = "ncbi"
)
```

    ## [1]  TRUE FALSE

over manual loops such as:

``` r

vapply(
  c("31452104", "999999999"),
  function(x) {
    scholidonline::id_exists(
      x,
      type = "pmid",
      provider = "ncbi"
    )
  },
  logical(1)
)
```

    ## Waiting 0.04 seconds before NCBI request.
    ## Waiting 0.23 seconds before NCBI request.

    ##  31452104 999999999 
    ##      TRUE     FALSE

Vectorized calls give the package an opportunity to use
provider-supported batching and avoid unnecessary repeated requests.

Provider etiquette is especially relevant for scripted workflows, large
identifier vectors, repeated checks during development, and automated
tests that query live services. Even when each individual request is
valid, many rapid requests can make a provider temporarily unavailable
for the current session or client.

## Batching

Batching means that `scholidonline` may resolve multiple identifiers
using a single provider request. This is an internal optimization. It
does not change the public API or the shape of returned objects.

For example,
[`id_exists()`](https://thomas-rauter.github.io/scholidonline/reference/id_exists.md)
still returns one logical value per input:

``` r

scholidonline::id_exists(
  c("31452104", "999999999", NA_character_),
  type = "pmid",
  provider = "ncbi"
)
```

    ## Waiting 0.2 seconds before NCBI request.

    ## Warning: HTTP request failed (429):
    ## https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi

    ## [1] NA NA NA

Likewise,
[`id_metadata()`](https://thomas-rauter.github.io/scholidonline/reference/id_metadata.md)
still returns one row per input identifier:

``` r

scholidonline::id_metadata(
  c("31452104", "999999999", NA_character_),
  type = "pmid",
  provider = "ncbi"
)
```

    ## Waiting 0.23 seconds before NCBI request.

    ##       input type provider                               title year
    ## 1  31452104 pmid     ncbi Molegro Virtual Docker for Docking. 2019
    ## 2 999999999 pmid     <NA>                                <NA>   NA
    ## 3      <NA> pmid     <NA>                                <NA>   NA
    ##          container  doi     pmid pmcid
    ## 1 Methods Mol Biol <NA> 31452104  <NA>
    ## 2             <NA> <NA>     <NA>  <NA>
    ## 3             <NA> <NA>     <NA>  <NA>
    ##                                         url
    ## 1 https://pubmed.ncbi.nlm.nih.gov/31452104/
    ## 2                                      <NA>
    ## 3                                      <NA>

[`id_links()`](https://thomas-rauter.github.io/scholidonline/reference/id_links.md)
still returns a long data frame of discovered links:

``` r

scholidonline::id_links(
  c("PMC6784763", "PMC999999999", NA_character_),
  type = "pmcid",
  provider = "ncbi"
)
```

    ## Waiting 0.22 seconds before NCBI request.

    ##        query query_type linked_type                    linked_id provider
    ## 1 PMC6784763      pmcid        pmid                     31469695     ncbi
    ## 3 PMC6784763      pmcid         doi 10.1097/EDE.0000000000001091     ncbi

And
[`id_convert()`](https://thomas-rauter.github.io/scholidonline/reference/id_convert.md)
still returns one converted identifier per input:

``` r

scholidonline::id_convert(
  c("31469695", "999999999", NA_character_),
  from = "pmid",
  to = "pmcid",
  provider = "ncbi"
)
```

    ## Waiting 0.18 seconds before NCBI request.

    ## [1] "PMC6784763" NA           NA

Batching is provider- and operation-specific. Some providers offer clean
multi-identifier endpoints; others do not. `scholidonline` uses batching
only where the provider interface supports reliable mapping back to the
original input identifiers.

For example, batching is used for selected arXiv operations and for
selected NCBI-backed PMID, PMCID, and DOI operations. These include
existence checks, metadata retrieval, linked-identifier lookup, and
supported identifier conversions where the provider response can be
mapped safely back to the input vector.

When batching is not available, the package falls back to scalar
provider calls while preserving the same public return contract. This
means users can write the same vectorized code regardless of whether a
provider currently supports a batch endpoint for that operation.

Batching also helps with provider etiquette because one request for a
vector of identifiers is usually preferable to one request per
identifier. For this reason, vectorized calls should generally be
preferred over manual loops.

## Throttling

Throttling means that `scholidonline` may wait before making a provider
request. The first request to a provider usually runs immediately. Later
requests may wait if they occur too soon after the previous request.

Package-managed rate limiting is enabled by default:

``` r

options(scholidonline.rate_limit = TRUE)
```

Users can disable package-managed waiting:

``` r

options(scholidonline.rate_limit = FALSE)
```

Provider-specific intervals can also be adjusted. For example, arXiv
access is intentionally conservative:

``` r

options(scholidonline.arxiv.min_interval = 3)
```

NCBI requests use a shorter default interval:

``` r

options(scholidonline.ncbi.min_interval = 0.34)
```

Europe PMC requests can also be controlled separately:

``` r

options(scholidonline.epmc.min_interval = 1)
```

These options affect future requests in the current R session. They do
not change the meaning of results.

The rate limiter is process-local. It tracks requests made in the
current R session. It is not shared across parallel R sessions,
background R processes, or separate machines. If you run highly parallel
code, each R process may have its own rate-limit state.

A provider failure is not the same as a confirmed absence. In
[`id_exists()`](https://thomas-rauter.github.io/scholidonline/reference/id_exists.md),
the return values have distinct meanings:

- `TRUE`: the provider returned usable evidence that the identifier
  exists.
- `FALSE`: the provider returned usable evidence that the identifier
  does not exist.
- `NA`: the identifier could not be checked reliably, for example
  because it could not be normalized, the provider was unavailable, or
  the provider response could not be interpreted safely.

This distinction matters for live services. A temporary rate-limit
response, service outage, malformed response, or network failure should
not be treated as evidence that an identifier does not exist. In such
cases, `NA` is the safer result.

For normal use, it is best to keep rate limiting enabled and to prefer
vectorized calls over manual loops. Users who need stricter provider
etiquette can increase the provider-specific intervals. Users who
already manage request pacing externally can disable package-managed
waiting with `options(scholidonline.rate_limit = FALSE)`.
