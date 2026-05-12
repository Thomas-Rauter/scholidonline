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

## scholidonline::id_exists(

## c(“31452104”, “999999999”),

## type = “pmid”,

## provider = “ncbi”

## )

over manual loops such as:

## vapply(

## c(“31452104”, “999999999”),

## function(x) {

## scholidonline::id_exists(

## x,

## type = “pmid”,

## provider = “ncbi”

## )

## },

## logical(1)

## )

Vectorized calls give the package an opportunity to use
provider-supported batching and avoid unnecessary repeated requests.

## Batching

Batching means that `scholidonline` may resolve multiple identifiers
using a single provider request. This is an internal optimization. It
does not change the public API or the shape of returned objects.

For example,
[`id_exists()`](https://thomas-rauter.github.io/scholidonline/dev/reference/id_exists.md)
still returns one logical value per input:

## scholidonline::id_exists(

## c(“31452104”, “999999999”, NA_character\_),

## type = “pmid”,

## provider = “ncbi”

## )

Likewise,
[`id_metadata()`](https://thomas-rauter.github.io/scholidonline/dev/reference/id_metadata.md)
still returns one row per input identifier:

## scholidonline::id_metadata(

## c(“31452104”, “999999999”, NA_character\_),

## type = “pmid”,

## provider = “ncbi”

## )

[`id_links()`](https://thomas-rauter.github.io/scholidonline/dev/reference/id_links.md)
still returns a long data frame of discovered links:

## scholidonline::id_links(

## c(“PMC6784763”, “PMC999999999”, NA_character\_),

## type = “pmcid”,

## provider = “ncbi”

## )

And
[`id_convert()`](https://thomas-rauter.github.io/scholidonline/dev/reference/id_convert.md)
still returns one converted identifier per input:

## scholidonline::id_convert(

## c(“31469695”, “999999999”, NA_character\_),

## from = “pmid”,

## to = “pmcid”,

## provider = “ncbi”

## )

Batching is provider- and operation-specific. Some providers offer clean
multi-identifier endpoints; others do not. `scholidonline` uses batching
only where the provider interface supports reliable mapping back to the
original input identifiers.

When batching is not available, the package falls back to scalar
provider calls while preserving the same public return contract.

## Throttling

Throttling means that `scholidonline` may wait before making a provider
request. The first request to a provider usually runs immediately. Later
requests may wait if they occur too soon after the previous request.

Package-managed rate limiting is enabled by default:

## options(scholidonline.rate_limit = TRUE)

Users can disable package-managed waiting:

## options(scholidonline.rate_limit = FALSE)

Provider-specific intervals can also be adjusted. For example, arXiv
access is intentionally conservative:

## options(scholidonline.arxiv.min_interval = 3)

NCBI requests use a shorter default interval:

## options(scholidonline.ncbi.min_interval = 0.34)

These options affect future requests in the current R session. They do
not change the meaning of results.

A provider failure is not the same as a confirmed absence. In
[`id_exists()`](https://thomas-rauter.github.io/scholidonline/dev/reference/id_exists.md),
the return values have distinct meanings:

- `TRUE`: the provider returned usable evidence that the identifier
  exists.
- `FALSE`: the provider returned usable evidence that the identifier
  does not exist.
- `NA`: the identifier could not be checked reliably, for example
  because it could not be normalized, the provider was unavailable, or
  the provider response could not be interpreted safely.

For normal use, it is best to keep rate limiting enabled and to prefer
vectorized calls over manual loops.
