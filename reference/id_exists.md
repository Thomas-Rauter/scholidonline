# Check whether scholarly identifiers exist

Check whether identifiers resolve or are found in their respective
registries.

`id_exists()` is vectorized over `x`. If `type = "auto"`, the identifier
type is inferred per element using
[`scholid::detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.html).
Inputs that cannot be classified or normalized yield `NA`.

Provider-/ID-specific logic lives in internal helpers named
`.exists_<type>()` (e.g. `.exists_doi()`), which are dispatched to from
this front-end function.

## Usage

``` r
id_exists(
  x,
  type = c("auto", scholidonline_types()),
  provider = c("auto", .scholidonline_providers()),
  ...,
  quiet = FALSE
)
```

## Arguments

- x:

  A character vector of identifiers.

- type:

  A single identifier type string, or `"auto"` to infer the type for
  each element of `x`. See
  [`scholidonline_types()`](https://thomas-rauter.github.io/scholidonline/reference/scholidonline_types.md)
  for supported values.

- provider:

  A single provider string. Use `"auto"` to use the default provider for
  the resolved identifier type.

- ...:

  Reserved for future provider-specific arguments.

- quiet:

  A single logical value; if `TRUE`, suppress provider warnings/messages
  where possible.

## Value

A logical vector. `TRUE` indicates that the identifier exists, `FALSE`
indicates that it was confirmed not found, and `NA` indicates that the
input could not be classified, normalized, or checked reliably.

## Examples

``` r
if (FALSE) { # \dontrun{
id_exists("10.1000/182", type = "doi")
id_exists(c("12345678", "PMC12345"))
} # }
```
