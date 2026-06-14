# Supported scholidonline capabilities

Return a summary of the capabilities supported by the scholidonline
package.

The returned table describes, for each supported identifier type:

- which single-identifier operations are available (`exists`, `links`,
  `meta`),

- which identifier conversions are available,

- which providers support each capability, and

- which provider is used by default when `provider = "auto"`.

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

- `target`: target identifier type for conversion operations, otherwise
  `NA`

- `providers`: comma-separated names of providers supporting the
  capability

- `default_provider`: default provider used when `provider = "auto"`

## Examples

``` r
caps <- scholidonline_capabilities()

subset(caps, type == "pmid" & operation == "convert")
#>    type operation target        providers default_provider
#> 31 pmid   convert    doi auto, ncbi, epmc             ncbi
#> 32 pmid   convert  pmcid auto, ncbi, epmc             ncbi

subset(caps, type == "doi" & target == "pmcid")
#>    type operation target        providers default_provider
#> 12  doi   convert  pmcid auto, ncbi, epmc             ncbi
```
