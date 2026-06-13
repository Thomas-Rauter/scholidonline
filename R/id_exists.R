#' Check whether scholarly identifiers exist
#'
#' @description
#' Check whether scholarly identifiers are found in their respective
#' registries.
#'
#' @details
#' Existence checking is not available for every identifier type supported
#' by `scholid`. Use [scholidonline_capabilities()] to see which types
#' support the `exists` operation and which providers implement it.
#'
#' `type` must be a single value or `"auto"`. For mixed identifier columns,
#' omit `type` or use `type = "auto"` so each element is classified
#' separately.
#'
#' @param x A character vector of identifiers.
#' @param type A single identifier type string, or `"auto"` to infer the type
#'   for each element of `x`. See `scholidonline_types()` for supported values.
#' @param provider A single provider string specifying which online service to
#'   use for the lookup. Use `"auto"` to use the default provider for the
#'   resolved identifier type. In most cases, `"auto"` is appropriate.
#' @param ... Reserved for future provider-specific arguments.
#' @param quiet A single logical value; if `TRUE`, suppress provider
#'   warnings and messages where possible.
#'
#' @return A logical vector. `TRUE` indicates that the identifier was found,
#'   `FALSE` indicates that it was not found, and `NA` indicates that the
#'   input could not be identified, normalized, or checked reliably.
#'
#' @examples
#' \donttest{
#'   id_exists("10.1038/nature12373", type = "doi")
#'   id_exists(c("31452104", "PMC6784763"))
#' }
#'
#' @export
id_exists <- function(
    x,
    type = c("auto", scholidonline_types()),
    provider = c("auto", .scholidonline_providers()),
    ...,
    quiet = FALSE
) {
  .scholidonline_check_x(x)
  type <- match.arg(type)
  provider <- match.arg(provider)
  .scholidonline_check_type_provider(
    type     = type,
    provider = provider
  )
  .scholidonline_check_quiet(quiet)
  
  out <- rep(
    x = NA,
    times = length(x)
  )

  prepared <- .scholidonline_prepare_inputs(
    x = x,
    type = type
  )

  if (!any(prepared$ok)) {
    return(out)
  }

  x_ok <- prepared$x_norm[prepared$ok]
  type_ok <- prepared$type_vec[prepared$ok]
  
  res <- .scholidonline_run_unary(
    x = x_ok,
    operation = "exists",
    type = type_ok,
    provider = provider,
    ...,
    quiet = quiet
  )
  
  out[prepared$ok] <- unlist(
    x = res,
    use.names = FALSE
  )
  
  out
}