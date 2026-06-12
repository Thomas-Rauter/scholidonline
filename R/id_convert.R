#' Convert scholarly identifiers across systems
#'
#' @description
#' Convert scholarly identifiers across registries, for example from PMID to
#' DOI.
#'
#' @details
#' Only some source/target type pairs are supported. Use
#' [scholidonline_capabilities()] with `operation = "convert"` (or filter the
#' returned table) to see which conversions are available and which providers
#' implement them.
#'
#' @param x A character vector of scholarly identifiers.
#' @param to A single target identifier type string, such as `"doi"` or
#'   `"pmid"`. See `scholidonline_types()` for all supported values.
#' @param from A single source identifier type string, or `NULL` to infer the
#'   source type for each element of `x`.
#' @param provider A single provider string specifying which online service to
#'   use for the conversion. Use `"auto"` to use the default provider for the
#'   requested conversion. In most cases, `"auto"` is appropriate.
#' @param ... Reserved for future provider-specific arguments.
#' @param quiet A single logical value; if `TRUE`, suppress provider
#'   warnings and messages where possible.
#'
#' @return A character vector of converted identifiers. Elements that cannot
#'   be identified, normalized, or converted return `NA_character_`.
#'
#' @examples
#' \donttest{
#'   id_convert("12345678", to = "doi", from = "pmid")
#'   id_convert("10.1038/nature12373", to = "pmid", from = "doi")
#' }
#'
#' @export
id_convert <- function(
    x,
    to = scholidonline_types(),
    from = NULL,
    provider = c("auto", .scholidonline_providers()),
    ...,
    quiet = FALSE
) {
  .scholidonline_check_x(x)
  to <- match.arg(to)
  provider <- match.arg(provider)
  .scholidonline_check_quiet(quiet)
  
  if (is.null(from)) {
    prepared <- .scholidonline_prepare_inputs(
      x = x,
      type = "auto",
      to = to
    )
  } else {
    from <- match.arg(
      arg = from,
      choices = scholidonline_types()
    )

    .scholidonline_check_conversion_pair(
      from = from,
      to = to
    )

    prepared <- .scholidonline_prepare_inputs(
      x = x,
      type = from
    )
  }

  out <- rep(NA_character_, length(x))

  if (!any(prepared$ok)) {
    return(out)
  }

  res <- .scholidonline_run_binary(
    x = prepared$x_norm[prepared$ok],
    from = prepared$type_vec[prepared$ok],
    to = to,
    provider = provider,
    ...,
    quiet = quiet
  )

  out[prepared$ok] <- res
  
  out
}