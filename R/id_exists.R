#' Check whether scholarly identifiers exist
#'
#' @description
#' Check whether identifiers resolve or are found in their respective
#' registries.
#'
#' `id_exists()` is vectorized over `x`. If `type = "auto"`, the identifier
#' type is inferred per element using `scholid::detect_scholid_type()`.
#' Inputs that cannot be classified or normalized yield `NA`.
#'
#' Provider-/ID-specific logic lives in internal helpers named
#' `.exists_<type>()` (e.g. `.exists_doi()`), which are dispatched to from
#' this front-end function.
#'
#' @param x A character vector of identifiers.
#' @param type A single identifier type string, or `"auto"` to infer the type
#'   for each element of `x`. See `scholidonline_types()` for supported values.
#' @param provider A single provider string. Use `"auto"` to use the default
#'   provider for the resolved identifier type.
#' @param ... Reserved for future provider-specific arguments.
#' @param quiet A single logical value; if `TRUE`, suppress provider
#'   warnings/messages where possible.
#'
#' @return A logical vector. `TRUE` indicates that the identifier exists,
#'   `FALSE` indicates that it was confirmed not found, and `NA` indicates
#'   that the input could not be classified, normalized, or checked reliably.
#'
#' @examples
#' \dontrun{
#' id_exists("10.1000/182", type = "doi")
#' id_exists(c("12345678", "PMC12345"))
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
  
  if (identical(type, "auto")) {
    type_vec <- scholid::detect_scholid_type(
      x = x
    )
    type_vec[!type_vec %in% scholidonline_types()] <- NA_character_
  } else {
    type_vec <- rep(
      x = type,
      times = length(x)
    )
  }
  
  x_norm <- rep(
    x = NA_character_,
    times = length(x)
  )
  
  for (i in seq_along(x)) {
    
    if (is.na(x[i]) || is.na(type_vec[i])) {
      next
    }
    
    x_norm[i] <- scholid::normalize_scholid(
      x = x[i],
      type = type_vec[i]
    )
  }
  
  ok <- !is.na(x_norm) & !is.na(type_vec)
  
  if (!any(ok)) {
    return(out)
  }
  
  res <- .scholidonline_run_unary(
    x = x_norm[ok],
    operation = "exists",
    type = type_vec[ok],
    provider = provider,
    ...,
    quiet = quiet
  )
  
  out[ok] <- unlist(
    x = res,
    use.names = FALSE
  )
  
  out
}