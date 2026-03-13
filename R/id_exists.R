#' Check whether scholarly identifiers exist
#'
#' @description
#' Check whether identifiers resolve or are found in their respective
#' registries.
#'
#' `id_exists()` is vectorized over `x`. If `type` is `NULL`, the identifier
#' type is inferred per element using `scholid::detect_scholid_type()`.
#' Inputs that cannot be classified or normalized yield `NA`.
#'
#' Provider-/ID-specific logic lives in internal helpers named
#' `.exists_<type>()` (e.g., `.exists_doi()`), which are dispatched to from
#' this front-door function.
#'
#' @param x A character vector of identifiers.
#' @param type A single string giving the identifier type, or `NULL` to infer
#'   per element. See `scholidonline::scholidonline_types()` for supported
#'   values.
#' @param provider Provider to use (e.g. `"auto"`, `"doi.org"`, `"crossref"`,
#'   `"ncbi"`, `"epmc"`, `"orcid"`, `"arxiv"`).
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A logical vector. `TRUE` if the identifier exists, `FALSE` if it is
#'   confirmed not found, and `NA` if the input cannot be classified,
#'   normalized, or checked reliably.
#'
#' @examples
#' \dontrun{
#' id_exists("10.1000/182", type = "doi")
#' id_exists(c("12345678", "PMC12345"))  # infer type
#' }
#'
#' @export
id_exists <- function(
    x,
    type = NULL,
    provider = "auto",
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_x(
    x = x,
    arg = "x"
  )
  
  quiet <- .scholidonline_check_quiet(
    quiet = quiet,
    arg = "quiet"
  )
  
  x <- as.character(x)
  out <- rep(
    x = NA,
    times = length(x)
  )
  
  supported_types <- scholidonline::scholidonline_types()
  
  if (is.null(type)) {
    
    type_vec <- scholid::detect_scholid_type(
      x = x
    )
    
    type_vec[!type_vec %in% supported_types] <- NA_character_
    
  } else {
    
    type <- .scholidonline_match_type(
      type = type,
      arg = "type",
      choices = supported_types
    )
    
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