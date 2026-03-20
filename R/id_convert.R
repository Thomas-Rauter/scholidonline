#' Convert scholarly identifiers across systems
#'
#' @description
#' Convert identifiers across registries (crosswalk), e.g. PMID -> DOI.
#'
#' `id_convert()` is vectorized over `x`. If `from` is `NULL`, the identifier
#' type is inferred per element using `scholid::detect_scholid_type()` after
#' normalizing (where possible). Inputs that cannot be classified or normalized
#' yield `NA_character_`.
#'
#' Provider-/ID-specific logic lives in internal helpers named
#' `convert_<from>_to_<to>()` (e.g., `convert_pmid_to_doi()`), which are
#' dispatched to from this front-door function.
#'
#' @param x A character vector of identifiers.
#' @param to A single string giving the target identifier type. See
#'   scholidonline_types()` for supported values.
#' @param from A single string giving the source identifier type, or `NULL` to
#'   infer per element.
#' @param provider Provider to use (e.g. "auto", "ncbi", "epmc", ...).
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A character vector of converted identifiers. Unconvertible or
#'   unclassified inputs yield `NA_character_`.
#'
#' @examples
#' \dontrun{
#' id_convert("12345678", to = "doi", from = "pmid")
#' id_convert(c("10.1000/182", "PMC12345"), to = "pmid") # infer `from`
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
  
  n <- length(x)
  
  if (is.null(from)) {
    from_vec <- scholid::detect_scholid_type(x)
    from_vec[!from_vec %in% scholidonline_types()] <- NA_character_
    
    from_vec[!vapply(
      from_vec,
      FUN = function(f) {
        if (is.na(f)) {
          return(FALSE)
        }
        
        if (identical(f, to)) {
          return(TRUE)
        }
        
        !is.null(.scholidonline_registry()[[f]]$convert[[to]])
      },
      FUN.VALUE = logical(1)
    )] <- NA_character_
    
  } else {
    from <- match.arg(
      arg = from,
      choices = scholidonline_types()
    )
    
    .scholidonline_check_conversion_pair(
      from = from,
      to = to
    )
    
    from_vec <- rep(from, n)
  }
  
  x_norm <- rep(NA_character_, n)
  
  for (i in seq_len(n)) {
    if (is.na(x[i]) || is.na(from_vec[i])) next
    
    x_norm[i] <- scholid::normalize_scholid(
      x = x[i],
      type = from_vec[i]
    )
  }
  
  ok <- !is.na(x_norm) & !is.na(from_vec)
  
  out <- rep(NA_character_, n)
  
  if (!any(ok)) return(out)
  
  res <- .scholidonline_run_binary(
    x = x_norm[ok],
    from = from_vec[ok],
    to = to,
    provider = provider,
    ...,
    quiet = quiet
  )
  
  out[ok] <- res
  
  out
}