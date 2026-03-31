#' Convert scholarly identifiers across systems
#'
#' @description
#' Convert scholarly identifiers across registries, for example from PMID to
#' DOI.
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