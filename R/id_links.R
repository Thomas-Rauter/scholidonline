#' Return linked scholarly identifiers
#'
#' @description
#' Return identifier links that external registries associate with the same
#' scholarly object or a directly corresponding manifestation.
#'
#' `id_links()` is vectorized over `x` and returns a long data.frame with one
#' row per discovered identifier link.
#'
#' The function is intended to expose cross-registry identifier links such as:
#' - DOI ↔ PMID
#' - DOI ↔ PMCID
#' - PMID ↔ PMCID
#' - arXiv ID ↔ DOI
#' - ORCID → DOI for works recorded in ORCID
#'
#' Only identifier links explicitly exposed by the queried provider are
#' returned. `id_links()` is not a general metadata retrieval function and does
#' not attempt to return broader related records unless the provider represents
#' them as direct identifier links for the same object or directly corresponding
#' manifestation.
#'
#' If `type` is `NULL`, the identifier type is inferred per element using
#' `scholid::detect_scholid_type()`. Inputs that cannot be classified or
#' normalized yield zero rows.
#'
#' Provider-/ID-specific logic lives in internal helpers named
#' `.links_<type>()` (e.g. `.links_pmid()`), which are dispatched to from this
#' front-end function.
#'
#' @param x A character vector of identifiers.
#' @param type A single string giving the identifier type, or `NULL` to infer
#'   per element. See `scholidonline::scholidonline_types()` for supported
#'   values.
#' @param provider Provider to use (e.g. `"auto"`, `"crossref"`, `"ncbi"`,
#'   `"epmc"`, `"orcid"`, `"arxiv"`).
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame with columns:
#'   `input`, `input_type`, `linked_type`, `linked_value`, `provider`.
#'
#' @examples
#' \dontrun{
#' id_links("10.1000/182", type = "doi")
#' id_links(c("12345678", "PMC12345"))
#' }
#'
#' @export
id_links <- function(
    x,
    type = c("auto", scholidonline_types()),
    provider = c("auto", .scholidonline_providers()),
    ...,
    quiet = FALSE
){
  .scholidonline_check_x(x)
  type <- match.arg(type)
  provider <- match.arg(provider)
  .scholidonline_check_type_provider(
    type     = type,
    provider = provider
  )
  .scholidonline_check_quiet(quiet)
  
  n <- length(x)
  
  out_list <- vector(
    mode = "list",
    length = n
  )
  
  if (identical(type, "auto")) {
    type_vec <- scholid::detect_scholid_type(
      x = x
    )
    type_vec[!type_vec %in% scholidonline_types()] <- NA_character_
  } else {
    type_vec <- rep(
      x = type,
      times = n
    )
  }
  
  x_norm <- rep(
    x = NA_character_,
    times = n
  )
  
  for (i in seq_len(n)) {
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
    return(
      data.frame(
        input = character(),
        input_type = character(),
        linked_type = character(),
        linked_value = character(),
        provider = character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  res <- .scholidonline_run_unary(
    x = x_norm[ok],
    operation = "links",
    type = type_vec[ok],
    provider = provider,
    ...,
    quiet = quiet
  )
  
  ok_idx <- which(ok)
  
  for (j in seq_along(res)) {
    df <- res[[j]]
    
    if (is.null(df) || nrow(df) == 0L) {
      out_list[[ok_idx[j]]] <- NULL
      next
    }
    
    df$input <- x_norm[ok_idx[j]]
    df$input_type <- type_vec[ok_idx[j]]
    
    df <- df[
      ,
      c(
        "input",
        "input_type",
        "linked_type",
        "linked_value",
        "provider"
      )
    ]
    
    out_list[[ok_idx[j]]] <- df
  }
  
  rows <- out_list[!vapply(out_list, is.null, logical(1))]
  
  if (length(rows) == 0L) {
    return(
      data.frame(
        input = character(),
        input_type = character(),
        linked_type = character(),
        linked_value = character(),
        provider = character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  do.call(
    rbind,
    rows
  )
}