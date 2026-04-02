#' Return identifiers linked to the same scholarly record
#'
#' @description
#' Return identifiers that external registries link to the same scholarly
#' record or to a closely corresponding version of it.
#'
#' @details
#' `id_links()` is vectorized over `x` and returns a long data.frame with one
#' row per discovered identifier link.
#'
#' Typical links include DOI <-> PMID, DOI <-> PMCID, PMID <-> PMCID,
#' arXiv ID <-> DOI, and ORCID -> DOI for works recorded in ORCID.
#'
#' Only identifier links explicitly exposed by the queried provider are
#' returned. `id_links()` does not retrieve general metadata or broader related
#' records unless the provider represents them as direct identifier links.
#'
#' Trivial self-links are excluded from the result.
#'
#' @param x A character vector of identifiers.
#' @param type A single identifier type string, or `"auto"` to infer the type
#'   for each element of `x`. See `scholidonline_types()` for supported values.
#' @param provider A single provider string specifying which online service to
#'   use. Use `"auto"` to use the default provider for the resolved identifier
#'   type. In most cases, `"auto"` is appropriate.
#' @param ... Reserved for future provider-specific arguments.
#' @param quiet A single logical value; if `TRUE`, suppress provider
#'   warnings and messages where possible.
#'
#' @return A data.frame with columns `query`, `query_type`, `linked_type`,
#'   `linked_id`, and `provider`. If no links are found, a zero-row
#'   data.frame with these columns is returned.
#'
#' @examples
#' \donttest{
#'   out <- id_links("31452104", provider = "epmc")
#'   knitr::kable(out)
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
    type = type,
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
        query = character(),
        query_type = character(),
        linked_type = character(),
        linked_id = character(),
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
    
    names(df)[names(df) == "linked_value"] <- "linked_id"
    
    df$query <- x_norm[ok_idx[j]]
    df$query_type <- type_vec[ok_idx[j]]
    
    df <- df[
      !(
        df$linked_type == df$query_type &
          df$linked_id == df$query
      ),
      ,
      drop = FALSE
    ]
    
    if (nrow(df) == 0L) {
      out_list[[ok_idx[j]]] <- NULL
      next
    }
    
    df <- df[
      ,
      c(
        "query",
        "query_type",
        "linked_type",
        "linked_id",
        "provider"
      ),
      drop = FALSE
    ]
    
    out_list[[ok_idx[j]]] <- df
  }
  
  rows <- out_list[!vapply(out_list, is.null, logical(1))]
  
  if (length(rows) == 0L) {
    return(
      data.frame(
        query = character(),
        query_type = character(),
        linked_type = character(),
        linked_id = character(),
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