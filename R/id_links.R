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
#' arXiv ID <-> DOI, ORCID -> DOI for works recorded in ORCID, and OpenAlex
#' work -> DOI, PMID, or PMCID where present in the OpenAlex record.
#'
#' Link discovery is not available for every supported identifier type; use
#' [scholidonline_capabilities()] to check whether `links` is supported.
#'
#' Only identifier links explicitly exposed by the queried provider are
#' returned. `id_links()` does not retrieve general metadata or broader related
#' records unless the provider represents them as direct identifier links.
#'
#' Trivial self-links are excluded from the result.
#'
#' `type` must be a single value or `"auto"`. For mixed identifier columns,
#' omit `type` or use `type = "auto"` so each element is classified
#' separately.
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

  prepared <- .scholidonline_prepare_inputs(
    x = x,
    type = type
  )

  out_list <- vector(
    mode = "list",
    length = n
  )

  if (!any(prepared$ok)) {
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
  
  x_ok <- prepared$x_norm[prepared$ok]
  type_ok <- prepared$type_vec[prepared$ok]
  
  res <- .scholidonline_run_unary(
    x = x_ok,
    operation = "links",
    type = type_ok,
    provider = provider,
    ...,
    quiet = quiet
  )
  
  for (j in seq_along(res)) {
    df <- res[[j]]
    idx <- prepared$ok_idx[j]

    if (is.null(df) || nrow(df) == 0L) {
      out_list[[idx]] <- NULL
      next
    }

    names(df)[names(df) == "linked_value"] <- "linked_id"

    df$query <- prepared$x_norm[idx]
    df$query_type <- prepared$type_vec[idx]
    
    df <- df[
      !(
        df$linked_type == df$query_type &
          df$linked_id == df$query
      ),
      ,
      drop = FALSE
    ]
    
    if (nrow(df) == 0L) {
      out_list[[idx]] <- NULL
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
    
    out_list[[idx]] <- df
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