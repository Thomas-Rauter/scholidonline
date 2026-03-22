#' Retrieve scholarly metadata
#'
#' @description
#' Retrieve structured metadata for scholarly identifiers from external
#' registries.
#'
#' `id_metadata()` is vectorized over `x` and returns a data.frame with one row
#' per input element.
#'
#' The function returns a stable cross-provider subset of record-level metadata
#' for the queried identifier. It is intended to expose core bibliographic
#' fields such as title, publication year, container title, linked DOI, PMID,
#' and PMCID when available, and a canonical URL.
#'
#' If `type = "auto"`, the identifier type is inferred per element using
#' `scholid::detect_scholid_type()`. Inputs that cannot be classified or
#' normalized are returned as rows with `NA` metadata fields.
#'
#' Provider-/ID-specific logic lives in internal helpers named
#' `.meta_<type>()` (e.g. `.meta_pmid()`), which are dispatched to from this
#' front-end function.
#'
#' @param x A character vector of identifiers.
#' @param type A single identifier type string, or `"auto"` to infer the type
#'   for each element of `x`. See `scholidonline_types()` for supported values.
#' @param provider A single provider string. Use `"auto"` to use the default
#'   provider for the resolved identifier type.
#' @param fields An optional character vector of column names to return. If
#'   `NULL`, all default metadata columns are returned.
#' @param ... Reserved for future provider-specific arguments.
#' @param quiet A single logical value; if `TRUE`, suppress provider
#'   warnings/messages where possible.
#'
#' @return A data.frame with one row per input identifier. By default, the
#'   returned columns are `input`, `type`, `provider`, `title`, `year`,
#'   `container`, `doi`, `pmid`, `pmcid`, and `url`.
#'
#' @examples
#' \dontrun{
#' id_metadata("10.1038/nature12373", type = "doi")
#' id_metadata(c("31452104", "PMC6821181"))
#' id_metadata("10.1038/nature12373", fields = c("title", "year", "doi"))
#' }
#'
#' @export
id_metadata <- function(
    x,
    type = c("auto", scholidonline_types()),
    provider = c("auto", .scholidonline_providers()),
    fields = NULL,
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
  
  if (identical(type, "auto")) {
    type_vec <- scholid::detect_scholid_type(x = x)
    type_vec[!type_vec %in% scholidonline_types()] <- NA_character_
  } else {
    type_vec <- rep(type, n)
  }
  
  x_norm <- rep(NA_character_, n)
  
  for (i in seq_len(n)) {
    if (is.na(x[i]) || is.na(type_vec[i])) next
    
    x_norm[i] <- scholid::normalize_scholid(
      x = x[i],
      type = type_vec[i]
    )
  }
  
  ok <- !is.na(x_norm) & !is.na(type_vec)
  
  base_df <- data.frame(
    input = x,
    type = type_vec,
    provider = NA_character_,
    title = NA_character_,
    year = NA_integer_,
    container = NA_character_,
    doi = NA_character_,
    pmid = NA_character_,
    pmcid = NA_character_,
    url = NA_character_,
    stringsAsFactors = FALSE
  )
  
  if (!any(ok)) return(base_df)
  
  res <- .scholidonline_run_unary(
    x = x_norm[ok],
    operation = "meta",
    type = type_vec[ok],
    provider = provider,
    ...,
    quiet = quiet
  )
  
  ok_idx <- which(ok)
  
  for (j in seq_along(res)) {
    df <- res[[j]]
    
    if (is.null(df) || nrow(df) == 0L) next
    
    base_df$provider[ok_idx[j]] <- df$provider[1]
    base_df$title[ok_idx[j]] <- df$title[1]
    base_df$year[ok_idx[j]] <- df$year[1]
    base_df$container[ok_idx[j]] <- df$container[1]
    base_df$doi[ok_idx[j]] <- df$doi[1]
    base_df$pmid[ok_idx[j]] <- df$pmid[1]
    base_df$pmcid[ok_idx[j]] <- df$pmcid[1]
    base_df$url[ok_idx[j]] <- df$url[1]
  }
  
  if (!is.null(fields)) {
    keep <- fields[fields %in% names(base_df)]
    base_df <- base_df[, keep, drop = FALSE]
  }
  
  base_df
}