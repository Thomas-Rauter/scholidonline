#' ORCID: check whether an ORCID exists
#'
#' @param x A single, normalized ORCID string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_orcid_orcid <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://pub.orcid.org/v3.0/",
    x
  )
  
  resp <- .scholidonline_http_get(
    url = url,
    headers = c(
      Accept = "application/json"
    ),
    quiet = quiet
  )

  .scholidonline_http_exists_from_response(
    resp = resp,
    quiet = quiet,
    provider_label = "ORCID"
  )
}


#' ORCID: return identifiers linked to an ORCID record
#'
#' @description
#' Provider adapter retrieving identifiers linked to an ORCID record
#' using the ORCID public API.
#'
#' @param x A single, normalized ORCID string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `linked_type`, `linked_value`, `provider`.
#'
#' @noRd
.links_orcid_orcid <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://pub.orcid.org/v3.0/",
    utils::URLencode(
      x,
      reserved = TRUE
    ),
    "/works"
  )
  
  json <- .scholidonline_http_get_json(
    url = url,
    quiet = quiet,
    provider_label = "ORCID",
    headers = c(
      Accept = "application/json"
    )
  )

  if (is.null(json)) {
    return(data.frame())
  }
  
  groups <- json$group
  
  if (is.null(groups) || length(groups) == 0L) {
    return(data.frame())
  }
  
  rows <- list()
  
  for (g in groups) {
    ids <- g$`external-ids`$`external-id`
    
    if (is.null(ids)) {
      next
    }
    
    for (id in ids) {
      type <- id$`external-id-type`
      value <- id$`external-id-value`
      
      if (is.null(type) || is.null(value) || !identical(type, "doi")) {
        next
      }
      
      rows[[length(rows) + 1L]] <- data.frame(
        linked_type = "doi",
        linked_value = as.character(value),
        provider = "orcid",
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(rows) == 0L) {
    return(data.frame())
  }
  
  do.call(rbind, rows)
}


#' ORCID: retrieve metadata for an ORCID record
#'
#' @description
#' Provider implementation for retrieving metadata for an ORCID record using
#' the ORCID public API.
#'
#' @param x A single, normalized ORCID identifier.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the ORCID record.
#'
#' @noRd
.meta_orcid_orcid <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://pub.orcid.org/v3.0/",
    utils::URLencode(
      x,
      reserved = TRUE
    ),
    "/record"
  )
  
  obj <- .scholidonline_http_get_json(
    url = url,
    quiet = quiet,
    provider_label = "ORCID",
    headers = c(
      Accept = "application/json"
    ),
    silent_404 = TRUE,
    simplifyVector = TRUE
  )

  if (is.null(obj)) {
    return(data.frame())
  }
  
  name <- obj$person$name
  full_name <- NA_character_
  
  if (!is.null(name)) {
    given <- name$`given-names`$value %||% ""
    family <- name$`family-name`$value %||% ""
    full_name <- trimws(paste(given, family))
  }
  
  data.frame(
    title = full_name,
    year = NA_integer_,
    container = NA_character_,
    doi = NA_character_,
    pmid = NA_character_,
    pmcid = NA_character_,
    url = paste0("https://orcid.org/", x),
    provider = "orcid",
    stringsAsFactors = FALSE
  )
}