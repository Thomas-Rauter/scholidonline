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
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_headers(
    req = req,
    Accept = "application/json"
  )
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("ORCID request failed.")
    }
    return(NA)
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status >= 200L && status < 300L) {
    return(TRUE)
  }
  
  if (status == 404L) {
    return(FALSE)
  }
  
  if (!isTRUE(quiet)) {
    rlang::warn(paste0("ORCID request returned HTTP ", status, "."))
  }
  
  NA
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
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_headers(
    req = req,
    Accept = "application/json"
  )
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("ORCID request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("ORCID request returned HTTP ", status, ".")
      )
    }
    return(data.frame())
  }
  
  json <- tryCatch(
    .scholidonline_resp_body_json(resp = resp),
    error = function(e) NULL
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
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_headers(
    req = req,
    Accept = "application/json"
  )
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("ORCID request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status == 404L) {
    return(data.frame())
  }
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("ORCID request returned HTTP ", status, "."))
    }
    return(data.frame())
  }
  
  obj <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = TRUE
  )
  
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