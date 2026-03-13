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
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  url <- paste0(
    "https://pub.orcid.org/v3.0/",
    x
  )
  
  req <- httr2::request(url)
  req <- httr2::req_headers(
    .req = req,
    Accept = "application/json"
  )
  
  req <- httr2::req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- tryCatch(
    httr2::req_perform(
      req = req
    ),
    error = function(e) NULL
  )
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      warning(
        "ORCID request failed.",
        call. = FALSE
      )
    }
    return(NA)
  }
  
  status <- httr2::resp_status(
    resp = resp
  )
  
  if (status >= 200L && status < 300L) {
    return(TRUE)
  }
  
  if (status == 404L) {
    return(FALSE)
  }
  
  if (!isTRUE(quiet)) {
    warning(
      "ORCID request returned HTTP ",
      status,
      ".",
      call. = FALSE
    )
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
.links_orcid_orcid <- function(x, ..., quiet = FALSE) {
  .scholidonline_check_scalar_chr(x)
  
  url <- paste0(
    "https://pub.orcid.org/v3.0/",
    utils::URLencode(x, reserved = TRUE),
    "/works"
  )
  
  req <- httr2::request(url)
  
  req <- httr2::req_headers(
    .req = req,
    Accept = "application/json"
  )
  
  req <- httr2::req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) NULL
  )
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("ORCID request failed.")
    }
    return(data.frame())
  }
  
  status <- httr2::resp_status(resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("ORCID request returned HTTP ", status, ".")
      )
    }
    return(data.frame())
  }
  
  json <- tryCatch(
    httr2::resp_body_json(resp),
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
      
      if (is.null(type) || is.null(value)) {
        next
      }
      
      if (identical(type, "doi")) {
        
        rows[[length(rows) + 1L]] <- data.frame(
          linked_type  = "doi",
          linked_value = as.character(value),
          provider     = "orcid",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(rows) == 0L) {
    return(data.frame())
  }
  
  do.call(rbind, rows)
}