#' Crossref: check whether a DOI exists
#'
#' @param x A single, normalized DOI string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_doi_crossref <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  url <- paste0(
    "https://api.crossref.org/works/",
    utils::URLencode(
      x,
      reserved = TRUE
    )
  )
  
  req <- httr2::request(url)
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
        "Crossref request failed.",
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
      "Crossref request returned HTTP ",
      status,
      ".",
      call. = FALSE
    )
  }
  
  NA
}


#' Crossref: return identifiers linked to a DOI
#'
#' @description
#' Provider adapter retrieving identifiers linked to a DOI via the
#' Crossref REST API.
#'
#' The Crossref `works` endpoint is queried and known identifier fields
#' (PMID, PMCID, DOI relations) are extracted where available.
#'
#' @param x A single, normalized DOI string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `linked_type`, `linked_value`, `provider`.
#'
#' @noRd
.links_doi_crossref <- function(x, ..., quiet = FALSE) {
  .scholidonline_check_scalar_chr(x)
  
  url <- paste0(
    "https://api.crossref.org/works/",
    utils::URLencode(x, reserved = TRUE)
  )
  
  req <- httr2::request(url)
  
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
      rlang::warn("Crossref request failed.")
    }
    return(data.frame())
  }
  
  status <- httr2::resp_status(resp)
  
  if (status == 404L) {
    return(data.frame())
  }
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("Crossref request returned HTTP ", status, ".")
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
  
  msg <- json$message
  
  rows <- list()
  
  # PMID
  if (!is.null(msg$`pubmed-id`)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "pmid",
      linked_value = as.character(msg$`pubmed-id`),
      provider     = "crossref",
      stringsAsFactors = FALSE
    )
  }
  
  # PMCID
  if (!is.null(msg$`pmcid`)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "pmcid",
      linked_value = as.character(msg$pmcid),
      provider     = "crossref",
      stringsAsFactors = FALSE
    )
  }
  
  # relation DOIs
  if (!is.null(msg$relation)) {
    
    rel <- msg$relation
    
    for (rel_type in names(rel)) {
      
      rel_entries <- rel[[rel_type]]
      
      for (entry in rel_entries) {
        
        if (!is.null(entry$id)) {
          
          rows[[length(rows) + 1L]] <- data.frame(
            linked_type  = "doi",
            linked_value = as.character(entry$id),
            provider     = "crossref",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  if (length(rows) == 0L) {
    return(data.frame())
  }
  
  do.call(rbind, rows)
}