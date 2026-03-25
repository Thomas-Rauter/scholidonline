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
  
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://api.crossref.org/works/",
    utils::URLencode(
      x,
      reserved = TRUE
    )
  )
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("Crossref request failed.")
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
    rlang::warn(paste0("Crossref request returned HTTP ", status, "."))
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
.links_doi_crossref <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://api.crossref.org/works/",
    utils::URLencode(
      x,
      reserved = TRUE
    )
  )
  
  req <- .scholidonline_request(url)
  
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("Crossref request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
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
    .scholidonline_resp_body_json(resp = resp),
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
      linked_type      = "pmid",
      linked_value     = as.character(msg$`pubmed-id`),
      provider         = "crossref",
      stringsAsFactors = FALSE
    )
  }
  
  # PMCID
  if (!is.null(msg$`pmcid`)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type      = "pmcid",
      linked_value     = as.character(msg$pmcid),
      provider         = "crossref",
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
            linked_type      = "doi",
            linked_value     = as.character(entry$id),
            provider         = "crossref",
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


#' Crossref: retrieve metadata for a DOI
#'
#' @description
#' Provider implementation for retrieving metadata for a DOI using the
#' Crossref API.
#'
#' @param x A single, normalized DOI string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the DOI.
#'
#' @noRd
.meta_doi_crossref <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://api.crossref.org/works/",
    utils::URLencode(
      x,
      reserved = TRUE
    )
  )
  
  req <- .scholidonline_request(url)
  
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("Crossref request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status == 404L) {
    return(data.frame())
  }
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("Crossref request returned HTTP ", status, "."))
    }
    return(data.frame())
  }
  
  obj <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = TRUE
  )
  
  msg <- obj$message
  
  data.frame(
    title = if (!is.null(msg$title)) {
      msg$title[[1]]
    } else {
      NA_character_
    },
    year = if (!is.null(msg$issued$`date-parts`)) {
      msg$issued$`date-parts`[[1]][1]
    } else {
      NA_integer_
    },
    container = if (!is.null(msg$`container-title`)) {
      msg$`container-title`[[1]]
    } else {
      NA_character_
    },
    doi              = msg$DOI %||% x,
    pmid             = NA_character_,
    pmcid            = NA_character_,
    url              = msg$URL %||% NA_character_,
    provider         = "crossref",
    stringsAsFactors = FALSE
  )
}