#' Europe PMC: check whether a PMID exists
#'
#' @param x A single, normalized PMID string.
#' @param ... Passed to Europe PMC search.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_pmid_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  js <- .scholidonline_epmc_search(
    query = paste0(
      "EXT_ID:",
      x,
      " AND SRC:MED"
    ),
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA)
  }
  
  hit_count <- suppressWarnings(
    as.integer(
      js$hitCount %||% NA_character_
    )
  )
  
  if (is.na(hit_count)) {
    return(NA)
  }
  
  hit_count > 0L
}


#' Europe PMC: check whether a PMCID exists
#'
#' @param x A single, normalized PMCID string.
#' @param ... Passed to Europe PMC search.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_pmcid_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  js <- .scholidonline_epmc_search(
    query = paste0(
      "PMCID:",
      x
    ),
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA)
  }
  
  hit_count <- suppressWarnings(
    as.integer(
      js$hitCount %||% NA_character_
    )
  )
  
  if (is.na(hit_count)) {
    return(NA)
  }
  
  hit_count > 0L
}


#' Europe PMC: return identifiers linked to a PMID
#'
#' @description
#' Provider adapter retrieving identifiers linked to a PMID using the
#' Europe PMC REST API.
#'
#' @param x A single, normalized PMID string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `linked_type`, `linked_value`, `provider`.
#'
#' @noRd
.links_pmid_epmc <- function(x, ..., quiet = FALSE) {
  .scholidonline_check_scalar_chr(x)
  
  url <- paste0(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=EXT_ID:",
    utils::URLencode(x, reserved = TRUE),
    "%20AND%20SRC:MED&format=json"
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
      rlang::warn("Europe PMC request failed.")
    }
    return(data.frame())
  }
  
  status <- httr2::resp_status(resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("Europe PMC request returned HTTP ", status, ".")
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
  
  results <- json$resultList$result
  
  if (is.null(results) || length(results) == 0L) {
    return(data.frame())
  }
  
  rec <- results[[1]]
  
  rows <- list()
  
  if (!is.null(rec$pmid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "pmid",
      linked_value = as.character(rec$pmid),
      provider     = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$pmcid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "pmcid",
      linked_value = as.character(rec$pmcid),
      provider     = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$doi)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "doi",
      linked_value = as.character(rec$doi),
      provider     = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (length(rows) == 0L) {
    return(data.frame())
  }
  
  do.call(rbind, rows)
}



#' Europe PMC: return identifiers linked to a PMCID
#'
#' @description
#' Provider adapter retrieving identifiers linked to a PMCID using the
#' Europe PMC REST API.
#'
#' @param x A single, normalized PMCID string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `linked_type`, `linked_value`, `provider`.
#'
#' @noRd
.links_pmcid_epmc <- function(x, ..., quiet = FALSE) {
  .scholidonline_check_scalar_chr(x)
  
  url <- paste0(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=EXT_ID:",
    utils::URLencode(x, reserved = TRUE),
    "%20AND%20SRC:PMC&format=json"
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
      rlang::warn("Europe PMC request failed.")
    }
    return(data.frame())
  }
  
  status <- httr2::resp_status(resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("Europe PMC request returned HTTP ", status, ".")
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
  
  results <- json$resultList$result
  
  if (is.null(results) || length(results) == 0L) {
    return(data.frame())
  }
  
  rec <- results[[1]]
  
  rows <- list()
  
  if (!is.null(rec$pmid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "pmid",
      linked_value = as.character(rec$pmid),
      provider     = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$pmcid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "pmcid",
      linked_value = as.character(rec$pmcid),
      provider     = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$doi)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "doi",
      linked_value = as.character(rec$doi),
      provider     = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (length(rows) == 0L) {
    return(data.frame())
  }
  
  do.call(rbind, rows)
}