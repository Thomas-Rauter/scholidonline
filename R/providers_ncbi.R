#' NCBI: check whether a PMID exists
#'
#' @param x A single, normalized PMID string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_pmid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  js <- .scholidonline_esummary_pubmed(
    id = x,
    ...,
    quiet = quiet
  )
  
  if (is.null(js) || is.null(js$result)) {
    return(NA)
  }
  
  rec <- js$result[[x]]
  
  if (is.null(rec)) {
    if (!is.null(js$result$uids) &&
        !x %in% unlist(js$result$uids, use.names = FALSE)) {
      return(FALSE)
    }
    return(NA)
  }
  
  if (!is.null(rec$error)) {
    return(FALSE)
  }
  
  uid <- rec$uid %||% NA_character_
  
  if (is.character(uid) && length(uid) == 1L && identical(uid, x)) {
    return(TRUE)
  }
  
  FALSE
}


#' NCBI: check whether a PMCID exists
#'
#' @param x A single, normalized PMCID string.
#' @param ... Passed to PMC ID Converter.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_pmcid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  js <- .scholidonline_pmc_idconv(
    ids = x,
    ...,
    quiet = quiet
  )
  
  if (is.null(js) || is.null(js$records) || length(js$records) < 1L) {
    return(NA)
  }
  
  rec <- js$records[[1L]]
  
  if (!is.null(rec$status) && identical(rec$status, "error")) {
    return(FALSE)
  }
  
  if (!is.null(rec$pmcid) && nzchar(rec$pmcid)) {
    return(TRUE)
  }
  
  FALSE
}


#' NCBI: return identifiers linked to a PMID
#'
#' @description
#' Provider adapter retrieving identifiers linked to a PMID using the
#' NCBI ID Converter API.
#'
#' @param x A single, normalized PMID string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `linked_type`, `linked_value`, `provider`.
#'
#' @noRd
.links_pmid_ncbi <- function(x, ..., quiet = FALSE) {
  .scholidonline_check_scalar_chr(x)
  
  url <- paste0(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=",
    utils::URLencode(x, reserved = TRUE),
    "&format=json"
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
      rlang::warn("NCBI request failed.")
    }
    return(data.frame())
  }
  
  status <- httr2::resp_status(resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("NCBI request returned HTTP ", status, ".")
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
  
  records <- json$records
  
  if (is.null(records) || length(records) == 0L) {
    return(data.frame())
  }
  
  rec <- records[[1]]
  
  rows <- list()
  
  if (!is.null(rec$pmid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "pmid",
      linked_value = as.character(rec$pmid),
      provider     = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$pmcid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "pmcid",
      linked_value = as.character(rec$pmcid),
      provider     = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$doi)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "doi",
      linked_value = as.character(rec$doi),
      provider     = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (length(rows) == 0L) {
    return(data.frame())
  }
  
  do.call(rbind, rows)
}



#' NCBI: return identifiers linked to a PMCID
#'
#' @description
#' Provider adapter retrieving identifiers linked to a PMCID using the
#' NCBI ID Converter API.
#'
#' @param x A single, normalized PMCID string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `linked_type`, `linked_value`, `provider`.
#'
#' @noRd
.links_pmcid_ncbi <- function(x, ..., quiet = FALSE) {
  .scholidonline_check_scalar_chr(x)
  
  url <- paste0(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=",
    utils::URLencode(x, reserved = TRUE),
    "&format=json"
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
      rlang::warn("NCBI request failed.")
    }
    return(data.frame())
  }
  
  status <- httr2::resp_status(resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("NCBI request returned HTTP ", status, ".")
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
  
  records <- json$records
  
  if (is.null(records) || length(records) == 0L) {
    return(data.frame())
  }
  
  rec <- records[[1]]
  
  rows <- list()
  
  if (!is.null(rec$pmid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "pmid",
      linked_value = as.character(rec$pmid),
      provider     = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$pmcid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "pmcid",
      linked_value = as.character(rec$pmcid),
      provider     = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$doi)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type  = "doi",
      linked_value = as.character(rec$doi),
      provider     = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (length(rows) == 0L) {
    return(data.frame())
  }
  
  do.call(rbind, rows)
}