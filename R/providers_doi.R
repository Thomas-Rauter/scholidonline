#' DOI.org: check whether a DOI exists
#'
#' @param x A single, normalized DOI string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_doi_doi_org <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  url <- paste0(
    "https://doi.org/",
    utils::URLencode(
      x,
      reserved = TRUE
    )
  )
  
  req <- httr2::request(url)
  req <- httr2::req_headers(
    .req = req,
    Accept = "application/vnd.citationstyles.csl+json"
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
        "DOI.org request failed.",
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
      "DOI.org request returned HTTP ",
      status,
      ".",
      call. = FALSE
    )
  }
  
  NA
}


#' DOI.org: retrieve metadata for a DOI
#'
#' @description
#' Provider implementation for retrieving metadata for a DOI using the
#' DOI.org content negotiation API.
#'
#' @param x A single, normalized DOI string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the DOI.
#'
#' @noRd
.meta_doi_doi_org <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  url <- paste0(
    "https://doi.org/",
    utils::URLencode(x, reserved = TRUE)
  )
  
  req <- httr2::request(url)
  
  req <- httr2::req_headers(
    .req = req,
    Accept = "application/vnd.citationstyles.csl+json"
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
      warning("DOI.org request failed.", call. = FALSE)
    }
    return(data.frame())
  }
  
  status <- httr2::resp_status(resp)
  
  if (status == 404L) {
    return(data.frame())
  }
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      warning(
        "DOI.org request returned HTTP ",
        status,
        ".",
        call. = FALSE
      )
    }
    return(data.frame())
  }
  
  obj <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  
  data.frame(
    title = obj$title %||% NA_character_,
    year = if (!is.null(obj$issued$`date-parts`)) {
      obj$issued$`date-parts`[[1]][1]
    } else {
      NA_integer_
    },
    container = obj$`container-title` %||% NA_character_,
    doi = obj$DOI %||% x,
    pmid = NA_character_,
    pmcid = NA_character_,
    url = obj$URL %||% NA_character_,
    provider = "doi.org",
    stringsAsFactors = FALSE
  )
}
