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