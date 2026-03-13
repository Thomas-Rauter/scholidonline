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