#' arXiv: check whether an arXiv identifier exists
#'
#' @param x A single, normalized arXiv identifier.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_arxiv_arxiv <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  query <- paste0(
    "id_list=",
    utils::URLencode(
      x,
      reserved = TRUE
    )
  )
  
  url <- paste0(
    "https://export.arxiv.org/api/query?",
    query
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
        "arXiv request failed.",
        call. = FALSE
      )
    }
    return(NA)
  }
  
  status <- httr2::resp_status(
    resp = resp
  )
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      warning(
        "arXiv request returned HTTP ",
        status,
        ".",
        call. = FALSE
      )
    }
    return(NA)
  }
  
  txt <- tryCatch(
    httr2::resp_body_string(
      resp = resp
    ),
    error = function(e) NULL
  )
  
  if (is.null(txt)) {
    return(NA)
  }
  
  grepl(
    pattern = "<entry>",
    x = txt,
    fixed = TRUE
  )
}