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


#' arXiv: return identifiers linked to an arXiv record
#'
#' @description
#' Provider adapter retrieving identifiers linked to an arXiv record
#' using the arXiv API.
#'
#' @param x A single, normalized arXiv identifier.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `linked_type`, `linked_value`, `provider`.
#'
#' @noRd
.links_arxiv_arxiv <- function(x, ..., quiet = FALSE) {
  .scholidonline_check_scalar_chr(x)
  
  url <- paste0(
    "http://export.arxiv.org/api/query?id_list=",
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
      rlang::warn("arXiv request failed.")
    }
    return(data.frame())
  }
  
  status <- httr2::resp_status(resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("arXiv request returned HTTP ", status, ".")
      )
    }
    return(data.frame())
  }
  
  xml <- tryCatch(
    httr2::resp_body_string(resp),
    error = function(e) NULL
  )
  
  if (is.null(xml)) {
    return(data.frame())
  }
  
  doc <- tryCatch(
    xml2::read_xml(xml),
    error = function(e) NULL
  )
  
  if (is.null(doc)) {
    return(data.frame())
  }
  
  doi_node <- xml2::xml_find_first(
    doc,
    ".//arxiv:doi",
    xml2::xml_ns(doc)
  )
  
  if (is.na(doi_node)) {
    return(data.frame())
  }
  
  doi <- xml2::xml_text(doi_node)
  
  if (is.null(doi) || identical(doi, "")) {
    return(data.frame())
  }
  
  data.frame(
    linked_type  = "doi",
    linked_value = doi,
    provider     = "arxiv",
    stringsAsFactors = FALSE
  )
}