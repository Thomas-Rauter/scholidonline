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
  .scholidonline_check_scalar_chr(x = x)
  
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
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  max_attempts <- 2L
  attempt <- 1L
  
  repeat {
    
    resp <- .scholidonline_req_perform_safe(req = req)
    
    if (is.null(resp)) {
      if (!isTRUE(quiet)) {
        rlang::warn("arXiv request failed.")
      }
      return(NA)
    }
    
    status <- .scholidonline_resp_status(resp = resp)
    
    if (status >= 200L && status < 300L) {
      break
    }
    
    if (status == 429L && attempt < max_attempts) {
      if (!isTRUE(quiet)) {
        rlang::warn("arXiv request rate-limited (HTTP 429); retrying once.")
      }
      attempt <- attempt + 1L
      Sys.sleep(1)
      next
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("arXiv request returned HTTP ", status, "."))
    }
    return(NA)
  }
  
  txt <- tryCatch(
    .scholidonline_resp_body_string(resp = resp),
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
.links_arxiv_arxiv <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "http://export.arxiv.org/api/query?id_list=",
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
      rlang::warn("arXiv request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("arXiv request returned HTTP ", status, ".")
      )
    }
    return(data.frame())
  }
  
  xml <- tryCatch(
    .scholidonline_resp_body_string(resp = resp),
    error = function(e) NULL
  )
  
  if (is.null(xml)) {
    return(data.frame())
  }
  
  doi <- sub(
    ".*<arxiv:doi[^>]*>([^<]+)</arxiv:doi>.*",
    "\\1",
    xml
  )
  
  if (identical(doi, xml) || !nzchar(doi)) {
    return(data.frame())
  }
  
  data.frame(
    linked_type      = "doi",
    linked_value     = doi,
    provider         = "arxiv",
    stringsAsFactors = FALSE
  )
}


#' arXiv: retrieve metadata for an arXiv identifier
#'
#' @description
#' Provider implementation for retrieving metadata for an arXiv identifier
#' using the arXiv API.
#'
#' @param x A single, normalized arXiv identifier.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the arXiv record.
#'
#' @noRd
.meta_arxiv_arxiv <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "http://export.arxiv.org/api/query?id_list=",
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
      rlang::warn("arXiv request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("arXiv request returned HTTP ", status, ".")
      )
    }
    return(data.frame())
  }
  
  txt <- .scholidonline_resp_body_string(resp = resp)
  
  entry_title <- sub(
    ".*<entry>.*?<title>(.*?)</title>.*",
    "\\1",
    txt
  )
  entry_year_raw <- sub(
    ".*<entry>.*?<published>(.*?)</published>.*",
    "\\1",
    txt
  )
  entry_year <- if (
    !identical(entry_year_raw, txt) &&
    grepl("^[0-9]{4}(-|$)", entry_year_raw)
  ) {
    as.integer(substr(entry_year_raw, 1L, 4L))
  } else {
    NA_integer_
  }
  entry_url <- sub(
    ".*<entry>.*?<id>(.*?)</id>.*",
    "\\1",
    txt
  )
  
  if (identical(entry_title, txt)) {
    return(data.frame())
  }
  
  data.frame(
    title            = entry_title,
    year             = entry_year,
    container        = "arXiv",
    doi              = NA_character_,
    pmid             = NA_character_,
    pmcid            = NA_character_,
    url              = entry_url,
    provider         = "arxiv",
    stringsAsFactors = FALSE
  )
}