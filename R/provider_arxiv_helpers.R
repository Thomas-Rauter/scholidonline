#' arXiv: internal state for request throttling
#'
#' @description
#' Internal environment used to store the timestamp of the most recent arXiv
#' request made through the package.
#'
#' @noRd
.scholidonline_arxiv_state <- new.env(parent = emptyenv())


#' arXiv: throttle repeated requests
#'
#' @description
#' Waits before a repeated arXiv request when the previous request was made
#' less than `min_interval` seconds ago. The first request does not wait.
#'
#' Throttling can be disabled globally with
#' `options(scholidonline.rate_limit = FALSE)`.
#'
#' @param min_interval A single numeric value giving the minimum number of
#'   seconds between arXiv requests.
#' @param quiet Logical.
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.arxiv_rate_limit <- function(
    min_interval = getOption("scholidonline.arxiv.min_interval", 3),
    quiet = FALSE
) {
  if (!isTRUE(getOption("scholidonline.rate_limit", TRUE))) {
    return(invisible(NULL))
  }
  
  if (!is.numeric(min_interval) || length(min_interval) != 1L) {
    stop("`min_interval` must be a single numeric value.", call. = FALSE)
  }
  
  if (is.na(min_interval) || min_interval <= 0) {
    return(invisible(NULL))
  }
  
  last <- .scholidonline_arxiv_state$last_request_time
  
  if (!is.null(last)) {
    elapsed <- as.numeric(difftime(Sys.time(), last, units = "secs"))
    wait <- min_interval - elapsed
    
    if (is.finite(wait) && wait > 0) {
      if (
        !isTRUE(quiet) &&
        isTRUE(getOption("scholidonline.rate_limit.verbose", FALSE))
      ) {
        rlang::inform(
          paste0(
            "Waiting ",
            round(wait, 2),
            " seconds before the next arXiv request."
          )
        )
      }
      
      Sys.sleep(wait)
    }
  }
  
  .scholidonline_arxiv_state$last_request_time <- Sys.time()
  
  invisible(NULL)
}


#' arXiv: reset request throttling state
#'
#' @description
#' Clears the stored timestamp of the most recent arXiv request. This helper is
#' intended for tests and internal diagnostics.
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.arxiv_rate_limit_reset <- function() {
  .scholidonline_arxiv_state$last_request_time <- NULL
  invisible(NULL)
}


#' arXiv: construct an API URL for an identifier list
#'
#' @param x A character vector of normalized arXiv identifiers.
#'
#' @return A single URL string.
#'
#' @noRd
.arxiv_id_list_url <- function(x) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  query <- paste0(
    "id_list=",
    paste(
      utils::URLencode(x, reserved = TRUE),
      collapse = ","
    )
  )
  
  paste0(
    "https://export.arxiv.org/api/query?",
    query
  )
}


#' arXiv: query records by identifier list
#'
#' @description
#' Performs one arXiv API request for a vector of arXiv identifiers using the
#' `id_list` parameter. Missing and empty identifiers are dropped before the
#' request.
#'
#' @param x A character vector of normalized arXiv identifiers.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single response body string, or `NULL` if the request failed or no
#'   non-missing identifiers were supplied.
#'
#' @noRd
.arxiv_query_id_list <- function(
    x,
    ...,
    quiet = FALSE
) {
  rlang::check_dots_empty()
  
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  x <- x[!is.na(x) & nzchar(x)]
  
  if (length(x) < 1L) {
    return(NULL)
  }
  
  url <- .arxiv_id_list_url(x = x)
  
  req <- .scholidonline_request(url)
  
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  .arxiv_rate_limit(quiet = quiet)
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("arXiv request failed.")
    }
    return(NULL)
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("arXiv request returned HTTP ", status, ".")
      )
    }
    return(NULL)
  }
  
  tryCatch(
    .scholidonline_resp_body_string(resp = resp),
    error = function(e) NULL
  )
}


#' arXiv: extract XML ID values
#'
#' @param txt A single arXiv API response body string.
#'
#' @return A character vector of values contained in XML `<id>` elements.
#'
#' @noRd
.arxiv_extract_xml_ids <- function(txt) {
  if (!is.character(txt) || length(txt) != 1L || is.na(txt) || !nzchar(txt)) {
    return(character())
  }
  
  matches <- gregexpr(
    "<id[^>]*>[^<]+</id>",
    txt,
    perl = TRUE
  )
  
  ids <- regmatches(txt, matches)[[1L]]
  
  if (length(ids) < 1L || identical(ids, character(0))) {
    return(character())
  }
  
  ids <- gsub("^<id[^>]*>", "", ids)
  ids <- gsub("</id>$", "", ids)
  
  ids
}


#' arXiv: extract article entry ID URLs
#'
#' @description
#' Extracts article entry ID URLs from an arXiv API response. Feed-level IDs and
#' arXiv API error IDs are ignored.
#'
#' @param txt A single arXiv API response body string.
#'
#' @return A character vector of arXiv article entry ID URLs.
#'
#' @noRd
.arxiv_extract_entry_id_urls <- function(txt) {
  ids <- .arxiv_extract_xml_ids(txt = txt)
  
  ids[
    grepl("^https?://arxiv\\.org/abs/", ids) |
      grepl("^https?://arxiv\\.org/pdf/", ids)
  ]
}


#' arXiv: convert article entry URLs to arXiv identifiers
#'
#' @param x A character vector of arXiv article entry ID URLs.
#'
#' @return A character vector of arXiv identifiers.
#'
#' @noRd
.arxiv_entry_urls_to_ids <- function(x) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  out <- sub("^https?://arxiv\\.org/abs/", "", x)
  out <- sub("^https?://arxiv\\.org/pdf/", "", out)
  out <- sub("\\.pdf$", "", out)
  
  out
}


#' arXiv: strip version suffix
#'
#' @param x A character vector of arXiv identifiers.
#'
#' @return A character vector of arXiv identifiers without trailing version
#'   suffixes such as `v1` or `v2`.
#'
#' @noRd
.arxiv_strip_version <- function(x) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  sub("v[0-9]+$", "", x)
}


#' arXiv: extract returned article identifiers
#'
#' @description
#' Extracts arXiv article identifiers from an arXiv API response body. Version
#' suffixes are retained.
#'
#' @param txt A single arXiv API response body string.
#'
#' @return A character vector of arXiv identifiers.
#'
#' @noRd
.arxiv_extract_entry_ids <- function(txt) {
  entry_urls <- .arxiv_extract_entry_id_urls(txt = txt)
  .arxiv_entry_urls_to_ids(entry_urls)
}


#' arXiv: check whether arXiv identifiers exist using one batch request
#'
#' @description
#' Performs one arXiv API request for a vector of normalized arXiv identifiers
#' and returns one logical value per input. Missing and empty inputs are returned
#' as `NA`.
#'
#' @param x A character vector of normalized arXiv identifiers.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A logical vector with one value per input.
#'
#' @noRd
.exists_arxiv_arxiv_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  rlang::check_dots_empty()
  
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  out <- rep(NA, length(x))
  
  valid <- !is.na(x) & nzchar(x)
  
  if (!any(valid)) {
    return(out)
  }
  
  x_valid <- x[valid]
  
  txt <- .arxiv_query_id_list(
    x = x_valid,
    quiet = quiet
  )
  
  if (is.null(txt)) {
    out[valid] <- NA
    return(out)
  }
  
  found <- .arxiv_extract_entry_ids(txt = txt)
  
  found_no_version <- .arxiv_strip_version(found)
  query_no_version <- .arxiv_strip_version(x_valid)
  
  out[valid] <- query_no_version %in% found_no_version
  
  out
}


#' arXiv: extract XML entry blocks
#'
#' @param txt A single arXiv API response body string.
#'
#' @return A character vector of XML entry blocks.
#'
#' @noRd
.arxiv_extract_entry_blocks <- function(txt) {
  if (!is.character(txt) || length(txt) != 1L || is.na(txt) || !nzchar(txt)) {
    return(character())
  }
  
  matches <- gregexpr(
    "<entry[^>]*>[\\s\\S]*?</entry>",
    txt,
    perl = TRUE
  )
  
  entries <- regmatches(txt, matches)[[1L]]
  
  if (length(entries) < 1L || identical(entries, character(0))) {
    return(character())
  }
  
  entries
}


#' arXiv: extract first XML element text
#'
#' @param txt A single XML string.
#' @param tag A single XML tag name.
#'
#' @return A single character value, or `NA_character_`.
#'
#' @noRd
.arxiv_extract_first_element_text <- function(txt, tag) {
  if (!is.character(txt) || length(txt) != 1L || is.na(txt) || !nzchar(txt)) {
    return(NA_character_)
  }
  
  if (!is.character(tag) || length(tag) != 1L || is.na(tag) || !nzchar(tag)) {
    stop("`tag` must be a single non-missing character string.", call. = FALSE)
  }
  
  pattern <- paste0(
    "<",
    tag,
    "[^>]*>[[:space:]]*(.*?)[[:space:]]*</",
    tag,
    ">"
  )
  
  match <- regexpr(
    pattern,
    txt,
    perl = TRUE
  )
  
  if (match[[1L]] < 0L) {
    return(NA_character_)
  }
  
  out <- regmatches(txt, match)
  out <- sub(
    paste0("^<", tag, "[^>]*>[[:space:]]*"),
    "",
    out,
    perl = TRUE
  )
  out <- sub(
    paste0("[[:space:]]*</", tag, ">$"),
    "",
    out,
    perl = TRUE
  )
  
  trimws(out)
}


#' arXiv: parse one metadata entry
#'
#' @param entry A single arXiv API XML entry block.
#'
#' @return A one-row data.frame containing arXiv metadata, or an empty
#'   data.frame for non-article entries.
#'
#' @noRd
.arxiv_parse_meta_entry <- function(entry) {
  entry_id <- .arxiv_extract_first_element_text(
    txt = entry,
    tag = "id"
  )
  
  if (
    is.na(entry_id) ||
    !grepl("^https?://arxiv\\.org/abs/", entry_id)
  ) {
    return(data.frame())
  }
  
  entry_title <- .arxiv_extract_first_element_text(
    txt = entry,
    tag = "title"
  )
  
  entry_published <- .arxiv_extract_first_element_text(
    txt = entry,
    tag = "published"
  )
  
  entry_year <- if (
    !is.na(entry_published) &&
    grepl("^[0-9]{4}(-|$)", entry_published)
  ) {
    as.integer(substr(entry_published, 1L, 4L))
  } else {
    NA_integer_
  }
  
  entry_doi <- .arxiv_extract_first_element_text(
    txt = entry,
    tag = "arxiv:doi"
  )
  
  if (is.na(entry_doi) || !nzchar(entry_doi)) {
    entry_doi <- NA_character_
  }
  
  data.frame(
    arxiv_id = .arxiv_entry_urls_to_ids(entry_id),
    title = entry_title,
    year = entry_year,
    container = "arXiv",
    doi = entry_doi,
    pmid = NA_character_,
    pmcid = NA_character_,
    url = entry_id,
    provider = "arxiv",
    stringsAsFactors = FALSE
  )
}


#' arXiv: retrieve metadata using one batch request
#'
#' @description
#' Performs one arXiv API request for a vector of normalized arXiv identifiers
#' and returns one provider metadata row per resolvable arXiv record.
#'
#' @param x A character vector of normalized arXiv identifiers.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A data.frame containing provider metadata rows.
#'
#' @noRd
.meta_arxiv_arxiv_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  rlang::check_dots_empty()
  
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  valid <- !is.na(x) & nzchar(x)
  
  if (!any(valid)) {
    return(data.frame())
  }
  
  x_valid <- x[valid]
  
  txt <- .arxiv_query_id_list(
    x = x_valid,
    quiet = quiet
  )
  
  if (is.null(txt)) {
    return(data.frame())
  }
  
  entries <- .arxiv_extract_entry_blocks(txt = txt)
  
  if (length(entries) < 1L) {
    return(data.frame())
  }
  
  out <- lapply(
    entries,
    .arxiv_parse_meta_entry
  )
  
  out <- do.call(
    rbind,
    out
  )
  
  if (is.null(out) || nrow(out) < 1L) {
    return(data.frame())
  }
  
  query_no_version <- .arxiv_strip_version(x_valid)
  found_no_version <- .arxiv_strip_version(out$arxiv_id)
  
  keep <- found_no_version %in% query_no_version
  out <- out[keep, , drop = FALSE]
  
  if (nrow(out) < 1L) {
    return(data.frame())
  }
  
  out
}


#' arXiv: extract first DOI from an entry
#'
#' @param entry A single arXiv API XML entry block.
#'
#' @return A single DOI string, or `NA_character_`.
#'
#' @noRd
.arxiv_extract_entry_doi <- function(entry) {
  doi <- .arxiv_extract_first_element_text(
    txt = entry,
    tag = "arxiv:doi"
  )
  
  if (!is.na(doi) && nzchar(doi)) {
    return(doi)
  }
  
  match <- regexpr(
    '<link[^>]+title=["\\\']doi["\\\'][^>]+>',
    entry,
    perl = TRUE
  )
  
  if (match[[1L]] < 0L) {
    match <- regexpr(
      '<link[^>]+href=["\\\'][^"\\\']*doi\\.org/[^"\\\']+["\\\'][^>]*>',
      entry,
      perl = TRUE
    )
  }
  
  if (match[[1L]] < 0L) {
    return(NA_character_)
  }
  
  link <- regmatches(entry, match)
  
  href_match <- regexpr(
    'href=["\\\'][^"\\\']+["\\\']',
    link,
    perl = TRUE
  )
  
  if (href_match[[1L]] < 0L) {
    return(NA_character_)
  }
  
  href <- regmatches(link, href_match)
  href <- sub('^href=["\\\']', "", href, perl = TRUE)
  href <- sub('["\\\']$', "", href, perl = TRUE)
  
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", href)
  doi <- trimws(doi)
  
  if (!nzchar(doi)) {
    return(NA_character_)
  }
  
  doi
}


#' arXiv: parse linked identifiers from one entry
#'
#' @param entry A single arXiv API XML entry block.
#'
#' @return A data.frame containing linked identifiers for one arXiv entry.
#'
#' @noRd
.arxiv_parse_links_entry <- function(entry) {
  entry_id <- .arxiv_extract_first_element_text(
    txt = entry,
    tag = "id"
  )
  
  if (
    is.na(entry_id) ||
    !grepl("^https?://arxiv\\.org/abs/", entry_id)
  ) {
    return(data.frame())
  }
  
  arxiv_id <- .arxiv_entry_urls_to_ids(entry_id)
  doi <- .arxiv_extract_entry_doi(entry = entry)
  
  if (is.na(doi) || !nzchar(doi)) {
    return(data.frame())
  }
  
  data.frame(
    arxiv_id = arxiv_id,
    linked_type = "doi",
    linked_value = doi,
    provider = "arxiv",
    stringsAsFactors = FALSE
  )
}


#' arXiv: return linked identifiers using one batch request
#'
#' @description
#' Performs one arXiv API request for a vector of normalized arXiv identifiers
#' and returns linked identifiers exposed by arXiv records.
#'
#' @param x A character vector of normalized arXiv identifiers.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A data.frame with columns `arxiv_id`, `linked_type`,
#'   `linked_value`, and `provider`.
#'
#' @noRd
.links_arxiv_arxiv_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  rlang::check_dots_empty()
  
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  valid <- !is.na(x) & nzchar(x)
  
  if (!any(valid)) {
    return(data.frame())
  }
  
  x_valid <- x[valid]
  
  txt <- .arxiv_query_id_list(
    x = x_valid,
    quiet = quiet
  )
  
  if (is.null(txt)) {
    return(data.frame())
  }
  
  entries <- .arxiv_extract_entry_blocks(txt = txt)
  
  if (length(entries) < 1L) {
    return(data.frame())
  }
  
  out <- lapply(
    entries,
    .arxiv_parse_links_entry
  )
  
  out <- do.call(
    rbind,
    out
  )
  
  if (is.null(out) || nrow(out) < 1L) {
    return(data.frame())
  }
  
  query_no_version <- .arxiv_strip_version(x_valid)
  found_no_version <- .arxiv_strip_version(out$arxiv_id)
  
  keep <- found_no_version %in% query_no_version
  out <- out[keep, , drop = FALSE]
  
  if (nrow(out) < 1L) {
    return(data.frame())
  }
  
  out
}