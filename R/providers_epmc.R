# id_exists() subfunctions -----------------------------------------------------


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
  .scholidonline_check_scalar_chr(x = x)
  
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
    as.integer(js$hitCount %||% NA_character_)
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
  .scholidonline_check_scalar_chr(x = x)
  
  js <- .scholidonline_epmc_search(
    query = paste0("PMCID:", x),
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA)
  }
  
  hit_count <- suppressWarnings(
    as.integer(js$hitCount %||% NA_character_)
  )
  
  if (is.na(hit_count)) {
    return(NA)
  }
  
  hit_count > 0L
}


# id_links() subfunctions ------------------------------------------------------


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
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=EXT_ID:",
    utils::URLencode(
      x,
      reserved = TRUE
    ),
    "%20AND%20SRC:MED&format=json"
  )
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("Europe PMC request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("Europe PMC request returned HTTP ", status, ".")
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
  
  results <- json$resultList$result
  
  if (is.null(results) || length(results) == 0L) {
    return(data.frame())
  }
  
  rec <- results[[1]]
  rows <- list()
  
  if (!is.null(rec$pmid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "pmid",
      linked_value = as.character(rec$pmid),
      provider = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$pmcid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "pmcid",
      linked_value = as.character(rec$pmcid),
      provider = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$doi)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "doi",
      linked_value = as.character(rec$doi),
      provider = "epmc",
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
  .scholidonline_check_scalar_chr(x = x)
  
  pmcid_clean <- gsub("^PMC", "", x)
  
  url <- paste0(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search",
    "?query=PMCID:PMC",
    utils::URLencode(
      pmcid_clean,
      reserved = TRUE
    ),
    "&format=json"
  )
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("Europe PMC request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("Europe PMC request returned HTTP ", status, ".")
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
  
  results <- json$resultList$result
  
  if (is.null(results) || length(results) == 0L) {
    return(data.frame())
  }
  
  rec <- results[[1]]
  rows <- list()
  
  if (!is.null(rec$pmid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "pmid",
      linked_value = as.character(rec$pmid),
      provider = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$pmcid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "pmcid",
      linked_value = as.character(rec$pmcid),
      provider = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$doi)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "doi",
      linked_value = as.character(rec$doi),
      provider = "epmc",
      stringsAsFactors = FALSE
    )
  }
  
  if (length(rows) == 0L) {
    return(data.frame())
  }
  
  do.call(rbind, rows)
}


# id_metadata() subfunctions ---------------------------------------------------


#' Europe PMC: retrieve metadata for a PMID
#'
#' @description
#' Provider implementation for retrieving metadata for a PMID using the
#' Europe PMC REST API.
#'
#' @param x A single, normalized PMID string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the PMID.
#'
#' @noRd
.meta_pmid_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search",
    "?query=EXT_ID:",
    utils::URLencode(
      x,
      reserved = TRUE
    ),
    "%20AND%20SRC:MED&format=json"
  )
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("Europe PMC request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("Europe PMC request returned HTTP ", status, "."))
    }
    return(data.frame())
  }
  
  obj <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = TRUE
  )
  
  recs <- obj$resultList$result
  
  if (is.null(recs) || length(recs) == 0L) {
    return(data.frame())
  }
  
  rec <- if (is.data.frame(recs)) {
    recs[1, ]
  } else if (is.list(recs) && !is.null(recs[[1]]) && is.list(recs[[1]])) {
    recs[[1]]
  } else {
    recs
  }
  
  data.frame(
    title = rec[["title"]] %||% NA_character_,
    year = if (!is.null(rec[["pubYear"]])) {
      as.integer(rec[["pubYear"]])
    } else {
      NA_integer_
    },
    container = rec[["journalTitle"]] %||% NA_character_,
    doi = rec[["doi"]] %||% NA_character_,
    pmid = x,
    pmcid = rec[["pmcid"]] %||% NA_character_,
    url = paste0(
      "https://europepmc.org/article/MED/",
      x
    ),
    provider = "epmc",
    stringsAsFactors = FALSE
  )
}


#' Europe PMC: retrieve metadata for a PMCID
#'
#' @description
#' Provider implementation for retrieving metadata for a PMCID using the
#' Europe PMC REST API.
#'
#' @param x A single, normalized PMCID string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the PMCID.
#'
#' @noRd
.meta_pmcid_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  pmcid_clean <- gsub("^PMC", "", x)
  
  url <- paste0(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search",
    "?query=PMCID:PMC",
    utils::URLencode(
      pmcid_clean,
      reserved = TRUE
    ),
    "&format=json"
  )
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("Europe PMC request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("Europe PMC request returned HTTP ", status, "."))
    }
    return(data.frame())
  }
  
  obj <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = TRUE
  )
  
  recs <- obj$resultList$result
  
  if (is.null(recs) || length(recs) == 0L) {
    return(data.frame())
  }
  
  rec <- if (is.data.frame(recs)) {
    recs[1, ]
  } else if (is.list(recs) && !is.null(recs[[1]]) && is.list(recs[[1]])) {
    recs[[1]]
  } else {
    recs
  }
  
  data.frame(
    title = rec[["title"]] %||% NA_character_,
    year = if (!is.null(rec[["pubYear"]])) {
      as.integer(rec[["pubYear"]])
    } else {
      NA_integer_
    },
    container = rec[["journalTitle"]] %||% NA_character_,
    doi = rec[["doi"]] %||% NA_character_,
    pmid = rec[["pmid"]] %||% NA_character_,
    pmcid = x,
    url = paste0(
      "https://europepmc.org/article/PMC/",
      pmcid_clean
    ),
    provider = "epmc",
    stringsAsFactors = FALSE
  )
}


# id_convert() provider functions ----------------------------------------------


#' Europe PMC: PMID -> DOI
#'
#' @param x A single PMID string.
#' @param ... Passed to Europe PMC search.
#' @param quiet Logical.
#'
#' @return A single DOI string or `NA_character_`.
#'
#' @noRd
.convert_pmid_to_doi_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  q <- paste0("EXT_ID:", x, " AND SRC:MED")
  
  js <- .scholidonline_epmc_search(
    query = q,
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA_character_)
  }
  
  rec <- .scholidonline_epmc_first_result(x = js)
  doi <- rec$doi %||% NA_character_
  
  if (is.na(doi) || !nzchar(doi)) {
    return(NA_character_)
  }
  
  as.character(doi)
}


#' Europe PMC: DOI -> PMID
#'
#' @param x A single DOI string.
#' @param ... Passed to Europe PMC search.
#' @param quiet Logical.
#'
#' @return A single PMID string or `NA_character_`.
#'
#' @noRd
.convert_doi_to_pmid_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  q <- paste0("DOI:\"", x, "\"")
  
  js <- .scholidonline_epmc_search(
    query = q,
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA_character_)
  }
  
  rec <- .scholidonline_epmc_first_result(x = js)
  pmid <- rec$pmid %||% NA_character_
  
  if (is.na(pmid) || !nzchar(pmid)) {
    return(NA_character_)
  }
  
  as.character(pmid)
}


#' Europe PMC: PMCID -> PMID
#'
#' @param x A single PMCID string.
#' @param ... Passed to Europe PMC search.
#' @param quiet Logical.
#'
#' @return A single PMID string or `NA_character_`.
#'
#' @noRd
.convert_pmcid_to_pmid_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  q <- paste0("PMCID:", x)
  
  js <- .scholidonline_epmc_search(
    query = q,
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA_character_)
  }
  
  rec <- .scholidonline_epmc_first_result(x = js)
  pmid <- rec$pmid %||% NA_character_
  
  if (is.na(pmid) || !nzchar(pmid)) {
    return(NA_character_)
  }
  
  as.character(pmid)
}


#' Europe PMC: PMCID -> DOI
#'
#' @param x A single PMCID string.
#' @param ... Passed to Europe PMC search.
#' @param quiet Logical.
#'
#' @return A single DOI string or `NA_character_`.
#'
#' @noRd
.convert_pmcid_to_doi_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  q <- paste0("PMCID:", x)
  
  js <- .scholidonline_epmc_search(
    query = q,
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA_character_)
  }
  
  rec <- .scholidonline_epmc_first_result(x = js)
  doi <- rec$doi %||% NA_character_
  
  if (is.na(doi) || !nzchar(doi)) {
    return(NA_character_)
  }
  
  as.character(doi)
}


#' Europe PMC: PMID -> PMCID
#'
#' @param x A single PMID string.
#' @param ... Passed to Europe PMC search.
#' @param quiet Logical.
#'
#' @return A single PMCID string or `NA_character_`.
#'
#' @noRd
.convert_pmid_to_pmcid_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  q <- paste0("EXT_ID:", x, " AND SRC:MED")
  
  js <- .scholidonline_epmc_search(
    query = q,
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA_character_)
  }
  
  rec <- .scholidonline_epmc_first_result(x = js)
  pmcid <- rec$pmcid %||% NA_character_
  
  if (is.na(pmcid) || !nzchar(pmcid)) {
    return(NA_character_)
  }
  
  as.character(pmcid)
}


#' Europe PMC: DOI -> PMCID
#'
#' @param x A single DOI string.
#' @param ... Passed to Europe PMC search.
#' @param quiet Logical.
#'
#' @return A single PMCID string or `NA_character_`.
#'
#' @noRd
.convert_doi_to_pmcid_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  q <- paste0("DOI:\"", x, "\"")
  
  js <- .scholidonline_epmc_search(
    query = q,
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA_character_)
  }
  
  rec <- .scholidonline_epmc_first_result(x = js)
  pmcid <- rec$pmcid %||% NA_character_
  
  if (is.na(pmcid) || !nzchar(pmcid)) {
    return(NA_character_)
  }
  
  as.character(pmcid)
}


# id_convert() provider functions helpers --------------------------------------


#' Return the first Europe PMC search result
#'
#' @description
#' Internal helper for extracting the first result record from a parsed
#' Europe PMC search response.
#'
#' This helper is used by provider implementations that query Europe PMC and
#' need only the first matching record.
#'
#' @param x A parsed Europe PMC search response as a list.
#'
#' @return The first result record as a list, or `NULL` if no result is
#'   available.
#'
#' @noRd
.scholidonline_epmc_first_result <- function(x) {
  res <- x$resultList$result
  
  if (is.null(res) || length(res) < 1L) {
    return(NULL)
  }
  
  res[[1]]
}