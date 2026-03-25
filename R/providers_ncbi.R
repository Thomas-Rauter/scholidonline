# id_exists() provider functions -----------------------------------------------


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
  .scholidonline_check_scalar_chr(x = x)
  
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
  .scholidonline_check_scalar_chr(x = x)
  
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


# id_links() provider functions ------------------------------------------------


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
.links_pmid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=",
    utils::URLencode(
      x,
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
      rlang::warn("NCBI request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("NCBI request returned HTTP ", status, ".")
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
  
  records <- json$records
  
  if (is.null(records) || length(records) == 0L) {
    return(data.frame())
  }
  
  rec <- records[[1]]
  rows <- list()
  
  if (!is.null(rec$pmid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "pmid",
      linked_value = as.character(rec$pmid),
      provider = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$pmcid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "pmcid",
      linked_value = as.character(rec$pmcid),
      provider = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$doi)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "doi",
      linked_value = as.character(rec$doi),
      provider = "ncbi",
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
.links_pmcid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=",
    utils::URLencode(
      x,
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
      rlang::warn("NCBI request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (!(status >= 200L && status < 300L)) {
    if (!isTRUE(quiet)) {
      rlang::warn(
        paste0("NCBI request returned HTTP ", status, ".")
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
  
  records <- json$records
  
  if (is.null(records) || length(records) == 0L) {
    return(data.frame())
  }
  
  rec <- records[[1]]
  rows <- list()
  
  if (!is.null(rec$pmid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "pmid",
      linked_value = as.character(rec$pmid),
      provider = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$pmcid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "pmcid",
      linked_value = as.character(rec$pmcid),
      provider = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(rec$doi)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "doi",
      linked_value = as.character(rec$doi),
      provider = "ncbi",
      stringsAsFactors = FALSE
    )
  }
  
  if (length(rows) == 0L) {
    return(data.frame())
  }
  
  do.call(rbind, rows)
}


# id_metadata() provider functions ---------------------------------------------


#' NCBI: retrieve metadata for a PMID
#'
#' @description
#' Provider implementation for retrieving metadata for a PMID using the
#' NCBI E-utilities (esummary) API.
#'
#' @param x A single, normalized PMID string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the PMID.
#'
#' @noRd
.meta_pmid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
    "?db=pubmed&id=",
    utils::URLencode(
      x,
      reserved = TRUE
    ),
    "&retmode=json"
  )
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("NCBI request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("NCBI request returned HTTP ", status, "."))
    }
    return(data.frame())
  }
  
  obj <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = TRUE
  )
  
  rec <- obj$result[[x]]
  
  if (is.null(rec)) {
    return(data.frame())
  }
  
  data.frame(
    title = rec$title %||% NA_character_,
    year = if (!is.null(rec$pubdate)) {
      as.integer(substr(rec$pubdate, 1, 4))
    } else {
      NA_integer_
    },
    container = rec$source %||% NA_character_,
    doi = if (!is.null(rec$elocationid) &&
              grepl("^10\\.", rec$elocationid)) {
      rec$elocationid
    } else {
      NA_character_
    },
    pmid = x,
    pmcid = NA_character_,
    url = paste0(
      "https://pubmed.ncbi.nlm.nih.gov/",
      x,
      "/"
    ),
    provider = "ncbi",
    stringsAsFactors = FALSE
  )
}


#' NCBI: retrieve metadata for a PMCID
#'
#' @description
#' Provider implementation for retrieving metadata for a PMCID using the
#' NCBI E-utilities (esummary) API.
#'
#' @param x A single, normalized PMCID string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the PMCID.
#'
#' @noRd
.meta_pmcid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  key <- gsub("^PMC", "", x)
  
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
    "?db=pmc&id=",
    utils::URLencode(
      key,
      reserved = TRUE
    ),
    "&retmode=json"
  )
  
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("NCBI request failed.")
    }
    return(data.frame())
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("NCBI request returned HTTP ", status, "."))
    }
    return(data.frame())
  }
  
  obj <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = TRUE
  )
  
  rec <- obj$result[[key]]
  
  if (is.null(rec)) {
    return(data.frame())
  }
  
  data.frame(
    title = rec$title %||% NA_character_,
    year = if (!is.null(rec$pubdate)) {
      as.integer(substr(rec$pubdate, 1, 4))
    } else {
      NA_integer_
    },
    container = rec$source %||% NA_character_,
    doi = if (!is.null(rec$elocationid) &&
              grepl("^10\\.", rec$elocationid)) {
      rec$elocationid
    } else {
      NA_character_
    },
    pmid = rec$pmid %||% NA_character_,
    pmcid = x,
    url = paste0(
      "https://www.ncbi.nlm.nih.gov/pmc/articles/",
      x,
      "/"
    ),
    provider = "ncbi",
    stringsAsFactors = FALSE
  )
}


# id_convert() provider functions ----------------------------------------------


#' NCBI: PMID -> DOI
#'
#' @param x A single PMID string.
#' @param ... Passed to NCBI E-utilities (e.g. `api_key`, `tool`, `email`).
#' @param quiet Logical.
#'
#' @return A single DOI string or `NA_character_`.
#'
#' @noRd
.convert_pmid_to_doi_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  req <- .scholidonline_request(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
  )
  req <- .scholidonline_req_url_query(
    req = req,
    db = "pubmed",
    id = x,
    retmode = "json",
    !!!
      list(...)
  )
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("NCBI request failed.")
    }
    return(NA_character_)
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("NCBI request returned HTTP ", status, "."))
    }
    return(NA_character_)
  }
  
  js <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = FALSE
  )
  
  rec <- js$result[[x]]
  
  if (is.null(rec) || is.null(rec$articleids)) {
    return(NA_character_)
  }
  
  ids <- rec$articleids
  
  if (is.data.frame(ids) && "idtype" %in% names(ids)) {
    hit <- ids[ids$idtype == "doi", , drop = FALSE]
    
    if (nrow(hit) < 1L) {
      return(NA_character_)
    }
    
    return(as.character(hit$value[[1]]))
  }
  
  if (is.list(ids)) {
    for (i in seq_along(ids)) {
      if (isTRUE(ids[[i]]$idtype == "doi")) {
        return(as.character(ids[[i]]$value))
      }
    }
  }
  
  NA_character_
}


#' NCBI: DOI -> PMID
#'
#' @param x A single DOI string.
#' @param ... Passed to NCBI E-utilities (e.g. `api_key`, `tool`, `email`).
#' @param quiet Logical.
#'
#' @return A single PMID string or `NA_character_`.
#'
#' @noRd
.convert_doi_to_pmid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  term <- paste0("\"", x, "\"[DOI]")
  
  req <- .scholidonline_request(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  )
  req <- .scholidonline_req_url_query(
    req = req,
    db = "pubmed",
    term = term,
    retmode = "json",
    !!!
      list(...)
  )
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("NCBI request failed.")
    }
    return(NA_character_)
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("NCBI request returned HTTP ", status, "."))
    }
    return(NA_character_)
  }
  
  js <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = FALSE
  )
  
  ids <- js$esearchresult$idlist
  
  if (is.null(ids) || length(ids) < 1L) {
    return(NA_character_)
  }
  
  as.character(ids[[1]])
}


#' NCBI: PMCID -> PMID
#'
#' @param x A single PMCID string.
#' @param ... Passed to PMC ID Converter (e.g. `tool`, `email`).
#' @param quiet Logical.
#'
#' @return A single PMID string or `NA_character_`.
#'
#' @noRd
.convert_pmcid_to_pmid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  req <- .scholidonline_request(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
  )
  req <- .scholidonline_req_url_query(
    req = req,
    format = "json",
    ids = x,
    !!!
      list(...)
  )
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("NCBI request failed.")
    }
    return(NA_character_)
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("NCBI request returned HTTP ", status, "."))
    }
    return(NA_character_)
  }
  
  js <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = FALSE
  )
  
  recs <- js$records
  
  if (is.null(recs) || length(recs) < 1L) {
    return(NA_character_)
  }
  
  val <- recs[[1]]$pmid
  
  if (is.null(val) || is.na(val) || !nzchar(val)) {
    return(NA_character_)
  }
  
  as.character(val)
}


#' NCBI: PMCID -> DOI
#'
#' @param x A single PMCID string.
#' @param ... Passed to PMC ID Converter (e.g. `tool`, `email`).
#' @param quiet Logical.
#'
#' @return A single DOI string or `NA_character_`.
#'
#' @noRd
.convert_pmcid_to_doi_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  req <- .scholidonline_request(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
  )
  req <- .scholidonline_req_url_query(
    req = req,
    format = "json",
    ids = x,
    !!!
      list(...)
  )
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("NCBI request failed.")
    }
    return(NA_character_)
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("NCBI request returned HTTP ", status, "."))
    }
    return(NA_character_)
  }
  
  js <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = FALSE
  )
  
  recs <- js$records
  
  if (is.null(recs) || length(recs) < 1L) {
    return(NA_character_)
  }
  
  val <- recs[[1]]$doi
  
  if (is.null(val) || is.na(val) || !nzchar(val)) {
    return(NA_character_)
  }
  
  as.character(val)
}


#' NCBI: PMID -> PMCID
#'
#' @param x A single PMID string.
#' @param ... Passed to PMC ID Converter (e.g. `tool`, `email`).
#' @param quiet Logical.
#'
#' @return A single PMCID string or `NA_character_`.
#'
#' @noRd
.convert_pmid_to_pmcid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  req <- .scholidonline_request(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
  )
  req <- .scholidonline_req_url_query(
    req = req,
    format = "json",
    ids = x,
    !!!
      list(...)
  )
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("NCBI request failed.")
    }
    return(NA_character_)
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("NCBI request returned HTTP ", status, "."))
    }
    return(NA_character_)
  }
  
  js <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = FALSE
  )
  
  recs <- js$records
  
  if (is.null(recs) || length(recs) < 1L) {
    return(NA_character_)
  }
  
  val <- recs[[1]]$pmcid
  
  if (is.null(val) || is.na(val) || !nzchar(val)) {
    return(NA_character_)
  }
  
  as.character(val)
}


#' NCBI: DOI -> PMCID
#'
#' @param x A single DOI string.
#' @param ... Passed to PMC ID Converter (e.g. `tool`, `email`).
#' @param quiet Logical.
#'
#' @return A single PMCID string or `NA_character_`.
#'
#' @noRd
.convert_doi_to_pmcid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  req <- .scholidonline_request(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
  )
  req <- .scholidonline_req_url_query(
    req = req,
    format = "json",
    ids = x,
    !!!
      list(...)
  )
  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform_safe(req = req)
  
  if (is.null(resp)) {
    if (!isTRUE(quiet)) {
      rlang::warn("NCBI request failed.")
    }
    return(NA_character_)
  }
  
  status <- .scholidonline_resp_status(resp = resp)
  
  if (status < 200L || status >= 300L) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0("NCBI request returned HTTP ", status, "."))
    }
    return(NA_character_)
  }
  
  js <- .scholidonline_resp_body_json(
    resp = resp,
    simplifyVector = FALSE
  )
  
  recs <- js$records
  
  if (is.null(recs) || length(recs) < 1L) {
    return(NA_character_)
  }
  
  val <- recs[[1]]$pmcid
  
  if (is.null(val) || is.na(val) || !nzchar(val)) {
    return(NA_character_)
  }
  
  as.character(val)
}