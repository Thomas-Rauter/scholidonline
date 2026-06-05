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


#' NCBI: check whether PMIDs exist using one batch request
#'
#' @param x A character vector of normalized PMID strings.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A logical vector with one value per input.
#'
#' @noRd
.exists_pmid_ncbi_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  out <- rep(NA, length(x))
  
  valid <- !is.na(x) & nzchar(x)
  
  if (!any(valid)) {
    return(out)
  }
  
  x_valid <- x[valid]
  
  js <- .scholidonline_esummary_pubmed(
    id = paste(x_valid, collapse = ","),
    ...,
    quiet = quiet
  )
  
  if (is.null(js) || is.null(js$result)) {
    return(out)
  }
  
  uids <- character()
  
  if (!is.null(js$result$uids)) {
    uids <- unlist(
      js$result$uids,
      use.names = FALSE
    )
  }
  
  out_valid <- rep(NA, length(x_valid))
  
  for (i in seq_along(x_valid)) {
    xi <- x_valid[[i]]
    rec <- js$result[[xi]]
    
    if (is.null(rec)) {
      if (length(uids) > 0L && !xi %in% uids) {
        out_valid[[i]] <- FALSE
      }
      
      next
    }
    
    if (!is.null(rec$error)) {
      out_valid[[i]] <- FALSE
      next
    }
    
    uid <- rec$uid %||% NA_character_
    
    out_valid[[i]] <- is.character(uid) &&
      length(uid) == 1L &&
      identical(uid, xi)
  }
  
  out[valid] <- out_valid
  
  out
}


#' NCBI: check whether PMCIDs exist using one batch request
#'
#' @param x A character vector of normalized PMCID strings.
#' @param ... Passed to PMC ID Converter.
#' @param quiet Logical.
#'
#' @return A logical vector with one value per input.
#'
#' @noRd
.exists_pmcid_ncbi_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  out <- rep(NA, length(x))
  
  valid <- !is.na(x) & nzchar(x)
  
  if (!any(valid)) {
    return(out)
  }
  
  x_valid <- x[valid]
  
  js <- .scholidonline_pmc_idconv(
    ids = paste(x_valid, collapse = ","),
    ...,
    quiet = quiet
  )
  
  if (is.null(js) || is.null(js$records) || length(js$records) < 1L) {
    return(out)
  }
  
  records <- js$records
  
  rec_key <- vapply(
    records,
    function(rec) {
      if (!is.null(rec$requested_id) && nzchar(rec$requested_id)) {
        return(rec$requested_id)
      }
      
      if (!is.null(rec$pmcid) && nzchar(rec$pmcid)) {
        return(rec$pmcid)
      }
      
      NA_character_
    },
    character(1)
  )
  
  out_valid <- rep(NA, length(x_valid))
  
  for (i in seq_along(x_valid)) {
    xi <- x_valid[[i]]
    hit <- match(xi, rec_key)
    
    if (is.na(hit)) {
      next
    }
    
    rec <- records[[hit]]
    
    if (!is.null(rec$status) && identical(rec$status, "error")) {
      out_valid[[i]] <- FALSE
      next
    }
    
    if (!is.null(rec$pmcid) && nzchar(rec$pmcid)) {
      out_valid[[i]] <- TRUE
      next
    }
    
    out_valid[[i]] <- FALSE
  }
  
  out[valid] <- out_valid
  
  out
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
  
  json <- .scholidonline_http_get_json(
    url = url,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    }
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
  
  json <- .scholidonline_http_get_json(
    url = url,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    }
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


#' NCBI: return linked identifiers using one batch request
#'
#' @description
#' Provider adapter retrieving identifiers linked to PMIDs or PMCIDs using one
#' NCBI ID Converter API request.
#'
#' @param x A character vector of normalized PMID or PMCID strings.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `query_id`, `linked_type`,
#'   `linked_value`, and `provider`.
#'
#' @noRd
.links_ncbi_idconv_batch <- function(
    x,
    query_type,
    ...,
    quiet = FALSE
) {
  rlang::check_dots_empty()
  
  if (
    !is.character(query_type) ||
    length(query_type) != 1L ||
    is.na(query_type) ||
    !query_type %in% c("pmid", "pmcid")
  ) {
    stop(
      "`query_type` must be either \"pmid\" or \"pmcid\".",
      call. = FALSE
    )
  }
  
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  valid <- !is.na(x) & nzchar(x)
  
  if (!any(valid)) {
    return(data.frame())
  }
  
  x_valid <- x[valid]
  
  url <- paste0(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=",
    utils::URLencode(
      paste(x_valid, collapse = ","),
      reserved = TRUE
    ),
    "&format=json"
  )
  
  json <- .scholidonline_http_get_json(
    url = url,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    }
  )

  if (is.null(json)) {
    return(data.frame())
  }
  
  records <- json$records
  
  if (is.null(records) || length(records) == 0L) {
    return(data.frame())
  }
  
  rows <- list()
  
  for (rec in records) {
    if (!is.null(rec$status) && identical(rec$status, "error")) {
      next
    }
    
    query_id <- if (identical(query_type, "pmid")) {
      rec$pmid %||% NA_character_
    } else {
      rec$pmcid %||% NA_character_
    }
    
    if (is.na(query_id) || !nzchar(query_id)) {
      next
    }
    
    if (!is.null(rec$pmid)) {
      rows[[length(rows) + 1L]] <- data.frame(
        query_id = query_id,
        linked_type = "pmid",
        linked_value = as.character(rec$pmid),
        provider = "ncbi",
        stringsAsFactors = FALSE
      )
    }
    
    if (!is.null(rec$pmcid)) {
      rows[[length(rows) + 1L]] <- data.frame(
        query_id = query_id,
        linked_type = "pmcid",
        linked_value = as.character(rec$pmcid),
        provider = "ncbi",
        stringsAsFactors = FALSE
      )
    }
    
    if (!is.null(rec$doi)) {
      rows[[length(rows) + 1L]] <- data.frame(
        query_id = query_id,
        linked_type = "doi",
        linked_value = as.character(rec$doi),
        provider = "ncbi",
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(rows) == 0L) {
    return(data.frame())
  }
  
  do.call(
    rbind,
    rows
  )
}


#' NCBI: return identifiers linked to PMIDs using one batch request
#'
#' @param x A character vector of normalized PMID strings.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `query_id`, `linked_type`,
#'   `linked_value`, and `provider`.
#'
#' @noRd
.links_pmid_ncbi_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  .links_ncbi_idconv_batch(
    x = x,
    query_type = "pmid",
    ...,
    quiet = quiet
  )
}


#' NCBI: return identifiers linked to PMCIDs using one batch request
#'
#' @param x A character vector of normalized PMCID strings.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `query_id`, `linked_type`,
#'   `linked_value`, and `provider`.
#'
#' @noRd
.links_pmcid_ncbi_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  .links_ncbi_idconv_batch(
    x = x,
    query_type = "pmcid",
    ...,
    quiet = quiet
  )
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
  
  obj <- .scholidonline_http_get_json(
    url = url,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    },
    simplifyVector = TRUE
  )

  if (is.null(obj)) {
    return(data.frame())
  }
  
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


#' NCBI: retrieve metadata for PMIDs using one batch request
#'
#' @description
#' Provider implementation for retrieving metadata for multiple PMIDs using
#' one NCBI E-utilities (esummary) API request.
#'
#' @param x A character vector of normalized PMID strings.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata rows for resolved PMIDs.
#'
#' @noRd
.meta_pmid_ncbi_batch <- function(
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
  
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
    "?db=pubmed&id=",
    utils::URLencode(
      paste(x_valid, collapse = ","),
      reserved = TRUE
    ),
    "&retmode=json"
  )
  
  obj <- .scholidonline_http_get_json(
    url = url,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    },
    simplifyVector = TRUE
  )

  if (is.null(obj)) {
    return(data.frame())
  }
  
  if (is.null(obj$result)) {
    return(data.frame())
  }
  
  rows <- lapply(
    x_valid,
    function(xi) {
      rec <- obj$result[[xi]]
      
      if (is.null(rec) || !is.null(rec$error)) {
        return(data.frame())
      }
      
      data.frame(
        pmid_key = xi,
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
        pmid = xi,
        pmcid = NA_character_,
        url = paste0(
          "https://pubmed.ncbi.nlm.nih.gov/",
          xi,
          "/"
        ),
        provider = "ncbi",
        stringsAsFactors = FALSE
      )
    }
  )
  
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  
  if (length(rows) < 1L) {
    return(data.frame())
  }
  
  do.call(
    rbind,
    rows
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
  
  obj <- .scholidonline_http_get_json(
    url = url,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    },
    simplifyVector = TRUE
  )

  if (is.null(obj)) {
    return(data.frame())
  }
  
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


#' NCBI: retrieve metadata for PMCIDs using one batch request
#'
#' @description
#' Provider implementation for retrieving metadata for multiple PMCIDs using
#' one NCBI E-utilities (esummary) API request.
#'
#' @param x A character vector of normalized PMCID strings.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata rows for resolved PMCIDs.
#'
#' @noRd
.meta_pmcid_ncbi_batch <- function(
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
  keys <- gsub("^PMC", "", x_valid)
  
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
    "?db=pmc&id=",
    utils::URLencode(
      paste(keys, collapse = ","),
      reserved = TRUE
    ),
    "&retmode=json"
  )
  
  obj <- .scholidonline_http_perform_json_body(
    url = url,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    },
    simplifyVector = TRUE,
    parse_failure_warn = "NCBI response could not be parsed as JSON."
  )

  if (is.null(obj)) {
    return(data.frame())
  }

  if (is.null(obj$result)) {
    if (!isTRUE(quiet)) {
      rlang::warn("NCBI response could not be parsed as JSON.")
    }
    return(data.frame())
  }
  
  rows <- lapply(
    seq_along(x_valid),
    function(i) {
      xi <- x_valid[[i]]
      key <- keys[[i]]
      rec <- obj$result[[key]]
      
      if (is.null(rec)) {
        return(data.frame())
      }
      
      if (!is.null(rec$error)) {
        return(data.frame())
      }
      
      if (
        (is.null(rec$title) || is.na(rec$title) || !nzchar(rec$title)) &&
        (is.null(rec$source) || is.na(rec$source) || !nzchar(rec$source)) &&
        (is.null(rec$pubdate) || is.na(rec$pubdate) || !nzchar(rec$pubdate))
      ) {
        return(data.frame())
      }
      
      data.frame(
        pmcid_key = xi,
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
        pmcid = xi,
        url = paste0(
          "https://www.ncbi.nlm.nih.gov/pmc/articles/",
          xi,
          "/"
        ),
        provider = "ncbi",
        stringsAsFactors = FALSE
      )
    }
  )
  
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  
  if (length(rows) < 1L) {
    return(data.frame())
  }
  
  do.call(
    rbind,
    rows
  )
}


# id_convert() provider functions ----------------------------------------------

## Level 3 functions (functions called by level 2 functions) --------------------


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

  js <- .scholidonline_http_request_json(
    req = req,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    },
    simplifyVector = FALSE
  )

  if (is.null(js)) {
    return(NA_character_)
  }
  
  rec <- js$result[[x]]
  
  if (is.null(rec) || is.null(rec$articleids)) {
    return(NA_character_)
  }
  
  .convert_ncbi_articleids_to_doi(
    ids = rec$articleids
  )
}


#' NCBI: PMID -> DOI in batch
#'
#' @param x A character vector of PMID strings.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A character vector of DOI values.
#'
#' @noRd
.convert_pmid_to_doi_ncbi_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  out <- rep(NA_character_, length(x))
  
  valid <- !is.na(x) & nzchar(x)
  
  if (!any(valid)) {
    return(out)
  }
  
  x_valid <- x[valid]
  
  js <- .scholidonline_esummary_pubmed(
    id = paste(x_valid, collapse = ","),
    ...,
    quiet = quiet
  )
  
  if (is.null(js) || is.null(js$result)) {
    return(out)
  }
  
  out_valid <- rep(NA_character_, length(x_valid))
  
  for (i in seq_along(x_valid)) {
    xi <- x_valid[[i]]
    rec <- js$result[[xi]]
    
    if (is.null(rec) || !is.null(rec$error)) {
      next
    }
    
    out_valid[[i]] <- .convert_ncbi_articleids_to_doi(
      ids = rec$articleids
    )
  }
  
  out[valid] <- out_valid
  
  out
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

  js <- .scholidonline_http_perform_json_body(
    req = req,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    },
    parse_failure_warn = "NCBI response could not be parsed as JSON."
  )

  if (is.null(js)) {
    return(NA_character_)
  }
  
  ids <- js$esearchresult$idlist
  
  if (is.null(ids) || length(ids) < 1L) {
    return(NA_character_)
  }
  
  as.character(ids[[1]])
}


#' NCBI: DOI -> PMID in batch
#'
#' @param x A character vector of DOI strings.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A character vector of PMID values.
#'
#' @noRd
.convert_doi_to_pmid_ncbi_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  out <- rep(NA_character_, length(x))
  
  valid <- !is.na(x) & nzchar(x)
  
  if (!any(valid)) {
    return(out)
  }
  
  x_valid <- x[valid]
  query_key <- tolower(x_valid)
  
  terms <- paste0(
    "\"",
    x_valid,
    "\"[DOI]"
  )
  
  req <- .scholidonline_request(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  )
  req <- .scholidonline_req_url_query(
    req = req,
    db = "pubmed",
    term = paste(terms, collapse = " OR "),
    retmode = "json",
    retmax = length(x_valid),
    !!!
      list(...)
  )

  search <- .scholidonline_http_perform_json_body(
    req = req,
    quiet = quiet,
    provider_label = "NCBI",
    parse_failure_warn = "NCBI response could not be parsed as JSON."
  )

  if (is.null(search)) {
    return(out)
  }
  
  pmids <- search$esearchresult$idlist
  
  if (is.null(pmids) || length(pmids) < 1L) {
    return(out)
  }
  
  pmids <- as.character(pmids)
  
  js <- .scholidonline_esummary_pubmed(
    id = paste(pmids, collapse = ","),
    ...,
    quiet = quiet
  )
  
  if (is.null(js) || is.null(js$result)) {
    return(out)
  }
  
  returned_doi <- rep(NA_character_, length(pmids))
  
  for (i in seq_along(pmids)) {
    rec <- js$result[[pmids[[i]]]]
    
    if (is.null(rec) || !is.null(rec$error)) {
      next
    }
    
    returned_doi[[i]] <- tolower(
      .convert_ncbi_articleids_to_doi(
        ids = rec$articleids
      )
    )
  }
  
  out_valid <- rep(NA_character_, length(x_valid))
  
  for (i in seq_along(x_valid)) {
    hit <- match(query_key[[i]], returned_doi)
    
    if (!is.na(hit)) {
      out_valid[[i]] <- pmids[[hit]]
    }
  }
  
  out[valid] <- out_valid
  
  out
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

  js <- .scholidonline_http_request_json(
    req = req,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    },
    simplifyVector = FALSE
  )

  if (is.null(js)) {
    return(NA_character_)
  }
  
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

  js <- .scholidonline_http_request_json(
    req = req,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    },
    simplifyVector = FALSE
  )

  if (is.null(js)) {
    return(NA_character_)
  }
  
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

  js <- .scholidonline_http_request_json(
    req = req,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    },
    simplifyVector = FALSE
  )

  if (is.null(js)) {
    return(NA_character_)
  }
  
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

  js <- .scholidonline_http_request_json(
    req = req,
    quiet = quiet,
    provider_label = "NCBI",
    before_request = function() {
      .ncbi_rate_limit(quiet = quiet)
    },
    simplifyVector = FALSE
  )

  if (is.null(js)) {
    return(NA_character_)
  }
  
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


#' NCBI: PMID -> PMCID in batch
#'
#' @param x A character vector of PMID strings.
#' @param ... Passed to PMC ID Converter.
#' @param quiet Logical.
#'
#' @return A character vector of PMCID values.
#'
#' @noRd
.convert_pmid_to_pmcid_ncbi_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  .convert_ncbi_idconv_batch(
    x = x,
    from = "pmid",
    to = "pmcid",
    ...,
    quiet = quiet
  )
}


#' NCBI: PMCID -> PMID in batch
#'
#' @param x A character vector of PMCID strings.
#' @param ... Passed to PMC ID Converter.
#' @param quiet Logical.
#'
#' @return A character vector of PMID values.
#'
#' @noRd
.convert_pmcid_to_pmid_ncbi_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  .convert_ncbi_idconv_batch(
    x = x,
    from = "pmcid",
    to = "pmid",
    ...,
    quiet = quiet
  )
}


#' NCBI: PMCID -> DOI in batch
#'
#' @param x A character vector of PMCID strings.
#' @param ... Passed to PMC ID Converter.
#' @param quiet Logical.
#'
#' @return A character vector of DOI values.
#'
#' @noRd
.convert_pmcid_to_doi_ncbi_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  .convert_ncbi_idconv_batch(
    x = x,
    from = "pmcid",
    to = "doi",
    ...,
    quiet = quiet
  )
}


#' NCBI: DOI -> PMCID in batch
#'
#' @param x A character vector of DOI strings.
#' @param ... Passed to PMC ID Converter.
#' @param quiet Logical.
#'
#' @return A character vector of PMCID values.
#'
#' @noRd
.convert_doi_to_pmcid_ncbi_batch <- function(
    x,
    ...,
    quiet = FALSE
) {
  .convert_ncbi_idconv_batch(
    x = x,
    from = "doi",
    to = "pmcid",
    ...,
    quiet = quiet
  )
}


## Level 4 functions (functions called by level 3 functions) --------------------


#' NCBI PubMed ESummary: extract DOI from article IDs
#'
#' @param ids An `articleids` object from an NCBI PubMed ESummary record.
#'
#' @return A single DOI string, or `NA_character_`.
#'
#' @noRd
.convert_ncbi_articleids_to_doi <- function(ids) {
  if (is.null(ids)) {
    return(NA_character_)
  }
  
  if (is.data.frame(ids) && "idtype" %in% names(ids)) {
    hit <- ids[ids$idtype == "doi", , drop = FALSE]
    
    if (nrow(hit) < 1L || !"value" %in% names(hit)) {
      return(NA_character_)
    }
    
    value <- hit$value[[1L]]
    
    if (is.null(value) || is.na(value) || !nzchar(value)) {
      return(NA_character_)
    }
    
    return(as.character(value))
  }
  
  if (is.list(ids)) {
    for (i in seq_along(ids)) {
      if (isTRUE(ids[[i]]$idtype == "doi")) {
        value <- ids[[i]]$value
        
        if (is.null(value) || is.na(value) || !nzchar(value)) {
          return(NA_character_)
        }
        
        return(as.character(value))
      }
    }
  }
  
  NA_character_
}


#' NCBI ID Converter: batch convert identifiers
#'
#' @description
#' Internal helper for batch conversions through the NCBI PMC ID Converter API.
#'
#' @param x A character vector of normalized identifiers.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param ... Passed to the NCBI ID Converter API.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A character vector with one value per input.
#'
#' @noRd
.convert_ncbi_idconv_batch <- function(
    x,
    from,
    to,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  if (
    !is.character(from) ||
    length(from) != 1L ||
    is.na(from) ||
    !from %in% c("pmid", "pmcid", "doi")
  ) {
    stop(
      "`from` must be one of \"pmid\", \"pmcid\", or \"doi\".",
      call. = FALSE
    )
  }
  
  if (
    !is.character(to) ||
    length(to) != 1L ||
    is.na(to) ||
    !to %in% c("pmid", "pmcid", "doi")
  ) {
    stop(
      "`to` must be one of \"pmid\", \"pmcid\", or \"doi\".",
      call. = FALSE
    )
  }
  
  out <- rep(NA_character_, length(x))
  
  valid <- !is.na(x) & nzchar(x)
  
  if (!any(valid)) {
    return(out)
  }
  
  x_valid <- x[valid]
  
  js <- .scholidonline_pmc_idconv(
    ids = paste(x_valid, collapse = ","),
    ...,
    quiet = quiet
  )
  
  if (is.null(js) || is.null(js$records) || length(js$records) < 1L) {
    return(out)
  }
  
  records <- js$records
  source_key <- .convert_ncbi_idconv_source_key(
    records = records,
    from = from
  )
  
  out_valid <- rep(NA_character_, length(x_valid))
  query_key <- .convert_ncbi_idconv_normalize_key(
    x = x_valid,
    type = from
  )
  
  for (i in seq_along(x_valid)) {
    hit <- match(query_key[[i]], source_key)
    
    if (is.na(hit)) {
      next
    }
    
    rec <- records[[hit]]
    
    if (!is.null(rec$status) && identical(rec$status, "error")) {
      next
    }
    
    value <- .convert_ncbi_idconv_record_value(
      rec = rec,
      type = to
    )
    
    if (!is.na(value) && nzchar(value)) {
      out_valid[[i]] <- value
    }
  }
  
  out[valid] <- out_valid
  
  out
}


## Level 5 functions (functions called by level 4 functions) --------------------


#' NCBI ID Converter: extract source keys from records
#'
#' @param records A list of NCBI ID Converter records.
#' @param from A single source identifier type string.
#'
#' @return A character vector of source keys.
#'
#' @noRd
.convert_ncbi_idconv_source_key <- function(
    records,
    from
) {
  vapply(
    records,
    function(rec) {
      value <- .convert_ncbi_idconv_record_value(
        rec = rec,
        type = from
      )
      
      .convert_ncbi_idconv_normalize_key(
        x = value,
        type = from
      )
    },
    character(1)
  )
}


## Level 6 functions (functions called by level 5 functions) --------------------


#' NCBI ID Converter: extract typed value from one record
#'
#' @param rec A single NCBI ID Converter record.
#' @param type A single identifier type string.
#'
#' @return A single character value, or `NA_character_`.
#'
#' @noRd
.convert_ncbi_idconv_record_value <- function(
    rec,
    type
) {
  value <- switch(
    type,
    pmid = rec$pmid %||% NA_character_,
    pmcid = rec$pmcid %||% NA_character_,
    doi = rec$doi %||% NA_character_,
    NA_character_
  )
  
  if (is.null(value) || length(value) < 1L || is.na(value[[1L]])) {
    return(NA_character_)
  }
  
  as.character(value[[1L]])
}


#' NCBI ID Converter: normalize matching keys
#'
#' @param x A character vector.
#' @param type A single identifier type string.
#'
#' @return A character vector.
#'
#' @noRd
.convert_ncbi_idconv_normalize_key <- function(
    x,
    type
) {
  out <- as.character(x)
  
  out[is.na(out)] <- NA_character_
  
  if (identical(type, "doi")) {
    out <- tolower(out)
  }
  
  out
}
