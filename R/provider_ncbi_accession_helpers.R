# Shared helpers for NCBI accession provider implementations


#' Query the NCBI Entrez ESummary API
#'
#' @description
#' Internal helper for querying the NCBI Entrez ESummary API for arbitrary
#' Entrez databases. Accession-specific provider implementations use this
#' helper for existence checks and metadata retrieval.
#'
#' @param db A single Entrez database name.
#' @param id A character vector of record identifiers.
#' @param ... Additional query parameters passed to the API.
#' @param quiet Logical; if `TRUE`, suppress warnings on failed requests.
#'
#' @return A parsed JSON object (list), or `NULL` on failure.
#'
#' @noRd
.scholidonline_esummary_entrez <- function(
    db,
    id,
    ...,
    quiet = FALSE
) {
  if (!is.character(db) || length(db) != 1L || is.na(db) || !nzchar(db)) {
    stop(
      "`db` must be a single, non-missing character string.",
      call. = FALSE
    )
  }

  if (!is.character(id) || length(id) < 1L) {
    stop("`id` must be a character vector.", call. = FALSE)
  }

  dots <- list(...)

  .ncbi_rate_limit(
    quiet = quiet
  )

  .scholidonline_req_json(
    url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
    query = c(
      list(
        db = db,
        id = id,
        retmode = "json"
      ),
      dots
    ),
    quiet = quiet
  )
}


#' Extract one record from an Entrez ESummary response
#'
#' @description
#' Resolve a single accession record from an ESummary JSON response. Records
#' may be keyed directly by the query identifier or by internal UIDs with the
#' accession stored in record fields.
#'
#' @param js Parsed ESummary JSON.
#' @param x A single normalized accession string.
#'
#' @return A list record, or `NULL` when no matching record is found.
#'
#' @noRd
.ncbi_accession_record_from_esummary <- function(js, x) {
  if (is.null(js) || is.null(js$result)) {
    return(NULL)
  }

  result <- js$result

  if (!is.null(result[[x]])) {
    return(result[[x]])
  }

  uids <- result$uids

  if (is.null(uids) || length(uids) == 0L) {
    return(NULL)
  }

  for (uid in uids) {
    rec <- result[[uid]]

    if (is.null(rec)) {
      next
    }

    accession <- rec$accession %||% rec$acc %||% NA_character_

    if (identical(as.character(accession), x)) {
      return(rec)
    }

    rec_id <- rec$id %||% rec$uid %||% NA_character_

    if (identical(as.character(rec_id), x)) {
      return(rec)
    }
  }

  NULL
}


#' Interpret an Entrez ESummary response for accession existence
#'
#' @description
#' Shared existence logic for NCBI accession provider implementations.
#'
#' @param js Parsed ESummary JSON.
#' @param x A single normalized accession string.
#'
#' @return `TRUE`, `FALSE`, or `NA`.
#'
#' @noRd
.ncbi_accession_exists_from_esummary <- function(js, x) {
  if (is.null(js) || is.null(js$result)) {
    return(NA)
  }

  rec <- .ncbi_accession_record_from_esummary(
    js = js,
    x = x
  )

  if (is.null(rec)) {
    uids <- js$result$uids

    if (!is.null(uids) && length(uids) == 0L) {
      return(FALSE)
    }

    if (
      !is.null(uids) &&
        !x %in% as.character(unlist(uids, use.names = FALSE))
    ) {
      return(FALSE)
    }

    return(NA)
  }

  if (!is.null(rec$error)) {
    return(FALSE)
  }

  TRUE
}


#' Extract a title-like value from an accession ESummary record
#'
#' @param rec A single ESummary record list.
#'
#' @return A single character string.
#'
#' @noRd
.ncbi_accession_title_from_record <- function(rec) {
  if (is.null(rec)) {
    return(NA_character_)
  }

  title <- rec$title %||% rec$caption %||% rec$name %||% NA_character_

  if (is.null(title) || !nzchar(title)) {
    NA_character_
  } else {
    as.character(title)
  }
}


#' Parse a publication year from an Entrez date-like value
#'
#' @param value A date or year value from an ESummary record.
#'
#' @return An integer year, or `NA_integer_`.
#'
#' @noRd
.ncbi_accession_year_from_value <- function(value) {
  if (is.null(value) || is.na(value) || !nzchar(as.character(value))) {
    return(NA_integer_)
  }

  year_chr <- substr(as.character(value), 1L, 4L)

  if (!grepl("^[0-9]{4}$", year_chr)) {
    return(NA_integer_)
  }

  as.integer(year_chr)
}


#' Build harmonized metadata for an NCBI accession record
#'
#' @description
#' Construct the standard scholidonline metadata data.frame for accession
#' types. Bibliographic identifier columns are always `NA`.
#'
#' @param title Record title.
#' @param year Optional publication or release year.
#' @param container Optional container or source label.
#' @param url Canonical record URL.
#' @param provider Provider label.
#'
#' @return A one-row data.frame.
#'
#' @noRd
.ncbi_accession_meta_frame <- function(
    title = NA_character_,
    year = NA_integer_,
    container = NA_character_,
    url = NA_character_,
    provider = "ncbi"
) {
  data.frame(
    title = title,
    year = year,
    container = container,
    doi = NA_character_,
    pmid = NA_character_,
    pmcid = NA_character_,
    url = url,
    provider = provider,
    stringsAsFactors = FALSE
  )
}
