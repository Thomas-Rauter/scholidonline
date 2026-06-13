# GEO-specific NCBI accession helpers


#' Map a GEO accession prefix to an Entrez database name
#'
#' @description
#' GEO accessions (`GDS`, `GSE`, `GSM`, `GPL`) are queried from the `gds`
#' database via Entrez ESearch and ESummary.
#'
#' @param x A single normalized GEO accession string.
#'
#' @return An Entrez database name, or `NULL` when unsupported.
#'
#' @noRd
.ncbi_geo_entrez_db <- function(x) {
  prefix <- toupper(substr(x, 1L, 3L))

  switch(
    prefix,
    GDS = "gds",
    GSE = "gds",
    GSM = "gds",
    GPL = "gds",
    NULL
  )
}


#' Build an Entrez ESearch term for a GEO accession
#'
#' @description
#' GEO accessions are filtered by Entrez entry type so platform queries such
#' as `GPL96` resolve to the platform record rather than related series.
#'
#' @param x A single normalized GEO accession string.
#'
#' @return A single Entrez query term.
#'
#' @noRd
.ncbi_geo_esearch_term <- function(x) {
  prefix <- toupper(substr(x, 1L, 3L))

  paste0(
    x,
    "[Accession] AND ",
    prefix,
    "[Entry Type]"
  )
}


#' Fetch an ESummary response for a GEO accession
#'
#' @param x A single normalized GEO accession string.
#' @param quiet Logical.
#' @param ... Passed to `.scholidonline_esummary_entrez()`.
#'
#' @return Parsed JSON, or `NULL` when routing or the request fails.
#'
#' @noRd
.ncbi_geo_fetch_esummary <- function(
    x,
    quiet,
    ...
) {
  db <- .ncbi_geo_entrez_db(x)

  if (is.null(db)) {
    return(NULL)
  }

  .ncbi_accession_fetch_esummary(
    db = db,
    x = x,
    esearch_term = .ncbi_geo_esearch_term(x),
    ...,
    quiet = quiet
  )
}


#' Build the canonical GEO record URL for an accession
#'
#' @param x A single normalized GEO accession string.
#'
#' @return A single URL string.
#'
#' @noRd
.ncbi_geo_record_url <- function(x) {
  paste0(
    "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",
    utils::URLencode(x, reserved = TRUE)
  )
}


#' Extract an organism label from a GEO ESummary record
#'
#' @param rec A single ESummary record list.
#'
#' @return A single character string.
#'
#' @noRd
.ncbi_geo_container_from_record <- function(rec) {
  if (is.null(rec)) {
    return(NA_character_)
  }

  taxon <- rec$taxon %||% rec$ncp_taxname %||% rec$tax %||% NA_character_

  if (is.null(taxon) || !nzchar(taxon)) {
    NA_character_
  } else {
    as.character(taxon)
  }
}


#' Extract a release year from a GEO ESummary record
#'
#' @param rec A single ESummary record list.
#'
#' @return An integer year, or `NA_integer_`.
#'
#' @noRd
.ncbi_geo_year_from_record <- function(rec) {
  if (is.null(rec)) {
    return(NA_integer_)
  }

  date_value <- rec$pdat %||% rec$PDAT %||% rec$update %||% NA_character_

  .ncbi_accession_year_from_value(date_value)
}
