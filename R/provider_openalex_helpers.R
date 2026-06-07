# OpenAlex provider helpers


#' Map an OpenAlex key prefix to an API collection name
#'
#' @param x A single OpenAlex key string.
#'
#' @return A collection name, or `NULL` when unsupported.
#'
#' @noRd
.openalex_collection <- function(x) {
  switch(
    substr(x, 1L, 1L),
    W = "works",
    A = "authors",
    S = "sources",
    I = "institutions",
    T = "topics",
    K = "keywords",
    P = "publishers",
    F = "funders",
    G = "grants",
    NULL
  )
}


#' Test whether an OpenAlex key refers to a work entity
#'
#' @param x A single OpenAlex key string.
#'
#' @return A single logical value.
#'
#' @noRd
.openalex_is_work <- function(x) {
  identical(substr(x, 1L, 1L), "W")
}


#' Build optional OpenAlex polite-pool query parameters
#'
#' @return A named list, possibly empty.
#'
#' @noRd
.openalex_mailto_query <- function() {
  mailto <- getOption("scholidonline.openalex.mailto", NULL)

  if (
    is.character(mailto) &&
      length(mailto) == 1L &&
      !is.na(mailto) &&
      nzchar(mailto)
  ) {
    return(list(mailto = mailto))
  }

  list()
}


#' Build an OpenAlex API URL for a normalized key
#'
#' @param x A single OpenAlex key string.
#'
#' @return A URL string, or `NULL` when the key prefix is unsupported.
#'
#' @noRd
.openalex_api_url <- function(x) {
  collection <- .openalex_collection(x)

  if (is.null(collection)) {
    return(NULL)
  }

  url <- paste0(
    "https://api.openalex.org/",
    collection,
    "/",
    utils::URLencode(x, reserved = TRUE)
  )

  query <- .openalex_mailto_query()

  if (length(query) == 0L) {
    return(url)
  }

  paste0(
    url,
    "?",
    paste0(
      names(query),
      "=",
      utils::URLencode(query, reserved = TRUE),
      collapse = "&"
    )
  )
}


#' Fetch JSON for an OpenAlex entity
#'
#' @param x A single OpenAlex key string.
#' @param quiet Logical.
#' @param silent_404 Logical.
#'
#' @return Parsed JSON, or `NULL`.
#'
#' @noRd
.openalex_fetch_entity_json <- function(
    x,
    quiet,
    silent_404 = TRUE
) {
  url <- .openalex_api_url(x)

  if (is.null(url)) {
    return(NULL)
  }

  .scholidonline_http_get_json(
    url = url,
    quiet = quiet,
    provider_label = "OpenAlex",
    silent_404 = silent_404
  )
}


#' Normalize a DOI value from OpenAlex JSON
#'
#' @param doi A DOI string or URL.
#'
#' @return A normalized DOI string, or `NA_character_`.
#'
#' @noRd
.openalex_normalize_doi <- function(doi) {
  if (is.null(doi) || is.na(doi) || !nzchar(doi)) {
    return(NA_character_)
  }

  doi <- as.character(doi)
  doi <- sub(
    "^https?://(dx\\.)?doi\\.org/",
    "",
    doi,
    ignore.case = TRUE
  )

  trimws(doi)
}


#' Normalize a PMID value from OpenAlex JSON
#'
#' @param pmid A PMID string or PubMed URL.
#'
#' @return A normalized PMID string, or `NA_character_`.
#'
#' @noRd
.openalex_normalize_pmid <- function(pmid) {
  if (is.null(pmid) || is.na(pmid) || !nzchar(pmid)) {
    return(NA_character_)
  }

  pmid <- as.character(pmid)
  pmid <- sub(
    "^https?://pubmed\\.ncbi\\.nlm\\.nih\\.gov/",
    "",
    pmid,
    ignore.case = TRUE
  )
  pmid <- sub("/$", "", pmid)

  trimws(pmid)
}


#' Normalize a PMCID value from OpenAlex JSON
#'
#' @param pmcid A PMCID string or PMC URL.
#'
#' @return A normalized PMCID string, or `NA_character_`.
#'
#' @noRd
.openalex_normalize_pmcid <- function(pmcid) {
  if (is.null(pmcid) || is.na(pmcid) || !nzchar(pmcid)) {
    return(NA_character_)
  }

  pmcid <- as.character(pmcid)
  pmcid <- sub(
    "^https?://(www\\.)?ncbi\\.nlm\\.nih\\.gov/pmc/articles/",
    "",
    pmcid,
    ignore.case = TRUE
  )
  pmcid <- sub("/$", "", pmcid)

  if (!grepl("^PMC", pmcid, ignore.case = TRUE)) {
    pmcid <- paste0("PMC", pmcid)
  }

  toupper(pmcid)
}


#' Extract linked identifier values from an OpenAlex work object
#'
#' @param work Parsed OpenAlex work JSON.
#'
#' @return A named list with `doi`, `pmid`, and `pmcid`.
#'
#' @noRd
.openalex_work_ids <- function(work) {
  ids <- work$ids

  doi <- .openalex_normalize_doi(work$doi %||% ids$doi %||% NA_character_)
  pmid <- .openalex_normalize_pmid(ids$pmid %||% NA_character_)
  pmcid <- .openalex_normalize_pmcid(ids$pmcid %||% NA_character_)

  list(
    doi = doi,
    pmid = pmid,
    pmcid = pmcid
  )
}


#' Extract bibliographic container title from an OpenAlex work object
#'
#' @param work Parsed OpenAlex work JSON.
#'
#' @return A single character string.
#'
#' @noRd
.openalex_work_container <- function(work) {
  pl <- work$primary_location

  if (is.null(pl) || is.null(pl$source) || is.null(pl$source$display_name)) {
    return(NA_character_)
  }

  as.character(pl$source$display_name)
}
