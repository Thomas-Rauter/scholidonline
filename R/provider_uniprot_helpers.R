# UniProt provider helpers


#' Build a UniProt REST API URL for an accession
#'
#' @param x A single normalized UniProt accession string.
#'
#' @return A URL string.
#'
#' @noRd
.uniprot_api_url <- function(x) {
  paste0(
    "https://rest.uniprot.org/uniprotkb/",
    utils::URLencode(x, reserved = TRUE),
    "?format=json"
  )
}


#' Build the canonical UniProt landing-page URL for an accession
#'
#' @param x A single normalized UniProt accession string.
#' @param obj Parsed UniProt entry JSON.
#'
#' @return A single URL string.
#'
#' @noRd
.uniprot_record_url <- function(x, obj) {
  accession <- obj$primaryAccession %||% NA_character_

  if (!is.na(accession) && nzchar(accession)) {
    x <- as.character(accession)
  }

  paste0(
    "https://www.uniprot.org/uniprotkb/",
    utils::URLencode(x, reserved = TRUE)
  )
}


#' Fetch JSON for a UniProt entry
#'
#' @param x A single normalized UniProt accession string.
#' @param quiet Logical.
#' @param silent_404 Logical.
#'
#' @return Parsed JSON, or `NULL`.
#'
#' @noRd
.uniprot_fetch_entry_json <- function(
    x,
    quiet,
    silent_404 = TRUE
) {
  .scholidonline_http_get_json(
    url = .uniprot_api_url(x),
    quiet = quiet,
    provider_label = "UniProt",
    silent_404 = silent_404
  )
}


#' Extract the protein name from a UniProt entry
#'
#' @param obj Parsed UniProt entry JSON.
#'
#' @return A single character string.
#'
#' @noRd
.uniprot_protein_name <- function(obj) {
  if (is.null(obj)) {
    return(NA_character_)
  }

  protein_description <- obj$proteinDescription

  if (!is.null(protein_description$recommendedName$fullName$value)) {
    value <- protein_description$recommendedName$fullName$value

    if (!is.null(value) && nzchar(value)) {
      return(as.character(value))
    }
  }

  alt_names <- protein_description$alternativeNames

  if (!is.null(alt_names) && length(alt_names) > 0L) {
    value <- alt_names[[1]]$fullName$value

    if (!is.null(value) && nzchar(value)) {
      return(as.character(value))
    }
  }

  id <- obj$uniProtkbId %||% NA_character_

  if (is.null(id) || !nzchar(id)) {
    NA_character_
  } else {
    as.character(id)
  }
}


#' Extract the organism scientific name from a UniProt entry
#'
#' @param obj Parsed UniProt entry JSON.
#'
#' @return A single character string.
#'
#' @noRd
.uniprot_organism_name <- function(obj) {
  if (is.null(obj) || is.null(obj$organism)) {
    return(NA_character_)
  }

  name <- obj$organism$scientificName %||% NA_character_

  if (is.null(name) || !nzchar(name)) {
    NA_character_
  } else {
    as.character(name)
  }
}


#' Extract the first-publication year from a UniProt entry
#'
#' @param obj Parsed UniProt entry JSON.
#'
#' @return An integer year, or `NA_integer_`.
#'
#' @noRd
.uniprot_year_from_entry <- function(obj) {
  if (is.null(obj) || is.null(obj$entryAudit)) {
    return(NA_integer_)
  }

  .ncbi_accession_year_from_value(
    obj$entryAudit$firstPublicDate %||% NA_character_
  )
}
