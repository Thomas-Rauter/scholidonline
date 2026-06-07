#' UniProt: check whether a UniProt accession exists
#'
#' @param x A single, normalized UniProt accession string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_uniprot_uniprot <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  resp <- .scholidonline_http_get(
    url = .uniprot_api_url(x),
    quiet = quiet
  )

  .scholidonline_http_exists_from_response(
    resp = resp,
    quiet = quiet,
    provider_label = "UniProt"
  )
}


#' UniProt: retrieve metadata for a UniProt entry
#'
#' @description
#' Provider implementation for retrieving metadata for a UniProt accession
#' using the UniProt REST API.
#'
#' @param x A single, normalized UniProt accession string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the UniProt entry.
#'
#' @noRd
.meta_uniprot_uniprot <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  obj <- .uniprot_fetch_entry_json(
    x = x,
    quiet = quiet,
    silent_404 = TRUE
  )

  if (is.null(obj)) {
    return(data.frame())
  }

  data.frame(
    title = .uniprot_protein_name(obj),
    year = .uniprot_year_from_entry(obj),
    container = .uniprot_organism_name(obj),
    doi = NA_character_,
    pmid = NA_character_,
    pmcid = NA_character_,
    url = .uniprot_record_url(x, obj),
    provider = "uniprot",
    stringsAsFactors = FALSE
  )
}
