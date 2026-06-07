#' NCBI: check whether a GEO accession exists
#'
#' @param x A single, normalized GEO accession string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_geo_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  js <- .ncbi_geo_fetch_esummary(
    x = x,
    ...,
    quiet = quiet
  )

  if (is.null(js) && is.null(.ncbi_geo_entrez_db(x))) {
    return(NA)
  }

  .ncbi_accession_exists_from_esummary(
    js = js,
    x = x
  )
}


#' NCBI: retrieve metadata for a GEO accession
#'
#' @description
#' Provider implementation for retrieving metadata for a GEO accession using
#' the NCBI Entrez ESummary API.
#'
#' @param x A single, normalized GEO accession string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the GEO accession.
#'
#' @noRd
.meta_geo_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (is.null(.ncbi_geo_entrez_db(x))) {
    return(data.frame())
  }

  js <- .ncbi_geo_fetch_esummary(
    x = x,
    ...,
    quiet = quiet
  )

  if (is.null(js)) {
    return(data.frame())
  }

  rec <- .ncbi_accession_record_from_esummary(
    js = js,
    x = x
  )

  if (is.null(rec)) {
    return(data.frame())
  }

  .ncbi_accession_meta_frame(
    title = .ncbi_accession_title_from_record(rec),
    year = .ncbi_geo_year_from_record(rec),
    container = .ncbi_geo_container_from_record(rec),
    url = .ncbi_geo_record_url(x)
  )
}
