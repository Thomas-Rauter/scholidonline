#' DOI.org: check whether a DOI exists
#'
#' @param x A single, normalized DOI string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_doi_doi_org <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://doi.org/",
    utils::URLencode(
      x,
      reserved = TRUE
    )
  )
  
  resp <- .scholidonline_http_get(
    url = url,
    headers = c(
      Accept = "application/vnd.citationstyles.csl+json"
    ),
    quiet = quiet
  )

  .scholidonline_http_exists_from_response(
    resp = resp,
    quiet = quiet,
    provider_label = "DOI.org"
  )
}


#' DOI.org: retrieve metadata for a DOI
#'
#' @description
#' Provider implementation for retrieving metadata for a DOI using the
#' DOI.org content negotiation API.
#'
#' @param x A single, normalized DOI string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the DOI.
#'
#' @noRd
.meta_doi_doi_org <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  
  url <- paste0(
    "https://doi.org/",
    utils::URLencode(
      x,
      reserved = TRUE
    )
  )
  
  obj <- .scholidonline_http_get_json(
    url = url,
    quiet = quiet,
    provider_label = "DOI.org",
    headers = c(
      Accept = "application/vnd.citationstyles.csl+json"
    ),
    silent_404 = TRUE,
    simplifyVector = TRUE
  )

  if (is.null(obj)) {
    return(data.frame())
  }
  
  data.frame(
    title = obj$title %||% NA_character_,
    year = if (!is.null(obj$issued$`date-parts`)) {
      obj$issued$`date-parts`[[1]][1]
    } else {
      NA_integer_
    },
    container        = obj$`container-title` %||% NA_character_,
    doi              = obj$DOI %||% x,
    pmid             = NA_character_,
    pmcid            = NA_character_,
    url              = obj$URL %||% NA_character_,
    provider         = "doi.org",
    stringsAsFactors = FALSE
  )
}