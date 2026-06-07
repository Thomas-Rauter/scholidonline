#' ROR: check whether a ROR iD exists
#'
#' @param x A single, normalized ROR iD string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_ror_ror <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  resp <- .scholidonline_http_get(
    url = .ror_api_url(x),
    quiet = quiet
  )

  .scholidonline_http_exists_from_response(
    resp = resp,
    quiet = quiet,
    provider_label = "ROR"
  )
}


#' ROR: retrieve metadata for an organization record
#'
#' @description
#' Provider implementation for retrieving metadata for a ROR organization
#' using the ROR API.
#'
#' @param x A single, normalized ROR iD string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the ROR organization.
#'
#' @noRd
.meta_ror_ror <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  obj <- .ror_fetch_organization_json(
    x = x,
    quiet = quiet,
    silent_404 = TRUE
  )

  if (is.null(obj)) {
    return(data.frame())
  }

  data.frame(
    title = .ror_display_name(obj),
    year = NA_integer_,
    container = .ror_country_name(obj),
    doi = NA_character_,
    pmid = NA_character_,
    pmcid = NA_character_,
    url = .ror_record_url(x, obj),
    provider = "ror",
    stringsAsFactors = FALSE
  )
}
