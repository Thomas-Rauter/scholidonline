#' OpenAlex: check whether an OpenAlex entity exists
#'
#' @param x A single, normalized OpenAlex key string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_openalex_openalex <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  url <- .openalex_api_url(x)

  if (is.null(url)) {
    return(NA)
  }

  resp <- .scholidonline_http_get(
    url = url,
    quiet = quiet
  )

  .scholidonline_http_exists_from_response(
    resp = resp,
    quiet = quiet,
    provider_label = "OpenAlex"
  )
}


#' OpenAlex: return identifiers linked to an OpenAlex work
#'
#' @description
#' Provider adapter retrieving DOI, PMID, and PMCID values associated with an
#' OpenAlex work record.
#'
#' @param x A single, normalized OpenAlex work key string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `linked_type`, `linked_value`, `provider`.
#'
#' @noRd
.links_openalex_openalex <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (!.openalex_is_work(x)) {
    return(data.frame())
  }

  work <- .openalex_fetch_entity_json(
    x = x,
    quiet = quiet,
    silent_404 = TRUE
  )

  if (is.null(work)) {
    return(data.frame())
  }

  ids <- .openalex_work_ids(work)
  rows <- list()

  if (!is.na(ids$doi) && nzchar(ids$doi)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "doi",
      linked_value = ids$doi,
      provider = "openalex",
      stringsAsFactors = FALSE
    )
  }

  if (!is.na(ids$pmid) && nzchar(ids$pmid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "pmid",
      linked_value = ids$pmid,
      provider = "openalex",
      stringsAsFactors = FALSE
    )
  }

  if (!is.na(ids$pmcid) && nzchar(ids$pmcid)) {
    rows[[length(rows) + 1L]] <- data.frame(
      linked_type = "pmcid",
      linked_value = ids$pmcid,
      provider = "openalex",
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L) {
    return(data.frame())
  }

  do.call(rbind, rows)
}


#' OpenAlex: retrieve metadata for an OpenAlex work
#'
#' @description
#' Provider implementation for retrieving bibliographic metadata for an
#' OpenAlex work using the OpenAlex API.
#'
#' @param x A single, normalized OpenAlex work key string.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the OpenAlex work.
#'
#' @noRd
.meta_openalex_openalex <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (!.openalex_is_work(x)) {
    return(data.frame())
  }

  work <- .openalex_fetch_entity_json(
    x = x,
    quiet = quiet,
    silent_404 = TRUE
  )

  if (is.null(work)) {
    return(data.frame())
  }

  ids <- .openalex_work_ids(work)

  data.frame(
    title = work$title %||% work$display_name %||% NA_character_,
    year = work$publication_year %||% NA_integer_,
    container = .openalex_work_container(work),
    doi = ids$doi,
    pmid = ids$pmid,
    pmcid = ids$pmcid,
    url = work$id %||% paste0("https://openalex.org/", x),
    provider = "openalex",
    stringsAsFactors = FALSE
  )
}


#' OpenAlex: convert an OpenAlex work to a DOI
#'
#' @param x A single, normalized OpenAlex work key string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single DOI string, or `NA_character_`.
#'
#' @noRd
.convert_openalex_to_doi_openalex <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (!.openalex_is_work(x)) {
    return(NA_character_)
  }

  work <- .openalex_fetch_entity_json(
    x = x,
    quiet = quiet,
    silent_404 = TRUE
  )

  if (is.null(work)) {
    return(NA_character_)
  }

  ids <- .openalex_work_ids(work)

  if (is.na(ids$doi) || !nzchar(ids$doi)) {
    NA_character_
  } else {
    ids$doi
  }
}


#' OpenAlex: convert an OpenAlex work to a PMID
#'
#' @param x A single, normalized OpenAlex work key string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single PMID string, or `NA_character_`.
#'
#' @noRd
.convert_openalex_to_pmid_openalex <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (!.openalex_is_work(x)) {
    return(NA_character_)
  }

  work <- .openalex_fetch_entity_json(
    x = x,
    quiet = quiet,
    silent_404 = TRUE
  )

  if (is.null(work)) {
    return(NA_character_)
  }

  ids <- .openalex_work_ids(work)

  if (is.na(ids$pmid) || !nzchar(ids$pmid)) {
    NA_character_
  } else {
    ids$pmid
  }
}
