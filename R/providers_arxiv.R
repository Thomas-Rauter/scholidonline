#' arXiv: check whether an arXiv identifier exists
#'
#' @param x A single, normalized arXiv identifier.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_arxiv_arxiv <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  rlang::check_dots_empty()

  .exists_arxiv_arxiv_batch(
    x = x,
    quiet = quiet
  )[[1L]]
}


#' arXiv: return identifiers linked to an arXiv record
#'
#' @description
#' Provider adapter retrieving identifiers linked to an arXiv record
#' using the arXiv API.
#'
#' @param x A single, normalized arXiv identifier.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages.
#'
#' @return A data.frame with columns `linked_type`, `linked_value`, `provider`.
#'
#' @noRd
.links_arxiv_arxiv <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  rlang::check_dots_empty()

  out <- .links_arxiv_arxiv_batch(
    x = x,
    quiet = quiet
  )

  if (nrow(out) < 1L) {
    return(data.frame())
  }

  out[
    ,
    c("linked_type", "linked_value", "provider"),
    drop = FALSE
  ]
}


#' arXiv: retrieve metadata for an arXiv identifier
#'
#' @description
#' Provider implementation for retrieving metadata for an arXiv identifier
#' using the arXiv API.
#'
#' @param x A single, normalized arXiv identifier.
#' @param ... Unused.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame containing metadata for the arXiv record.
#'
#' @noRd
.meta_arxiv_arxiv <- function(
    x,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)
  rlang::check_dots_empty()

  out <- .meta_arxiv_arxiv_batch(
    x = x,
    quiet = quiet
  )

  if (nrow(out) < 1L) {
    return(data.frame())
  }

  out[
    ,
    c(
      "title",
      "year",
      "container",
      "doi",
      "pmid",
      "pmcid",
      "url",
      "provider"
    ),
    drop = FALSE
  ]
}
