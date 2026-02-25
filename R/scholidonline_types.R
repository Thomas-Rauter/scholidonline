#' Supported scholidonline identifier types
#'
#' @description
#' Returns the set of identifier types supported by the scholidonline package.
#'
#' This is the set of identifiers for which scholidonline provides registry-
#' backed functionality (existence checks, conversion, metadata, and links).
#'
#' @return A character vector of supported identifier type strings.
#' @examples
#' scholidonline_types()
#' "doi" %in% scholidonline_types()
#' @export
scholidonline_types <- function() {
    names(.scholidonline_registry())
}


# Level 1 function (functions called by exported functions) definitions --------


#' Internal scholidonline identifier registry
#'
#' @description
#' Internal helper that defines the supported identifier types for
#' scholidonline. This is the single source of truth for type names used by
#' exported helpers.
#'
#' Values are reserved for per-type metadata such as provider defaults or
#' conversion capabilities.
#'
#' @return A named list. Names are identifier types; values are reserved for
#'   per-type metadata.
#' @noRd
.scholidonline_registry <- function() {
    reg <- list(
        arxiv = list(),
        doi = list(),
        orcid = list(),
        pmcid = list(),
        pmid = list()
    )

    reg[order(names(reg))]
}
