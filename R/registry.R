#' scholidonline identifier registry
#'
#' @description
#' Internal registry defining the identifier types supported by the
#' scholidonline package and their associated metadata.
#'
#' The registry is the single source of truth for identifier capabilities,
#' including:
#' - existence-check providers
#' - default providers
#' - supported identifier conversions
#' - conversion providers
#'
#' Helper functions in this file expose registry metadata used by the
#' exported front-end functions (e.g. `id_exists()`, `id_convert()`).


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


.scholidonline_registry <- function() {
  reg <- list(
    
    arxiv = list(
      exists = list(
        providers = c("auto", "arxiv"),
        default_provider = "arxiv",
        dispatcher = ".exists_arxiv"
      ),
      links = list(
        providers = c("auto", "arxiv"),
        default_provider = "arxiv",
        dispatcher = ".links_arxiv"
      ),
      meta = list(
        providers = c("auto", "arxiv"),
        default_provider = "arxiv",
        dispatcher = ".meta_arxiv"
      ),
      convert = list()
    ),
    
    doi = list(
      exists = list(
        providers = c("auto", "doi.org", "crossref"),
        default_provider = "doi.org",
        dispatcher = ".exists_doi"
      ),
      links = list(
        providers = c("auto", "crossref"),
        default_provider = "crossref",
        dispatcher = ".links_doi"
      ),
      meta = list(
        providers = c("auto", "crossref", "doi.org"),
        default_provider = "crossref",
        dispatcher = ".meta_doi"
      ),
      convert = list(
        pmid = list(
          providers = c("auto", "ncbi", "epmc"),
          default_provider = "ncbi"
        ),
        pmcid = list(
          providers = c("auto", "ncbi", "epmc"),
          default_provider = "ncbi"
        )
      )
    ),
    
    orcid = list(
      exists = list(
        providers = c("auto", "orcid"),
        default_provider = "orcid",
        dispatcher = ".exists_orcid"
      ),
      links = list(
        providers = c("auto", "orcid"),
        default_provider = "orcid",
        dispatcher = ".links_orcid"
      ),
      meta = list(
        providers = c("auto", "orcid"),
        default_provider = "orcid",
        dispatcher = ".meta_orcid"
      ),
      convert = list()
    ),
    
    pmcid = list(
      exists = list(
        providers = c("auto", "ncbi", "epmc"),
        default_provider = "ncbi",
        dispatcher = ".exists_pmcid"
      ),
      links = list(
        providers = c("auto", "ncbi", "epmc"),
        default_provider = "ncbi",
        dispatcher = ".links_pmcid"
      ),
      meta = list(
        providers = c("auto", "ncbi", "epmc"),
        default_provider = "ncbi",
        dispatcher = ".meta_pmcid"
      ),
      convert = list(
        pmid = list(
          providers = c("auto", "ncbi", "epmc"),
          default_provider = "ncbi"
        ),
        doi = list(
          providers = c("auto", "ncbi", "epmc"),
          default_provider = "ncbi"
        )
      )
    ),
    
    pmid = list(
      exists = list(
        providers = c("auto", "ncbi", "epmc"),
        default_provider = "ncbi",
        dispatcher = ".exists_pmid"
      ),
      links = list(
        providers = c("auto", "ncbi", "epmc"),
        default_provider = "ncbi",
        dispatcher = ".links_pmid"
      ),
      meta = list(
        providers = c("auto", "ncbi", "epmc"),
        default_provider = "ncbi",
        dispatcher = ".meta_pmid"
      ),
      convert = list(
        doi = list(
          providers = c("auto", "ncbi", "epmc"),
          default_provider = "ncbi"
        ),
        pmcid = list(
          providers = c("auto", "ncbi", "epmc"),
          default_provider = "ncbi"
        )
      )
    )
    
  )
  
  reg[order(names(reg))]
}


#' Get existence-check metadata for an identifier type
#'
#' @param type A single identifier type string.
#'
#' @return A list with `providers` and `default_provider`.
#'
#' @noRd
.scholidonline_exists_meta <- function(type) {
  type <- .scholidonline_match_type(type, arg = "type")
  reg <- .scholidonline_registry()
  meta <- reg[[type]]$exists
  
  if (is.null(meta)) {
    rlang::abort(
      paste0("Existence checking is not supported for `", type, "`.")
    )
  }
  
  meta
}


#' Get conversion metadata for a source/target type pair
#'
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#'
#' @return A list with `providers` and `default_provider`.
#'
#' @noRd
.scholidonline_conversion_meta <- function(
    from,
    to
) {
  from <- .scholidonline_match_type(from, arg = "from")
  to   <- .scholidonline_match_type(to, arg = "to")
  reg <- .scholidonline_registry()
  meta <- reg[[from]]$convert[[to]]
  
  if (is.null(meta)) {
    rlang::abort(
      paste0("Unsupported conversion: ", from, " -> ", to, ".")
    )
  }
  
  meta
}