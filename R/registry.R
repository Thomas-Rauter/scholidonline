# scholidonline identifier registry
#
# @description
# Internal registry defining the identifier types supported by the
# scholidonline package and their associated metadata.
#
# The registry is the single source of truth for identifier capabilities,
# including:
# - existence-check providers
# - default providers
# - supported identifier conversions
# - conversion providers
#
# Helper functions in this file expose registry metadata used by the
# exported front-end functions (e.g. `id_exists()`, `id_convert()`).


#' Supported scholidonline identifier types
#'
#' @description
#' Return the set of identifier types supported by the scholidonline package.
#'
#' This is the set of identifier types for which scholidonline provides
#' registry-backed functionality. Available operations vary by type; use
#' [scholidonline_capabilities()] to see which of existence checks,
#' metadata retrieval, link discovery, and identifier conversion are
#' supported for each type.
#'
#' @return A character vector of supported identifier type strings.
#'
#' @examples
#' scholidonline_types()
#' "doi" %in% scholidonline_types()
#'
#' @export
scholidonline_types <- function() {
    names(.scholidonline_registry())
}


# Level 1 function (functions called by exported functions) definitions --------


#' scholidonline registry
#'
#' @description
#' Internal registry describing all supported scholarly identifier types,
#' their capabilities, and provider-specific dispatch configuration.
#'
#' The registry is the central configuration object used by the
#' scholidonline engine to:
#'
#' - determine which operations are supported for each identifier type
#' - enumerate available providers for each operation
#' - resolve default providers when `provider = "auto"`
#' - dispatch to the correct implementation via dispatcher functions
#'
#' Each top-level entry corresponds to a single identifier type (e.g.
#' `"doi"`, `"pmid"`, `"pmcid"`).
#'
#' For each identifier type, the following operation blocks may be defined:
#'
#' \itemize{
#'   \item \code{exists}: check whether an identifier exists
#'   \item \code{links}: retrieve external links
#'   \item \code{meta}: retrieve metadata
#'   \item \code{convert}: convert to other identifier types
#' }
#'
#' Each operation block is a list containing:
#'
#' \itemize{
#'   \item \code{providers}: character vector of supported providers
#'   \item \code{default_provider}: default provider used when
#'     \code{provider = "auto"}
#'   \item \code{dispatcher}: name of the dispatcher function implementing
#'     the operation
#' }
#'
#' The \code{convert} block is itself a named list, where each element
#' corresponds to a target identifier type and defines the conversion
#' capabilities for that pair.
#'
#' This registry is consumed by both the unary and binary dispatch engines
#' (e.g. \code{.scholidonline_run_unary()} and
#' \code{.scholidonline_run_binary()}).
#'
#' @return
#' A named list representing the scholidonline registry. The list is ordered
#' alphabetically by identifier type.
#'
#' @details
#' This function is internal and not intended to be called directly by users.
#'
#' @noRd
.scholidonline_registry <- function() {
  reg <- list(
    
    assembly = list(
      exists = list(
        providers = c("auto", "ncbi"),
        default_provider = "ncbi",
        dispatcher = ".exists_assembly"
      ),
      meta = list(
        providers = c("auto", "ncbi"),
        default_provider = "ncbi",
        dispatcher = ".meta_assembly"
      ),
      convert = list()
    ),

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
    
    bioproject = list(
      exists = list(
        providers = c("auto", "ncbi"),
        default_provider = "ncbi",
        dispatcher = ".exists_bioproject"
      ),
      meta = list(
        providers = c("auto", "ncbi"),
        default_provider = "ncbi",
        dispatcher = ".meta_bioproject"
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
          default_provider = "ncbi",
          dispatcher = ".convert_doi_to_pmid"
        ),
        pmcid = list(
          providers = c("auto", "ncbi", "epmc"),
          default_provider = "ncbi",
          dispatcher = ".convert_doi_to_pmcid"
        )
      )
    ),

    geo = list(
      exists = list(
        providers = c("auto", "ncbi"),
        default_provider = "ncbi",
        dispatcher = ".exists_geo"
      ),
      meta = list(
        providers = c("auto", "ncbi"),
        default_provider = "ncbi",
        dispatcher = ".meta_geo"
      ),
      convert = list()
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

    openalex = list(
      exists = list(
        providers = c("auto", "openalex"),
        default_provider = "openalex",
        dispatcher = ".exists_openalex"
      ),
      links = list(
        providers = c("auto", "openalex"),
        default_provider = "openalex",
        dispatcher = ".links_openalex"
      ),
      meta = list(
        providers = c("auto", "openalex"),
        default_provider = "openalex",
        dispatcher = ".meta_openalex"
      ),
      convert = list(
        doi = list(
          providers = c("auto", "openalex"),
          default_provider = "openalex",
          dispatcher = ".convert_openalex_to_doi"
        ),
        pmid = list(
          providers = c("auto", "openalex"),
          default_provider = "openalex",
          dispatcher = ".convert_openalex_to_pmid"
        )
      )
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
          default_provider = "ncbi",
          dispatcher = ".convert_pmcid_to_pmid"
        ),
        doi = list(
          providers = c("auto", "ncbi", "epmc"),
          default_provider = "ncbi",
          dispatcher = ".convert_pmcid_to_doi"
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
          default_provider = "ncbi",
          dispatcher = ".convert_pmid_to_doi"
        ),
        pmcid = list(
          providers = c("auto", "ncbi", "epmc"),
          default_provider = "ncbi",
          dispatcher = ".convert_pmid_to_pmcid"
        )
      )
    ),

    refseq = list(
      exists = list(
        providers = c("auto", "ncbi"),
        default_provider = "ncbi",
        dispatcher = ".exists_refseq"
      ),
      meta = list(
        providers = c("auto", "ncbi"),
        default_provider = "ncbi",
        dispatcher = ".meta_refseq"
      ),
      convert = list()
    ),

    ror = list(
      exists = list(
        providers = c("auto", "ror"),
        default_provider = "ror",
        dispatcher = ".exists_ror"
      ),
      meta = list(
        providers = c("auto", "ror"),
        default_provider = "ror",
        dispatcher = ".meta_ror"
      ),
      convert = list()
    ),

    sra = list(
      exists = list(
        providers = c("auto", "ncbi"),
        default_provider = "ncbi",
        dispatcher = ".exists_sra"
      ),
      meta = list(
        providers = c("auto", "ncbi"),
        default_provider = "ncbi",
        dispatcher = ".meta_sra"
      ),
      convert = list()
    ),

    uniprot = list(
      exists = list(
        providers = c("auto", "uniprot"),
        default_provider = "uniprot",
        dispatcher = ".exists_uniprot"
      ),
      meta = list(
        providers = c("auto", "uniprot"),
        default_provider = "uniprot",
        dispatcher = ".meta_uniprot"
      ),
      convert = list()
    )
    
  )
  
  reg[order(names(reg))]
}


#' Read existence-check metadata from a registry object
#'
#' @param type A single identifier type string.
#' @param reg A scholidonline registry object.
#'
#' @return A list with `providers` and `default_provider`.
#'
#' @noRd
.scholidonline_registry_exists_meta <- function(
    type,
    reg = .scholidonline_registry()
) {
  meta <- reg[[type]]$exists

  if (is.null(meta)) {
    rlang::abort(
      paste0("Existence checking is not supported for `", type, "`.")
    )
  }

  meta
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
  .scholidonline_registry_exists_meta(type)
}


#' Read conversion metadata from a registry object
#'
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param reg A scholidonline registry object.
#'
#' @return A list with `providers` and `default_provider`.
#'
#' @noRd
.scholidonline_registry_conversion_meta <- function(
    from,
    to,
    reg = .scholidonline_registry()
) {
  meta <- reg[[from]]$convert[[to]]

  if (is.null(meta)) {
    rlang::abort(
      paste0("Unsupported conversion: ", from, " -> ", to, ".")
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
  .scholidonline_registry_conversion_meta(from, to)
}


# Level 2 function (functions called by level 1 functions) definitions ---------


#' Match and validate a scholidonline identifier type
#'
#' @description
#' Internal helper used to validate and normalize identifier type arguments
#' such as `from` and `to`.
#'
#' This function matches the input against the set of supported identifier
#' types defined by `scholidonline_types()` using `match.arg()`, ensuring that
#' only valid types are accepted.
#'
#' @param x A single character string specifying an identifier type.
#' @param arg Name of the argument being validated (used for error messages).
#'
#' @return A single, validated identifier type string.
#'
#' @noRd
.scholidonline_match_type <- function(
    x,
    arg = "x"
) {
  
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(
      "`", arg, "` must be a single, non-missing character string.",
      call. = FALSE
    )
  }
  
  match.arg(
    arg = x,
    choices = scholidonline_types()
  )
}