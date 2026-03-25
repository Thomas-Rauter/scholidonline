# Level 1 function (functions called by exported functions) definitions --------


#' Return metadata for a DOI
#'
#' @description
#' Internal dispatcher for retrieving metadata for a DOI.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_doi_<provider>()`.
#'
#' If `provider = "auto"`, the dispatcher uses Crossref.
#'
#' @param x A single, normalized DOI string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the DOI.
#'
#' @noRd
.meta_doi <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    provider <- "crossref"
  }
  
  switch(
    provider,
    crossref = .meta_doi_crossref(
      x = x,
      ...,
      quiet = quiet
    ),
    doi.org = .meta_doi_doi_org(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Return metadata for a PMID
#'
#' @description
#' Internal dispatcher for retrieving metadata for a PMID.
#'
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NULL` or empty.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_pmid_<provider>()`.
#'
#' @param x A single, normalized PMID string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the PMID.
#'
#' @noRd
.meta_pmid <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    
    out_ncbi <- .meta_pmid_ncbi(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.null(out_ncbi) && nrow(out_ncbi) > 0L) {
      return(out_ncbi)
    }
    
    out_epmc <- .meta_pmid_epmc(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.null(out_epmc) && nrow(out_epmc) > 0L) {
      return(out_epmc)
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(
        "Metadata for this PMID could not be retrieved via NCBI or Europe PMC."
      )
    }
    
    return(data.frame())
  }
  
  switch(
    provider,
    ncbi = .meta_pmid_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .meta_pmid_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Return metadata for a PMCID
#'
#' @description
#' Internal dispatcher for retrieving metadata for a PMCID.
#'
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NULL` or empty.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_pmcid_<provider>()`.
#'
#' @param x A single, normalized PMCID string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the PMCID.
#'
#' @noRd
.meta_pmcid <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    
    out_ncbi <- .meta_pmcid_ncbi(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.null(out_ncbi) && nrow(out_ncbi) > 0L) {
      return(out_ncbi)
    }
    
    out_epmc <- .meta_pmcid_epmc(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.null(out_epmc) && nrow(out_epmc) > 0L) {
      return(out_epmc)
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(
        "Metadata for this PMCID could not be retrieved via NCBI or Europe PMC."
      )
    }
    
    return(data.frame())
  }
  
  switch(
    provider,
    ncbi = .meta_pmcid_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .meta_pmcid_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Return metadata for an arXiv record
#'
#' @description
#' Internal dispatcher for retrieving metadata for an arXiv record.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_arxiv_<provider>()`.
#'
#' @param x A single, normalized arXiv identifier.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the arXiv record.
#'
#' @noRd
.meta_arxiv <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    provider <- "arxiv"
  }
  
  switch(
    provider,
    arxiv = .meta_arxiv_arxiv(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Return metadata for an ORCID record
#'
#' @description
#' Internal dispatcher for retrieving metadata for an ORCID record.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_orcid_<provider>()`.
#'
#' @param x A single, normalized ORCID identifier.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the ORCID record.
#'
#' @noRd
.meta_orcid <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    provider <- "orcid"
  }
  
  switch(
    provider,
    orcid = .meta_orcid_orcid(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}