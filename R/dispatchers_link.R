# Level 1 function (functions called by exported functions) definitions --------


#' Return linked identifiers for an arXiv record
#'
#' @description
#' Internal dispatcher for retrieving identifiers linked to an arXiv record.
#'
#' Provider-specific implementations live in helpers named
#' `.links_arxiv_<provider>()`.
#'
#' @param x A single, normalized arXiv identifier.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing linked identifiers.
#'
#' @noRd
.links_arxiv <- function(
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
    arxiv = .links_arxiv_arxiv(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(
      paste0("Unknown provider: ", provider)
    )
  )
}



#' Return linked identifiers for a DOI
#'
#' @description
#' Internal dispatcher for retrieving identifiers linked to a DOI.
#'
#' Provider-specific implementations live in helpers named
#' `.links_doi_<provider>()`.
#'
#' If `provider = "auto"`, the dispatcher uses Crossref.
#'
#' @param x A single, normalized DOI string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing linked identifiers.
#'
#' @noRd
.links_doi <- function(
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
    crossref = .links_doi_crossref(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(
      paste0("Unknown provider: ", provider)
    )
  )
}



#' Return linked identifiers for an ORCID record
#'
#' @description
#' Internal dispatcher for retrieving identifiers linked to an ORCID record.
#'
#' Provider-specific implementations live in helpers named
#' `.links_orcid_<provider>()`.
#'
#' @param x A single, normalized ORCID string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing linked identifiers.
#'
#' @noRd
.links_orcid <- function(
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
    orcid = .links_orcid_orcid(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(
      paste0("Unknown provider: ", provider)
    )
  )
}



#' Return linked identifiers for a PMCID
#'
#' @description
#' Internal dispatcher for retrieving identifiers linked to a PMCID.
#'
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NULL` or empty.
#'
#' Provider-specific implementations live in helpers named
#' `.links_pmcid_<provider>()`.
#'
#' @param x A single, normalized PMCID string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing linked identifiers.
#'
#' @noRd
.links_pmcid <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    
    out_ncbi <- .links_pmcid_ncbi(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.null(out_ncbi) && nrow(out_ncbi) > 0L) {
      return(out_ncbi)
    }
    
    out_epmc <- .links_pmcid_epmc(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.null(out_epmc) && nrow(out_epmc) > 0L) {
      return(out_epmc)
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(paste0(
        "Linked identifiers for this PMCID could not be determined via ",
        "NCBI or Europe PMC."
      ))
    }
    
    return(data.frame())
  }
  
  switch(
    provider,
    ncbi = .links_pmcid_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .links_pmcid_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(
      paste0("Unknown provider: ", provider)
    )
  )
}



#' Return linked identifiers for a PMID
#'
#' @description
#' Internal dispatcher for retrieving identifiers linked to a PMID.
#'
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NULL` or empty.
#'
#' Provider-specific implementations live in helpers named
#' `.links_pmid_<provider>()`.
#'
#' @param x A single, normalized PMID string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing linked identifiers.
#'
#' @noRd
.links_pmid <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    
    out_ncbi <- .links_pmid_ncbi(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.null(out_ncbi) && nrow(out_ncbi) > 0L) {
      return(out_ncbi)
    }
    
    out_epmc <- .links_pmid_epmc(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.null(out_epmc) && nrow(out_epmc) > 0L) {
      return(out_epmc)
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(paste(
        "Linked identifiers for this PMID could not be determined",
        "via NCBI or Europe PMC."
      ))
    }
    
    return(data.frame())
  }
  
  switch(
    provider,
    ncbi = .links_pmid_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .links_pmid_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(
      paste0("Unknown provider: ", provider)
    )
  )
}