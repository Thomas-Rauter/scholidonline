#' Check whether an arXiv identifier exists
#'
#' @description
#' Internal dispatcher for arXiv existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_arxiv_<provider>()`.
#'
#' @param x A single, normalized arXiv identifier.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_arxiv <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  switch(
    provider,
    arxiv = .exists_arxiv_arxiv(
      x = x,
      ...,
      quiet = quiet
    ),
    stop(
      "Unknown provider: ",
      provider,
      call. = FALSE
    )
  )
}


#' Check whether a DOI exists
#'
#' @description
#' Internal dispatcher for DOI existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_doi_<provider>()`.
#'
#' @param x A single, normalized DOI string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_doi <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  switch(
    provider,
    "doi.org" = .exists_doi_doi_org(
      x = x,
      ...,
      quiet = quiet
    ),
    crossref = .exists_doi_crossref(
      x = x,
      ...,
      quiet = quiet
    ),
    stop(
      "Unknown provider: ",
      provider,
      call. = FALSE
    )
  )
}


#' Check whether an ORCID exists
#'
#' @description
#' Internal dispatcher for ORCID existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_orcid_<provider>()`.
#'
#' @param x A single, normalized ORCID string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_orcid <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  switch(
    provider,
    orcid = .exists_orcid_orcid(
      x = x,
      ...,
      quiet = quiet
    ),
    stop(
      "Unknown provider: ",
      provider,
      call. = FALSE
    )
  )
}


#' Check whether a PMCID exists
#'
#' @description
#' Internal dispatcher for PMCID existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_pmcid_<provider>()`.
#'
#' @param x A single, normalized PMCID string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_pmcid <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  switch(
    provider,
    ncbi = .exists_pmcid_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .exists_pmcid_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    stop(
      "Unknown provider: ",
      provider,
      call. = FALSE
    )
  )
}


#' Check whether a PMID exists
#'
#' @description
#' Internal dispatcher for PMID existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_pmid_<provider>()`.
#'
#' @param x A single, normalized PMID string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_pmid <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  switch(
    provider,
    ncbi = .exists_pmid_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .exists_pmid_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    stop(
      "Unknown provider: ",
      provider,
      call. = FALSE
    )
  )
}