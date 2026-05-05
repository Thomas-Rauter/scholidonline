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


#' Retrieve metadata for PMIDs in batch
#'
#' @description
#' Internal batch dispatcher for retrieving metadata for PMIDs.
#'
#' @param x A character vector of normalized PMID strings.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A list with one data.frame per input.
#'
#' @noRd
.meta_pmid_batch <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }
  
  switch(
    provider,
    ncbi = {
      res <- .meta_pmid_ncbi_batch(
        x = x,
        ...,
        quiet = quiet
      )
      
      .meta_pmid_batch_split(
        x = x,
        res = res
      )
    },
    NULL
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


#' Retrieve metadata for PMCIDs in batch
#'
#' @description
#' Internal batch dispatcher for retrieving metadata for PMCIDs.
#'
#' @param x A character vector of normalized PMCID strings.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A list with one data.frame per input.
#'
#' @noRd
.meta_pmcid_batch <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }
  
  switch(
    provider,
    ncbi = {
      res <- .meta_pmcid_ncbi_batch(
        x = x,
        ...,
        quiet = quiet
      )
      
      .meta_pmcid_batch_split(
        x = x,
        res = res
      )
    },
    NULL
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


#' Retrieve metadata for arXiv identifiers in batch
#'
#' @description
#' Internal batch dispatcher for retrieving metadata for arXiv identifiers.
#'
#' @param x A character vector of normalized arXiv identifiers.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A list with one data.frame per input.
#'
#' @noRd
.meta_arxiv_batch <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  if (identical(provider, "auto")) {
    provider <- "arxiv"
  }
  
  switch(
    provider,
    arxiv = {
      res <- .meta_arxiv_arxiv_batch(
        x = x,
        ...,
        quiet = quiet
      )
      
      .meta_arxiv_batch_split(
        x = x,
        res = res
      )
    },
    NULL
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


# Level 1 functions (functions called by level 2 functions) --------------------


#' Split arXiv metadata batch output by query
#'
#' @param x A character vector of normalized arXiv identifiers.
#' @param res A data.frame returned by the arXiv metadata batch provider.
#'
#' @return A list with one data.frame per input element.
#'
#' @noRd
.meta_arxiv_batch_split <- function(
    x,
    res
) {
  out <- vector(
    mode = "list",
    length = length(x)
  )
  
  if (is.null(res) || nrow(res) == 0L) {
    for (i in seq_along(out)) {
      out[[i]] <- data.frame()
    }
    
    return(out)
  }
  
  res_key <- .arxiv_strip_version(res$arxiv_id)
  query_key <- .arxiv_strip_version(x)
  
  for (i in seq_along(x)) {
    hit <- match(query_key[[i]], res_key)
    
    if (is.na(hit)) {
      out[[i]] <- data.frame()
      next
    }
    
    out[[i]] <- res[
      hit,
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
  
  out
}


#' Split NCBI PMID metadata batch output by query
#'
#' @param x A character vector of normalized PMID strings.
#' @param res A data.frame returned by the NCBI PMID metadata batch provider.
#'
#' @return A list with one data.frame per input element.
#'
#' @noRd
.meta_pmid_batch_split <- function(
    x,
    res
) {
  out <- vector(
    mode = "list",
    length = length(x)
  )
  
  if (is.null(res) || nrow(res) == 0L) {
    for (i in seq_along(out)) {
      out[[i]] <- data.frame()
    }
    
    return(out)
  }
  
  res_key <- res$pmid_key
  
  for (i in seq_along(x)) {
    hit <- match(x[[i]], res_key)
    
    if (is.na(hit)) {
      out[[i]] <- data.frame()
      next
    }
    
    out[[i]] <- res[
      hit,
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
  
  out
}


#' Split NCBI PMCID metadata batch output by query
#'
#' @param x A character vector of normalized PMCID strings.
#' @param res A data.frame returned by the NCBI PMCID metadata batch provider.
#'
#' @return A list with one data.frame per input element.
#'
#' @noRd
.meta_pmcid_batch_split <- function(
    x,
    res
) {
  out <- vector(
    mode = "list",
    length = length(x)
  )
  
  if (is.null(res) || nrow(res) == 0L) {
    for (i in seq_along(out)) {
      out[[i]] <- data.frame()
    }
    
    return(out)
  }
  
  res_key <- res$pmcid_key
  
  for (i in seq_along(x)) {
    hit <- match(x[[i]], res_key)
    
    if (is.na(hit)) {
      out[[i]] <- data.frame()
      next
    }
    
    out[[i]] <- res[
      hit,
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
  
  out
}