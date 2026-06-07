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


#' Return linked identifiers for arXiv records in batch
#'
#' @description
#' Internal batch dispatcher for retrieving identifiers linked to arXiv records.
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
.links_arxiv_batch <- function(
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
      res <- .links_arxiv_arxiv_batch(
        x = x,
        ...,
        quiet = quiet
      )
      
      .links_arxiv_batch_split(
        x = x,
        res = res
      )
    },
    NULL
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


#' Return linked identifiers for an OpenAlex work
#'
#' @description
#' Internal dispatcher for retrieving identifiers linked to an OpenAlex work.
#'
#' Provider-specific implementations live in helpers named
#' `.links_openalex_<provider>()`.
#'
#' @param x A single, normalized OpenAlex key string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing linked identifiers.
#'
#' @noRd
.links_openalex <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)

  if (identical(provider, "auto")) {
    provider <- "openalex"
  }

  switch(
    provider,
    openalex = .links_openalex_openalex(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
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
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .links_pmcid_ncbi,
      epmc_fn = .links_pmcid_epmc,
      is_success = .dispatch_ncbi_epmc_df_success,
      empty_value = data.frame(),
      warn_message = paste0(
        "Linked identifiers for this PMCID could not be determined via ",
        "NCBI or Europe PMC."
      ),
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .links_pmcid_ncbi,
    epmc_fn = .links_pmcid_epmc,
    on_unknown = function(p) {
      rlang::abort(paste0("Unknown provider: ", p))
    },
    quiet = quiet,
    ...
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
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .links_pmid_ncbi,
      epmc_fn = .links_pmid_epmc,
      is_success = .dispatch_ncbi_epmc_df_success,
      empty_value = data.frame(),
      warn_message = paste(
        "Linked identifiers for this PMID could not be determined",
        "via NCBI or Europe PMC."
      ),
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .links_pmid_ncbi,
    epmc_fn = .links_pmid_epmc,
    on_unknown = function(p) {
      rlang::abort(paste0("Unknown provider: ", p))
    },
    quiet = quiet,
    ...
  )
}


#' Return linked identifiers for PMIDs in batch
#'
#' @description
#' Internal batch dispatcher for retrieving identifiers linked to PMIDs.
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
.links_pmid_batch <- function(
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
      res <- .links_pmid_ncbi_batch(
        x = x,
        ...,
        quiet = quiet
      )
      
      .links_ncbi_batch_split(
        x = x,
        res = res
      )
    },
    NULL
  )
}


#' Return linked identifiers for PMCIDs in batch
#'
#' @description
#' Internal batch dispatcher for retrieving identifiers linked to PMCIDs.
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
.links_pmcid_batch <- function(
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
      res <- .links_pmcid_ncbi_batch(
        x = x,
        ...,
        quiet = quiet
      )
      
      .links_ncbi_batch_split(
        x = x,
        res = res
      )
    },
    NULL
  )
}


# Level 2 function (functions called by level 1 functions) ---------------------


#' Split arXiv linked-identifier batch output by query
#'
#' @param x A character vector of normalized arXiv identifiers.
#' @param res A data.frame returned by the arXiv links batch provider.
#'
#' @return A list with one data.frame per input element.
#'
#' @noRd
.links_arxiv_batch_split <- function(
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
    hits <- which(res_key == query_key[[i]])
    
    if (length(hits) < 1L) {
      out[[i]] <- data.frame()
      next
    }
    
    out[[i]] <- res[
      hits,
      c("linked_type", "linked_value", "provider"),
      drop = FALSE
    ]
  }
  
  out
}


#' Split NCBI linked-identifier batch output by query
#'
#' @param x A character vector of normalized query identifiers.
#' @param res A data.frame returned by an NCBI links batch provider.
#'
#' @return A list with one data.frame per input element.
#'
#' @noRd
.links_ncbi_batch_split <- function(
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
  
  for (i in seq_along(x)) {
    hits <- which(res$query_id == x[[i]])
    
    if (length(hits) < 1L) {
      out[[i]] <- data.frame()
      next
    }
    
    out[[i]] <- res[
      hits,
      c("linked_type", "linked_value", "provider"),
      drop = FALSE
    ]
  }
  
  out
}
