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


#' Return metadata for a genome assembly accession
#'
#' @description
#' Internal dispatcher for retrieving metadata for a genome assembly accession.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_assembly_<provider>()`.
#'
#' @param x A single, normalized assembly accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the assembly accession.
#'
#' @noRd
.meta_assembly <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)

  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }

  switch(
    provider,
    ncbi = .meta_assembly_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Return metadata for a BioProject accession
#'
#' @description
#' Internal dispatcher for retrieving metadata for a BioProject accession.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_bioproject_<provider>()`.
#'
#' @param x A single, normalized BioProject accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the BioProject accession.
#'
#' @noRd
.meta_bioproject <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)

  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }

  switch(
    provider,
    ncbi = .meta_bioproject_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Return metadata for a GEO accession
#'
#' @description
#' Internal dispatcher for retrieving metadata for a GEO accession.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_geo_<provider>()`.
#'
#' @param x A single, normalized GEO accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the GEO accession.
#'
#' @noRd
.meta_geo <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)

  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }

  switch(
    provider,
    ncbi = .meta_geo_ncbi(
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
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .meta_pmid_ncbi,
      epmc_fn = .meta_pmid_epmc,
      is_success = .dispatch_ncbi_epmc_df_success,
      empty_value = data.frame(),
      warn_message = paste(
        "Metadata for this PMID could not be retrieved via NCBI or Europe PMC."
      ),
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .meta_pmid_ncbi,
    epmc_fn = .meta_pmid_epmc,
    on_unknown = function(p) {
      rlang::abort(paste0("Unknown provider: ", p))
    },
    quiet = quiet,
    ...
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
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .meta_pmcid_ncbi,
      epmc_fn = .meta_pmcid_epmc,
      is_success = .dispatch_ncbi_epmc_df_success,
      empty_value = data.frame(),
      warn_message = paste(
        "Metadata for this PMCID could not be retrieved via NCBI or Europe PMC."
      ),
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .meta_pmcid_ncbi,
    epmc_fn = .meta_pmcid_epmc,
    on_unknown = function(p) {
      rlang::abort(paste0("Unknown provider: ", p))
    },
    quiet = quiet,
    ...
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


#' Return metadata for an OpenAlex work
#'
#' @description
#' Internal dispatcher for retrieving metadata for an OpenAlex work.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_openalex_<provider>()`.
#'
#' @param x A single, normalized OpenAlex key string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the OpenAlex work.
#'
#' @noRd
.meta_openalex <- function(
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
    openalex = .meta_openalex_openalex(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Return metadata for a RefSeq accession
#'
#' @description
#' Internal dispatcher for retrieving metadata for a RefSeq accession.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_refseq_<provider>()`.
#'
#' @param x A single, normalized RefSeq accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the RefSeq accession.
#'
#' @noRd
.meta_refseq <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)

  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }

  switch(
    provider,
    ncbi = .meta_refseq_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Return metadata for a ROR organization
#'
#' @description
#' Internal dispatcher for retrieving metadata for a ROR organization.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_ror_<provider>()`.
#'
#' @param x A single, normalized ROR iD string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the ROR organization.
#'
#' @noRd
.meta_ror <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)

  if (identical(provider, "auto")) {
    provider <- "ror"
  }

  switch(
    provider,
    ror = .meta_ror_ror(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Return metadata for an SRA accession
#'
#' @description
#' Internal dispatcher for retrieving metadata for an SRA accession.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_sra_<provider>()`.
#'
#' @param x A single, normalized SRA accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the SRA accession.
#'
#' @noRd
.meta_sra <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)

  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }

  switch(
    provider,
    ncbi = .meta_sra_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Return metadata for a UniProt entry
#'
#' @description
#' Internal dispatcher for retrieving metadata for a UniProt accession.
#'
#' Provider-specific implementations live in helpers named
#' `.meta_uniprot_<provider>()`.
#'
#' @param x A single, normalized UniProt accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame describing metadata for the UniProt entry.
#'
#' @noRd
.meta_uniprot <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)

  if (identical(provider, "auto")) {
    provider <- "uniprot"
  }

  switch(
    provider,
    uniprot = .meta_uniprot_uniprot(
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