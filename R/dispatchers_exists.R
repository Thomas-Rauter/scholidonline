# Level 1 function (functions called by exported functions) definitions --------


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
  
  if (identical(provider, "auto")) {
    provider <- "arxiv"
  }
  
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


#' Check whether arXiv identifiers exist in batch
#'
#' @description
#' Internal batch dispatcher for checking whether arXiv identifiers exist.
#'
#' @param x A character vector of normalized arXiv identifiers.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A list with one logical scalar per input.
#'
#' @noRd
.exists_arxiv_batch <- function(
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
    arxiv = .exists_batch_as_list(
      .exists_arxiv_arxiv_batch(
        x = x,
        ...,
        quiet = quiet
      )
    ),
    NULL
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
#' If `provider = "auto"`, the dispatcher uses DOI.org.
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
  
  if (identical(provider, "auto")) {
    provider <- "doi.org"
  }
  
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


#' Check whether a genome assembly accession exists
#'
#' @description
#' Internal dispatcher for assembly existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_assembly_<provider>()`.
#'
#' @param x A single, normalized assembly accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_assembly <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }

  switch(
    provider,
    ncbi = .exists_assembly_ncbi(
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


#' Check whether a BioProject accession exists
#'
#' @description
#' Internal dispatcher for BioProject existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_bioproject_<provider>()`.
#'
#' @param x A single, normalized BioProject accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_bioproject <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }

  switch(
    provider,
    ncbi = .exists_bioproject_ncbi(
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


#' Check whether a GEO accession exists
#'
#' @description
#' Internal dispatcher for GEO existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_geo_<provider>()`.
#'
#' @param x A single, normalized GEO accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_geo <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }

  switch(
    provider,
    ncbi = .exists_geo_ncbi(
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
  
  if (identical(provider, "auto")) {
    provider <- "orcid"
  }
  
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


#' Check whether an OpenAlex entity exists
#'
#' @description
#' Internal dispatcher for OpenAlex existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_openalex_<provider>()`.
#'
#' @param x A single, normalized OpenAlex key string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_openalex <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (identical(provider, "auto")) {
    provider <- "openalex"
  }

  switch(
    provider,
    openalex = .exists_openalex_openalex(
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


#' Check whether a RefSeq accession exists
#'
#' @description
#' Internal dispatcher for RefSeq existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_refseq_<provider>()`.
#'
#' @param x A single, normalized RefSeq accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_refseq <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }

  switch(
    provider,
    ncbi = .exists_refseq_ncbi(
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


#' Check whether a ROR iD exists
#'
#' @description
#' Internal dispatcher for ROR existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_ror_<provider>()`.
#'
#' @param x A single, normalized ROR iD string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_ror <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (identical(provider, "auto")) {
    provider <- "ror"
  }

  switch(
    provider,
    ror = .exists_ror_ror(
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


#' Check whether an SRA accession exists
#'
#' @description
#' Internal dispatcher for SRA existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_sra_<provider>()`.
#'
#' @param x A single, normalized SRA accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_sra <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (identical(provider, "auto")) {
    provider <- "ncbi"
  }

  switch(
    provider,
    ncbi = .exists_sra_ncbi(
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


#' Check whether a UniProt accession exists
#'
#' @description
#' Internal dispatcher for UniProt existence checks.
#'
#' Provider-specific implementations live in helpers named
#' `.exists_uniprot_<provider>()`.
#'
#' @param x A single, normalized UniProt accession string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_uniprot <- function(
    x,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x = x)

  if (identical(provider, "auto")) {
    provider <- "uniprot"
  }

  switch(
    provider,
    uniprot = .exists_uniprot_uniprot(
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
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NA`.
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
  
  if (identical(provider, "auto")) {
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .exists_pmid_ncbi,
      epmc_fn = .exists_pmid_epmc,
      is_success = function(out) !is.na(out),
      empty_value = NA,
      warn_message = paste(
        "PMID existence could not be determined via NCBI or Europe PMC."
      ),
      warn_style = "base",
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .exists_pmid_ncbi,
    epmc_fn = .exists_pmid_epmc,
    on_unknown = function(p) {
      stop(
        "Unknown provider: ",
        p,
        call. = FALSE
      )
    },
    quiet = quiet,
    ...
  )
}


#' Check whether PMIDs exist in batch
#'
#' @description
#' Internal batch dispatcher for checking whether PMIDs exist.
#'
#' @param x A character vector of normalized PMID strings.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A list with one logical scalar per input.
#'
#' @noRd
.exists_pmid_batch <- function(
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
    ncbi = .exists_batch_as_list(
      .exists_pmid_ncbi_batch(
        x = x,
        ...,
        quiet = quiet
      )
    ),
    NULL
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
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NA`.
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
  
  if (identical(provider, "auto")) {
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .exists_pmcid_ncbi,
      epmc_fn = .exists_pmcid_epmc,
      is_success = function(out) !is.na(out),
      empty_value = NA,
      warn_message = paste(
        "PMCID existence could not be determined via NCBI or Europe PMC."
      ),
      warn_style = "base",
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .exists_pmcid_ncbi,
    epmc_fn = .exists_pmcid_epmc,
    on_unknown = function(p) {
      stop(
        "Unknown provider: ",
        p,
        call. = FALSE
      )
    },
    quiet = quiet,
    ...
  )
}


#' Check whether PMCIDs exist in batch
#'
#' @description
#' Internal batch dispatcher for checking whether PMCIDs exist.
#'
#' @param x A character vector of normalized PMCID strings.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A list with one logical scalar per input.
#'
#' @noRd
.exists_pmcid_batch <- function(
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
    ncbi = .exists_batch_as_list(
      .exists_pmcid_ncbi_batch(
        x = x,
        ...,
        quiet = quiet
      )
    ),
    NULL
  )
}


# Level 2 functions (functions called by level 1 functions)  -------------------


#' Convert existence batch output to unary-engine list output
#'
#' @param x A logical vector.
#'
#' @return A list with one logical scalar per input.
#'
#' @noRd
.exists_batch_as_list <- function(x) {
  if (!is.logical(x)) {
    rlang::abort("Existence batch output must be logical.")
  }
  
  as.list(x)
}