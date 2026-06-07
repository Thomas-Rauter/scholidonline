# Level 2 function (functions called by level 1 functions) ---------------------


#' Convert an OpenAlex work to a DOI
#'
#' @description
#' Internal dispatcher for converting an OpenAlex work to a DOI.
#'
#' Provider-specific implementations live in helpers named
#' `.convert_openalex_to_doi_<provider>()`.
#'
#' @param x A single, normalized OpenAlex work key string.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single DOI string, or `NA_character_`.
#'
#' @noRd
.convert_openalex_to_doi <- function(
    x,
    from,
    to,
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
    openalex = .convert_openalex_to_doi_openalex(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Convert an OpenAlex work to a PMID
#'
#' @description
#' Internal dispatcher for converting an OpenAlex work to a PMID.
#'
#' Provider-specific implementations live in helpers named
#' `.convert_openalex_to_pmid_<provider>()`.
#'
#' @param x A single, normalized OpenAlex work key string.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single PMID string, or `NA_character_`.
#'
#' @noRd
.convert_openalex_to_pmid <- function(
    x,
    from,
    to,
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
    openalex = .convert_openalex_to_pmid_openalex(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
  )
}


#' Convert a PMID to a DOI
#'
#' @description
#' Internal dispatcher for converting a PMID to a DOI.
#'
#' Provider-specific implementations live in helpers named
#' `.convert_pmid_to_doi_<provider>()`.
#'
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NA_character_`.
#'
#' @param x A single, normalized PMID string.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single DOI string, or `NA_character_`.
#'
#' @noRd
.convert_pmid_to_doi <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .convert_pmid_to_doi_ncbi,
      epmc_fn = .convert_pmid_to_doi_epmc,
      is_success = function(out) !is.na(out),
      empty_value = NA_character_,
      warn_message = paste(
        "DOI for this PMID could not be determined via NCBI or Europe PMC."
      ),
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .convert_pmid_to_doi_ncbi,
    epmc_fn = .convert_pmid_to_doi_epmc,
    on_unknown = function(p) {
      rlang::abort(paste0("Unknown provider: ", p))
    },
    quiet = quiet,
    ...
  )
}


#' Convert PMIDs to DOIs in batch
#'
#' @description
#' Internal batch dispatcher for converting PMIDs to DOIs.
#'
#' @param x A character vector of normalized PMID strings.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A character vector with one value per input.
#'
#' @noRd
.convert_pmid_to_doi_batch <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  if (identical(provider, "auto")) {
    return(.dispatch_convert_ncbi_epmc_auto_batch(
      x = x,
      ncbi_batch_fn = .convert_pmid_to_doi_ncbi_batch,
      epmc_fn = .convert_pmid_to_doi_epmc,
      ...
    ))
  }
  
  switch(
    provider,
    ncbi = .convert_pmid_to_doi_ncbi_batch(
      x = x,
      ...,
      quiet = quiet
    ),
    NULL
  )
}


#' Convert a DOI to a PMID
#'
#' @description
#' Internal dispatcher for converting a DOI to a PMID.
#'
#' Provider-specific implementations live in helpers named
#' `.convert_doi_to_pmid_<provider>()`.
#'
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NA_character_`.
#'
#' @param x A single, normalized DOI string.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single PMID string, or `NA_character_`.
#'
#' @noRd
.convert_doi_to_pmid <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .convert_doi_to_pmid_ncbi,
      epmc_fn = .convert_doi_to_pmid_epmc,
      is_success = function(out) !is.na(out),
      empty_value = NA_character_,
      warn_message = paste(
        "PMID for this DOI could not be determined via NCBI or Europe PMC."
      ),
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .convert_doi_to_pmid_ncbi,
    epmc_fn = .convert_doi_to_pmid_epmc,
    on_unknown = function(p) {
      rlang::abort(paste0("Unknown provider: ", p))
    },
    quiet = quiet,
    ...
  )
}


#' Convert DOIs to PMIDs in batch
#'
#' @description
#' Internal batch dispatcher for converting DOIs to PMIDs.
#'
#' @param x A character vector of normalized DOI strings.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A character vector with one value per input.
#'
#' @noRd
.convert_doi_to_pmid_batch <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  if (identical(provider, "auto")) {
    return(.dispatch_convert_ncbi_epmc_auto_batch(
      x = x,
      ncbi_batch_fn = .convert_doi_to_pmid_ncbi_batch,
      epmc_fn = .convert_doi_to_pmid_epmc,
      ...
    ))
  }
  
  switch(
    provider,
    ncbi = .convert_doi_to_pmid_ncbi_batch(
      x = x,
      ...,
      quiet = quiet
    ),
    NULL
  )
}


#' Convert a PMCID to a PMID
#'
#' @description
#' Internal dispatcher for converting a PMCID to a PMID.
#'
#' Provider-specific implementations live in helpers named
#' `.convert_pmcid_to_pmid_<provider>()`.
#'
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NA_character_`.
#'
#' @param x A single, normalized PMCID string.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single PMID string, or `NA_character_`.
#'
#' @noRd
.convert_pmcid_to_pmid <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .convert_pmcid_to_pmid_ncbi,
      epmc_fn = .convert_pmcid_to_pmid_epmc,
      is_success = function(out) !is.na(out),
      empty_value = NA_character_,
      warn_message = paste(
        "PMID for this PMCID could not be determined via NCBI or Europe PMC."
      ),
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .convert_pmcid_to_pmid_ncbi,
    epmc_fn = .convert_pmcid_to_pmid_epmc,
    on_unknown = function(p) {
      rlang::abort(paste0("Unknown provider: ", p))
    },
    quiet = quiet,
    ...
  )
}


#' Convert a PMCID to a DOI
#'
#' @description
#' Internal dispatcher for converting a PMCID to a DOI.
#'
#' Provider-specific implementations live in helpers named
#' `.convert_pmcid_to_doi_<provider>()`.
#'
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NA_character_`.
#'
#' @param x A single, normalized PMCID string.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single DOI string, or `NA_character_`.
#'
#' @noRd
.convert_pmcid_to_doi <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .convert_pmcid_to_doi_ncbi,
      epmc_fn = .convert_pmcid_to_doi_epmc,
      is_success = function(out) !is.na(out),
      empty_value = NA_character_,
      warn_message = paste(
        "DOI for this PMCID could not be determined via NCBI or Europe PMC."
      ),
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .convert_pmcid_to_doi_ncbi,
    epmc_fn = .convert_pmcid_to_doi_epmc,
    on_unknown = function(p) {
      rlang::abort(paste0("Unknown provider: ", p))
    },
    quiet = quiet,
    ...
  )
}


#' Convert a PMID to a PMCID
#'
#' @description
#' Internal dispatcher for converting a PMID to a PMCID.
#'
#' Provider-specific implementations live in helpers named
#' `.convert_pmid_to_pmcid_<provider>()`.
#'
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NA_character_`.
#'
#' @param x A single, normalized PMID string.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single PMCID string, or `NA_character_`.
#'
#' @noRd
.convert_pmid_to_pmcid <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .convert_pmid_to_pmcid_ncbi,
      epmc_fn = .convert_pmid_to_pmcid_epmc,
      is_success = function(out) !is.na(out),
      empty_value = NA_character_,
      warn_message = paste(
        "PMCID for this PMID could not be determined via NCBI or Europe PMC."
      ),
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .convert_pmid_to_pmcid_ncbi,
    epmc_fn = .convert_pmid_to_pmcid_epmc,
    on_unknown = function(p) {
      rlang::abort(paste0("Unknown provider: ", p))
    },
    quiet = quiet,
    ...
  )
}


#' Convert a DOI to a PMCID
#'
#' @description
#' Internal dispatcher for converting a DOI to a PMCID.
#'
#' Provider-specific implementations live in helpers named
#' `.convert_doi_to_pmcid_<provider>()`.
#'
#' If `provider = "auto"`, the dispatcher first tries NCBI and falls back to
#' Europe PMC if the NCBI result is `NA_character_`.
#'
#' @param x A single, normalized DOI string.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single PMCID string, or `NA_character_`.
#'
#' @noRd
.convert_doi_to_pmcid <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  .scholidonline_check_scalar_chr(x)
  
  if (identical(provider, "auto")) {
    return(.dispatch_ncbi_epmc_auto(
      x = x,
      ncbi_fn = .convert_doi_to_pmcid_ncbi,
      epmc_fn = .convert_doi_to_pmcid_epmc,
      is_success = function(out) !is.na(out),
      empty_value = NA_character_,
      warn_message = paste(
        "PMCID for this DOI could not be determined via NCBI or Europe PMC."
      ),
      quiet = quiet,
      ...
    ))
  }

  .dispatch_ncbi_epmc_provider(
    x = x,
    provider = provider,
    ncbi_fn = .convert_doi_to_pmcid_ncbi,
    epmc_fn = .convert_doi_to_pmcid_epmc,
    on_unknown = function(p) {
      rlang::abort(paste0("Unknown provider: ", p))
    },
    quiet = quiet,
    ...
  )
}


#' Convert PMIDs to PMCIDs in batch
#'
#' @description
#' Internal batch dispatcher for converting PMIDs to PMCIDs.
#'
#' @param x A character vector of normalized PMID strings.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A character vector with one value per input.
#'
#' @noRd
.convert_pmid_to_pmcid_batch <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  if (identical(provider, "auto")) {
    return(.dispatch_convert_ncbi_epmc_auto_batch(
      x = x,
      ncbi_batch_fn = .convert_pmid_to_pmcid_ncbi_batch,
      epmc_fn = .convert_pmid_to_pmcid_epmc,
      ...
    ))
  }
  
  switch(
    provider,
    ncbi = .convert_pmid_to_pmcid_ncbi_batch(
      x = x,
      ...,
      quiet = quiet
    ),
    NULL
  )
}


#' Convert PMCIDs to PMIDs in batch
#'
#' @description
#' Internal batch dispatcher for converting PMCIDs to PMIDs.
#'
#' @param x A character vector of normalized PMCID strings.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A character vector with one value per input.
#'
#' @noRd
.convert_pmcid_to_pmid_batch <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  if (identical(provider, "auto")) {
    return(.dispatch_convert_ncbi_epmc_auto_batch(
      x = x,
      ncbi_batch_fn = .convert_pmcid_to_pmid_ncbi_batch,
      epmc_fn = .convert_pmcid_to_pmid_epmc,
      ...
    ))
  }
  
  switch(
    provider,
    ncbi = .convert_pmcid_to_pmid_ncbi_batch(
      x = x,
      ...,
      quiet = quiet
    ),
    NULL
  )
}


#' Convert PMCIDs to DOIs in batch
#'
#' @description
#' Internal batch dispatcher for converting PMCIDs to DOIs.
#'
#' @param x A character vector of normalized PMCID strings.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A character vector with one value per input.
#'
#' @noRd
.convert_pmcid_to_doi_batch <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  if (identical(provider, "auto")) {
    return(.dispatch_convert_ncbi_epmc_auto_batch(
      x = x,
      ncbi_batch_fn = .convert_pmcid_to_doi_ncbi_batch,
      epmc_fn = .convert_pmcid_to_doi_epmc,
      ...
    ))
  }
  
  switch(
    provider,
    ncbi = .convert_pmcid_to_doi_ncbi_batch(
      x = x,
      ...,
      quiet = quiet
    ),
    NULL
  )
}


#' Convert DOIs to PMCIDs in batch
#'
#' @description
#' Internal batch dispatcher for converting DOIs to PMCIDs.
#'
#' @param x A character vector of normalized DOI strings.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single provider string.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A character vector with one value per input.
#'
#' @noRd
.convert_doi_to_pmcid_batch <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet = FALSE
) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  
  if (identical(provider, "auto")) {
    return(.dispatch_convert_ncbi_epmc_auto_batch(
      x = x,
      ncbi_batch_fn = .convert_doi_to_pmcid_ncbi_batch,
      epmc_fn = .convert_doi_to_pmcid_epmc,
      ...
    ))
  }
  
  switch(
    provider,
    ncbi = .convert_doi_to_pmcid_ncbi_batch(
      x = x,
      ...,
      quiet = quiet
    ),
    NULL
  )
}