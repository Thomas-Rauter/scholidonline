# Level 2 function (functions called by level 1 functions) ---------------------


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
    out_ncbi <- .convert_pmid_to_doi_ncbi(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_ncbi)) {
      return(out_ncbi)
    }
    
    out_epmc <- .convert_pmid_to_doi_epmc(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_epmc)) {
      return(out_epmc)
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(
        "DOI for this PMID could not be determined via NCBI or Europe PMC."
      )
    }
    
    return(NA_character_)
  }
  
  switch(
    provider,
    ncbi = .convert_pmid_to_doi_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .convert_pmid_to_doi_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
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
    out <- .convert_pmid_to_doi_ncbi_batch(
      x = x,
      ...,
      quiet = TRUE
    )
    
    missing <- is.na(out)
    
    if (any(missing)) {
      out[missing] <- vapply(
        x[missing],
        .convert_pmid_to_doi_epmc,
        character(1),
        ...,
        quiet = TRUE
      )
    }
    
    return(out)
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
    out_ncbi <- .convert_doi_to_pmid_ncbi(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_ncbi)) {
      return(out_ncbi)
    }
    
    out_epmc <- .convert_doi_to_pmid_epmc(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_epmc)) {
      return(out_epmc)
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(
        "PMID for this DOI could not be determined via NCBI or Europe PMC."
      )
    }
    
    return(NA_character_)
  }
  
  switch(
    provider,
    ncbi = .convert_doi_to_pmid_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .convert_doi_to_pmid_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
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
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = x,
      ...,
      quiet = TRUE
    )
    
    missing <- is.na(out)
    
    if (any(missing)) {
      out[missing] <- vapply(
        x[missing],
        .convert_doi_to_pmid_epmc,
        character(1),
        ...,
        quiet = TRUE
      )
    }
    
    return(out)
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
    out_ncbi <- .convert_pmcid_to_pmid_ncbi(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_ncbi)) {
      return(out_ncbi)
    }
    
    out_epmc <- .convert_pmcid_to_pmid_epmc(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_epmc)) {
      return(out_epmc)
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(
        "PMID for this PMCID could not be determined via NCBI or Europe PMC."
      )
    }
    
    return(NA_character_)
  }
  
  switch(
    provider,
    ncbi = .convert_pmcid_to_pmid_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .convert_pmcid_to_pmid_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
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
    out_ncbi <- .convert_pmcid_to_doi_ncbi(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_ncbi)) {
      return(out_ncbi)
    }
    
    out_epmc <- .convert_pmcid_to_doi_epmc(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_epmc)) {
      return(out_epmc)
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(
        "DOI for this PMCID could not be determined via NCBI or Europe PMC."
      )
    }
    
    return(NA_character_)
  }
  
  switch(
    provider,
    ncbi = .convert_pmcid_to_doi_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .convert_pmcid_to_doi_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
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
    out_ncbi <- .convert_pmid_to_pmcid_ncbi(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_ncbi)) {
      return(out_ncbi)
    }
    
    out_epmc <- .convert_pmid_to_pmcid_epmc(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_epmc)) {
      return(out_epmc)
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(
        "PMCID for this PMID could not be determined via NCBI or Europe PMC."
      )
    }
    
    return(NA_character_)
  }
  
  switch(
    provider,
    ncbi = .convert_pmid_to_pmcid_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .convert_pmid_to_pmcid_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
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
    out_ncbi <- .convert_doi_to_pmcid_ncbi(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_ncbi)) {
      return(out_ncbi)
    }
    
    out_epmc <- .convert_doi_to_pmcid_epmc(
      x = x,
      ...,
      quiet = TRUE
    )
    
    if (!is.na(out_epmc)) {
      return(out_epmc)
    }
    
    if (!isTRUE(quiet)) {
      rlang::warn(
        "PMCID for this DOI could not be determined via NCBI or Europe PMC."
      )
    }
    
    return(NA_character_)
  }
  
  switch(
    provider,
    ncbi = .convert_doi_to_pmcid_ncbi(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = .convert_doi_to_pmcid_epmc(
      x = x,
      ...,
      quiet = quiet
    ),
    rlang::abort(paste0("Unknown provider: ", provider))
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
    out <- .convert_pmid_to_pmcid_ncbi_batch(
      x = x,
      ...,
      quiet = TRUE
    )
    
    missing <- is.na(out)
    
    if (any(missing)) {
      out[missing] <- vapply(
        x[missing],
        .convert_pmid_to_pmcid_epmc,
        character(1),
        ...,
        quiet = TRUE
      )
    }
    
    return(out)
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
    out <- .convert_pmcid_to_pmid_ncbi_batch(
      x = x,
      ...,
      quiet = TRUE
    )
    
    missing <- is.na(out)
    
    if (any(missing)) {
      out[missing] <- vapply(
        x[missing],
        .convert_pmcid_to_pmid_epmc,
        character(1),
        ...,
        quiet = TRUE
      )
    }
    
    return(out)
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
    out <- .convert_pmcid_to_doi_ncbi_batch(
      x = x,
      ...,
      quiet = TRUE
    )
    
    missing <- is.na(out)
    
    if (any(missing)) {
      out[missing] <- vapply(
        x[missing],
        .convert_pmcid_to_doi_epmc,
        character(1),
        ...,
        quiet = TRUE
      )
    }
    
    return(out)
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
    out <- .convert_doi_to_pmcid_ncbi_batch(
      x = x,
      ...,
      quiet = TRUE
    )
    
    missing <- is.na(out)
    
    if (any(missing)) {
      out[missing] <- vapply(
        x[missing],
        .convert_doi_to_pmcid_epmc,
        character(1),
        ...,
        quiet = TRUE
      )
    }
    
    return(out)
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