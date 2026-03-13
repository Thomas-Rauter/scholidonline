#' Europe PMC: check whether a PMID exists
#'
#' @param x A single, normalized PMID string.
#' @param ... Passed to Europe PMC search.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_pmid_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  js <- .scholidonline_epmc_search(
    query = paste0(
      "EXT_ID:",
      x,
      " AND SRC:MED"
    ),
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA)
  }
  
  hit_count <- suppressWarnings(
    as.integer(
      js$hitCount %||% NA_character_
    )
  )
  
  if (is.na(hit_count)) {
    return(NA)
  }
  
  hit_count > 0L
}


#' Europe PMC: check whether a PMCID exists
#'
#' @param x A single, normalized PMCID string.
#' @param ... Passed to Europe PMC search.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_pmcid_epmc <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  js <- .scholidonline_epmc_search(
    query = paste0(
      "PMCID:",
      x
    ),
    ...,
    quiet = quiet
  )
  
  if (is.null(js)) {
    return(NA)
  }
  
  hit_count <- suppressWarnings(
    as.integer(
      js$hitCount %||% NA_character_
    )
  )
  
  if (is.na(hit_count)) {
    return(NA)
  }
  
  hit_count > 0L
}