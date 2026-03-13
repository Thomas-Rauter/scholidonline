#' NCBI: check whether a PMID exists
#'
#' @param x A single, normalized PMID string.
#' @param ... Passed to NCBI E-utilities.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_pmid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  js <- .scholidonline_esummary_pubmed(
    id = x,
    ...,
    quiet = quiet
  )
  
  if (is.null(js) || is.null(js$result)) {
    return(NA)
  }
  
  if (!is.null(js$result[[x]])) {
    return(TRUE)
  }
  
  if (!is.null(js$result$uids) && !x %in% unlist(js$result$uids, use.names = FALSE)) {
    return(FALSE)
  }
  
  NA
}


#' NCBI: check whether a PMCID exists
#'
#' @param x A single, normalized PMCID string.
#' @param ... Passed to PMC ID Converter.
#' @param quiet Logical.
#'
#' @return A single logical value.
#'
#' @noRd
.exists_pmcid_ncbi <- function(
    x,
    ...,
    quiet = FALSE
) {
  
  .scholidonline_check_scalar_chr(
    x = x
  )
  
  js <- .scholidonline_pmc_idconv(
    ids = x,
    ...,
    quiet = quiet
  )
  
  if (is.null(js) || is.null(js$records) || length(js$records) < 1L) {
    return(NA)
  }
  
  rec <- js$records[[1L]]
  
  if (!is.null(rec$status) && identical(rec$status, "error")) {
    return(FALSE)
  }
  
  if (!is.null(rec$pmcid) && nzchar(rec$pmcid)) {
    return(TRUE)
  }
  
  FALSE
}