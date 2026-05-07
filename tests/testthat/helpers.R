#' Conditionally skip live internet tests
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
skip_if_no_internet_for_live_tests <- function() {
  testthat::skip_if_offline()
  
  if (!identical(Sys.getenv("RUN_LIVE_TESTS"), "true")) {
    testthat::skip(
      paste(
        "Skipping live internet tests.",
        "To enable them in this R session, run:",
        'Sys.setenv(RUN_LIVE_TESTS = "true")'
      )
    )
  } else{
    invisible(NULL)
  }
}


#' Conditionally skip when arXiv is temporarily unavailable
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
skip_if_arxiv_live_unavailable <- function(txt) {
  if (is.null(txt)) {
    testthat::skip(
      paste(
        "Skipping live arXiv test because arXiv returned no usable",
        "response, likely due to rate limiting or temporary downtime."
      )
    )
  }
  
  invisible(NULL)
}