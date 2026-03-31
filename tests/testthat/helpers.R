#' Conditionally skip live internet tests
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
skip_if_no_internet_for_live_tests <- function() {
  testthat::skip_if_offline()
  
  if (!identical(Sys.getenv("RUN_LIVE_TESTS"), "true")) {
    testthat::skip(
      "Skipping live internet tests; set RUN_LIVE_TESTS=true to enable."
    )
  } else{
    invisible(NULL)
  }
}