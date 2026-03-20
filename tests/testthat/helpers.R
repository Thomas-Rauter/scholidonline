#' Conditionally skip live internet tests
#'
#' Helper for testthat to control execution of tests that require live
#' internet access (e.g. NCBI API calls).
#'
#' Behavior:
#' \itemize{
#'   \item If the environment variable \code{RUN_LIVE_TESTS} is set to
#'     \code{"true"}, tests are always executed (no skipping is performed).
#'   \item Otherwise, tests are skipped when no internet connection is
#'     available via \code{testthat::skip_if_offline()}.
#' }
#'
#' This allows:
#' \itemize{
#'   \item Safe default behavior for CRAN and offline environments.
#'   \item Opt-in execution of live integration tests during development
#'     or coverage runs (e.g. \code{covr::report()}).
#' }
#'
#' To enable live tests in the current R session:
#' \preformatted{
#' Sys.setenv(RUN_LIVE_TESTS = "true")
#' }
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   skipping tests when appropriate.
#' @keywords internal
skip_if_no_internet_for_live_tests <- function() {
  if (identical(Sys.getenv("RUN_LIVE_TESTS"), "true")) {
    return(invisible())
  }
  
  testthat::skip_if_offline()
}