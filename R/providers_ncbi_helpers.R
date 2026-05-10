#' NCBI: rate-limit state environment
#'
#' @return An environment storing the timestamp of the last NCBI request.
#'
#' @noRd
.ncbi_rate_limit_env <- new.env(parent = emptyenv())
.ncbi_rate_limit_env$last_request <- NULL


#' NCBI: reset rate-limit state
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.ncbi_rate_limit_reset <- function() {
  .ncbi_rate_limit_env$last_request <- NULL
  
  invisible(NULL)
}


#' NCBI: wait before making a request when needed
#'
#' @description
#' Internal helper enforcing a minimum interval between NCBI requests.
#'
#' The rate limiter is controlled by two options:
#' - `scholidonline.rate_limit`
#' - `scholidonline.ncbi.min_interval`
#'
#' If `scholidonline.rate_limit` is `FALSE`, this helper does nothing.
#'
#' @param quiet Logical; if `TRUE`, suppress messages.
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.ncbi_rate_limit <- function(
    quiet = FALSE
) {
  enabled <- getOption(
    x = "scholidonline.rate_limit",
    default = TRUE
  )
  
  if (!isTRUE(enabled)) {
    return(invisible(NULL))
  }
  
  min_interval <- getOption(
    x = "scholidonline.ncbi.min_interval",
    default = 0.34
  )
  
  if (
    !is.numeric(min_interval) ||
    length(min_interval) != 1L ||
    is.na(min_interval) ||
    min_interval < 0
  ) {
    min_interval <- 0.34
  }
  
  last_request <- .ncbi_rate_limit_env$last_request
  now <- Sys.time()
  
  if (!is.null(last_request)) {
    elapsed <- as.numeric(
      difftime(
        time1 = now,
        time2 = last_request,
        units = "secs"
      )
    )
    
    wait <- min_interval - elapsed
    
    if (wait > 0) {
      if (!isTRUE(quiet)) {
        rlang::inform(
          paste0(
            "Waiting ",
            round(wait, 2),
            " seconds before NCBI request."
          )
        )
      }
      
      Sys.sleep(wait)
    }
  }
  
  .ncbi_rate_limit_env$last_request <- Sys.time()
  
  invisible(NULL)
}