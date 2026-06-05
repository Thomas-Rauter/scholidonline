# Shared request rate limiting for external providers


#' NCBI: rate-limit state environment
#'
#' @return An environment storing the timestamp of the last NCBI request.
#'
#' @noRd
.ncbi_rate_limit_env <- new.env(parent = emptyenv())
.ncbi_rate_limit_env$last_request <- NULL


#' Europe PMC: rate-limit state environment
#'
#' @return An environment storing the timestamp of the last Europe PMC request.
#'
#' @noRd
.epmc_rate_limit_env <- new.env(parent = emptyenv())
.epmc_rate_limit_env$last_request <- NULL


#' arXiv: internal state for request throttling
#'
#' @description
#' Internal environment used to store the timestamp of the most recent arXiv
#' request made through the package.
#'
#' @noRd
.scholidonline_arxiv_state <- new.env(parent = emptyenv())


#' Reset stored rate-limit state for a provider service
#'
#' @param service A single provider service name.
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.scholidonline_rate_limit_reset <- function(
    service = c("ncbi", "epmc", "arxiv")
) {
  service <- match.arg(service)
  state <- .scholidonline_rate_limit_state(service)
  state$env[[state$key]] <- NULL
  invisible(NULL)
}


#' NCBI: reset rate-limit state
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.ncbi_rate_limit_reset <- function() {
  .scholidonline_rate_limit_reset(service = "ncbi")
}


#' Europe PMC: reset rate-limit state
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.epmc_rate_limit_reset <- function() {
  .scholidonline_rate_limit_reset(service = "epmc")
}


#' arXiv: reset request throttling state
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.arxiv_rate_limit_reset <- function() {
  .scholidonline_rate_limit_reset(service = "arxiv")
}


#' Wait before making a provider request when needed
#'
#' @description
#' Internal helper enforcing a minimum interval between requests to external
#' provider APIs. Throttling is controlled globally by
#' `scholidonline.rate_limit` and per service by
#' `scholidonline.<service>.min_interval`.
#'
#' @param service A single provider service name.
#' @param quiet Logical; if `TRUE`, suppress informational messages where
#'   applicable.
#' @param min_interval Optional minimum interval override. When `NULL`, the
#'   service-specific option is used.
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.scholidonline_rate_limit <- function(
    service = c("ncbi", "epmc", "arxiv"),
    quiet = FALSE,
    min_interval = NULL
) {
  service <- match.arg(service)

  if (!isTRUE(getOption("scholidonline.rate_limit", TRUE))) {
    return(invisible(NULL))
  }

  cfg <- .scholidonline_rate_limit_service_config(service)

  if (is.null(min_interval)) {
    min_interval <- getOption(
      x = cfg$interval_option,
      default = cfg$default_interval
    )
  }

  min_interval <- .scholidonline_rate_limit_normalize_interval(
    min_interval = min_interval,
    service = service,
    default = cfg$default_interval
  )

  if (is.null(min_interval)) {
    return(invisible(NULL))
  }

  state <- .scholidonline_rate_limit_state(service)
  last_request <- state$env[[state$key]]
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

    if (is.finite(wait) && wait > 0) {
      if (.scholidonline_rate_limit_should_inform(
        service = service,
        quiet = quiet
      )) {
        rlang::inform(
          paste0(
            "Waiting ",
            round(wait, 2),
            " seconds before ",
            cfg$message_suffix
          )
        )
      }

      Sys.sleep(wait)
    }
  }

  state$env[[state$key]] <- Sys.time()

  invisible(NULL)
}


#' NCBI: wait before making a request when needed
#'
#' @param quiet Logical; if `TRUE`, suppress messages.
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.ncbi_rate_limit <- function(
    quiet = FALSE
) {
  .scholidonline_rate_limit(
    service = "ncbi",
    quiet = quiet
  )
}


#' Europe PMC: wait before making a request when needed
#'
#' @param quiet Logical; if `TRUE`, suppress messages.
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.epmc_rate_limit <- function(
    quiet = FALSE
) {
  .scholidonline_rate_limit(
    service = "epmc",
    quiet = quiet
  )
}


#' arXiv: throttle repeated requests
#'
#' @param min_interval A single numeric value giving the minimum number of
#'   seconds between arXiv requests.
#' @param quiet Logical.
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.arxiv_rate_limit <- function(
    min_interval = getOption("scholidonline.arxiv.min_interval", 3),
    quiet = FALSE
) {
  .scholidonline_rate_limit(
    service = "arxiv",
    quiet = quiet,
    min_interval = min_interval
  )
}


#' Return per-service rate-limit configuration
#'
#' @param service A single provider service name.
#'
#' @return A list with service-specific settings.
#'
#' @noRd
.scholidonline_rate_limit_service_config <- function(service) {
  switch(
    service,
    ncbi = list(
      interval_option = "scholidonline.ncbi.min_interval",
      default_interval = 0.34,
      message_suffix = "NCBI request."
    ),
    epmc = list(
      interval_option = "scholidonline.epmc.min_interval",
      default_interval = 1,
      message_suffix = "Europe PMC request."
    ),
    arxiv = list(
      interval_option = "scholidonline.arxiv.min_interval",
      default_interval = 3,
      message_suffix = "the next arXiv request."
    ),
    rlang::abort(paste0("Unknown rate-limit service: ", service, "."))
  )
}


#' Return per-service rate-limit state storage
#'
#' @param service A single provider service name.
#'
#' @return A list with `env` and `key`.
#'
#' @noRd
.scholidonline_rate_limit_state <- function(service) {
  switch(
    service,
    ncbi = list(
      env = .ncbi_rate_limit_env,
      key = "last_request"
    ),
    epmc = list(
      env = .epmc_rate_limit_env,
      key = "last_request"
    ),
    arxiv = list(
      env = .scholidonline_arxiv_state,
      key = "last_request_time"
    ),
    rlang::abort(paste0("Unknown rate-limit service: ", service, "."))
  )
}


#' Normalize a minimum interval for a provider service
#'
#' @param min_interval Candidate minimum interval.
#' @param service A single provider service name.
#' @param default Default interval for NCBI and Europe PMC fallbacks.
#'
#' @return A numeric interval, or `NULL` when throttling should be skipped.
#'
#' @noRd
.scholidonline_rate_limit_normalize_interval <- function(
    min_interval,
    service,
    default
) {
  if (identical(service, "arxiv")) {
    if (!is.numeric(min_interval) || length(min_interval) != 1L) {
      stop(
        "`min_interval` must be a single numeric value.",
        call. = FALSE
      )
    }

    if (is.na(min_interval) || min_interval <= 0) {
      return(NULL)
    }

    return(min_interval)
  }

  if (
    !is.numeric(min_interval) ||
    length(min_interval) != 1L ||
    is.na(min_interval) ||
    min_interval < 0
  ) {
    return(default)
  }

  min_interval
}


#' Decide whether to emit a rate-limit wait message
#'
#' @param service A single provider service name.
#' @param quiet Logical.
#'
#' @return Logical scalar.
#'
#' @noRd
.scholidonline_rate_limit_should_inform <- function(
    service,
    quiet
) {
  if (isTRUE(quiet)) {
    return(FALSE)
  }

  if (identical(service, "arxiv")) {
    return(isTRUE(getOption("scholidonline.rate_limit.verbose", FALSE)))
  }

  TRUE
}
