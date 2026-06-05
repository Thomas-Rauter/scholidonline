# Shared HTTP helpers for provider implementations


#' Perform a prepared HTTP request
#'
#' @param req An httr2 request object.
#' @param quiet Logical.
#' @param before_request Optional zero-argument function run before perform.
#'
#' @return An httr2 response object, or `NULL` on error.
#'
#' @noRd
.scholidonline_http_perform <- function(
    req,
    quiet = FALSE,
    before_request = NULL
) {
  if (is.function(before_request)) {
    before_request()
  }

  req <- .scholidonline_req_error(
    req = req,
    is_error = function(resp) FALSE
  )

  .scholidonline_req_perform_safe(req = req)
}


#' Perform an HTTP GET request
#'
#' @param url A single URL string.
#' @param headers Optional named character vector of request headers.
#' @param quiet Logical.
#' @param before_request Optional zero-argument function run before perform.
#'
#' @return An httr2 response object, or `NULL` on error.
#'
#' @noRd
.scholidonline_http_get <- function(
    url,
    headers = NULL,
    quiet = FALSE,
    before_request = NULL
) {
  req <- .scholidonline_request(url)

  if (!is.null(headers) && length(headers) > 0L) {
    req <- do.call(
      .scholidonline_req_headers,
      c(list(req = req), as.list(headers))
    )
  }

  .scholidonline_http_perform(
    req = req,
    quiet = quiet,
    before_request = before_request
  )
}


#' Warn that an HTTP request failed
#'
#' @param provider_label Provider label used in warning text.
#' @param quiet Logical.
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.scholidonline_http_warn_failed <- function(
    provider_label,
    quiet
) {
  if (!isTRUE(quiet)) {
    rlang::warn(
      paste0(provider_label, " request failed.")
    )
  }

  invisible(NULL)
}


#' Warn about a non-success HTTP status
#'
#' @param provider_label Provider label used in warning text.
#' @param status HTTP status code.
#' @param quiet Logical.
#'
#' @return Invisibly returns `NULL`.
#'
#' @noRd
.scholidonline_http_warn_status <- function(
    provider_label,
    status,
    quiet
) {
  if (!isTRUE(quiet)) {
    rlang::warn(
      paste0(
        provider_label,
        " request returned HTTP ",
        status,
        "."
      )
    )
  }

  invisible(NULL)
}


#' Interpret an HTTP response for existence checks
#'
#' @param resp An httr2 response object or `NULL`.
#' @param quiet Logical.
#' @param provider_label Provider label used in warning text.
#'
#' @return `TRUE`, `FALSE`, or `NA`.
#'
#' @noRd
.scholidonline_http_exists_from_response <- function(
    resp,
    quiet,
    provider_label
) {
  if (is.null(resp)) {
    .scholidonline_http_warn_failed(
      provider_label = provider_label,
      quiet = quiet
    )
    return(NA)
  }

  status <- .scholidonline_resp_status(resp = resp)

  if (status >= 200L && status < 300L) {
    return(TRUE)
  }

  if (status == 404L) {
    return(FALSE)
  }

  .scholidonline_http_warn_status(
    provider_label = provider_label,
    status = status,
    quiet = quiet
  )

  NA
}


#' Parse JSON from an HTTP response
#'
#' @param resp An httr2 response object or `NULL`.
#' @param quiet Logical.
#' @param provider_label Provider label used in warning text.
#' @param silent_404 If `TRUE`, HTTP 404 returns `NULL` without warning.
#' @param simplifyVector Passed to `httr2::resp_body_json()`.
#'
#' @return Parsed JSON, or `NULL`.
#'
#' @noRd
.scholidonline_http_json_from_response <- function(
    resp,
    quiet,
    provider_label,
    silent_404 = FALSE,
    simplifyVector = FALSE
) {
  if (is.null(resp)) {
    .scholidonline_http_warn_failed(
      provider_label = provider_label,
      quiet = quiet
    )
    return(NULL)
  }

  status <- .scholidonline_resp_status(resp = resp)

  if (silent_404 && status == 404L) {
    return(NULL)
  }

  if (status < 200L || status >= 300L) {
    .scholidonline_http_warn_status(
      provider_label = provider_label,
      status = status,
      quiet = quiet
    )
    return(NULL)
  }

  args <- list(resp = resp)

  if (!identical(simplifyVector, FALSE)) {
    args$simplifyVector <- simplifyVector
  }

  tryCatch(
    do.call(.scholidonline_resp_body_json, args),
    error = function(e) NULL
  )
}


#' Parse a string body from an HTTP response
#'
#' @param resp An httr2 response object or `NULL`.
#' @param quiet Logical.
#' @param provider_label Provider label used in warning text.
#' @param require_success If `TRUE`, non-2xx responses return `NULL`.
#'
#' @return Response body string, or `NULL`.
#'
#' @noRd
.scholidonline_http_string_from_response <- function(
    resp,
    quiet,
    provider_label,
    require_success = TRUE
) {
  if (is.null(resp)) {
    .scholidonline_http_warn_failed(
      provider_label = provider_label,
      quiet = quiet
    )
    return(NULL)
  }

  status <- .scholidonline_resp_status(resp = resp)

  if (require_success && (status < 200L || status >= 300L)) {
    .scholidonline_http_warn_status(
      provider_label = provider_label,
      status = status,
      quiet = quiet
    )
    return(NULL)
  }

  tryCatch(
    .scholidonline_resp_body_string(resp = resp),
    error = function(e) NULL
  )
}


#' Perform an HTTP GET request and parse JSON
#'
#' @param url A single URL string.
#' @param quiet Logical.
#' @param provider_label Provider label used in warning text.
#' @param headers Optional named character vector of request headers.
#' @param before_request Optional zero-argument function run before perform.
#' @param silent_404 If `TRUE`, HTTP 404 returns `NULL` without warning.
#' @param simplifyVector Passed to `httr2::resp_body_json()`.
#'
#' @return Parsed JSON, or `NULL`.
#'
#' @noRd
.scholidonline_http_get_json <- function(
    url,
    quiet,
    provider_label,
    headers = NULL,
    before_request = NULL,
    silent_404 = FALSE,
    simplifyVector = FALSE
) {
  resp <- .scholidonline_http_get(
    url = url,
    headers = headers,
    quiet = quiet,
    before_request = before_request
  )

  .scholidonline_http_json_from_response(
    resp = resp,
    quiet = quiet,
    provider_label = provider_label,
    silent_404 = silent_404,
    simplifyVector = simplifyVector
  )
}


#' Perform an HTTP request and parse JSON with explicit failure warnings
#'
#' @param req An httr2 request object. Supply `req` or `url`.
#' @param url A single URL string. Supply `req` or `url`.
#' @param quiet Logical.
#' @param provider_label Provider label used in warning text.
#' @param before_request Optional zero-argument function run before perform.
#' @param simplifyVector Passed to `httr2::resp_body_json()` when not `FALSE`.
#' @param parse_failure_warn If non-NULL, warn with this message when a 2xx
#'   response body cannot be parsed as JSON.
#'
#' @return Parsed JSON, or `NULL`.
#'
#' @noRd
.scholidonline_http_perform_json_body <- function(
    req = NULL,
    url = NULL,
    quiet,
    provider_label,
    before_request = NULL,
    simplifyVector = FALSE,
    parse_failure_warn = NULL
) {
  resp <- if (!is.null(req)) {
    .scholidonline_http_perform(
      req = req,
      quiet = quiet,
      before_request = before_request
    )
  } else {
    .scholidonline_http_get(
      url = url,
      quiet = quiet,
      before_request = before_request
    )
  }

  if (is.null(resp)) {
    .scholidonline_http_warn_failed(
      provider_label = provider_label,
      quiet = quiet
    )
    return(NULL)
  }

  status <- .scholidonline_resp_status(resp = resp)

  if (status < 200L || status >= 300L) {
    .scholidonline_http_warn_status(
      provider_label = provider_label,
      status = status,
      quiet = quiet
    )
    return(NULL)
  }

  args <- list(resp = resp)

  if (!identical(simplifyVector, FALSE)) {
    args$simplifyVector <- simplifyVector
  }

  json <- tryCatch(
    do.call(.scholidonline_resp_body_json, args),
    error = function(e) NULL
  )

  if (is.null(json) && !is.null(parse_failure_warn) && !isTRUE(quiet)) {
    rlang::warn(parse_failure_warn)
  }

  json
}


#' Perform a prepared HTTP request and parse JSON
#'
#' @param req An httr2 request object.
#' @param quiet Logical.
#' @param provider_label Provider label used in warning text.
#' @param before_request Optional zero-argument function run before perform.
#' @param silent_404 If `TRUE`, HTTP 404 returns `NULL` without warning.
#' @param simplifyVector Passed to `httr2::resp_body_json()`.
#'
#' @return Parsed JSON, or `NULL`.
#'
#' @noRd
.scholidonline_http_request_json <- function(
    req,
    quiet,
    provider_label,
    before_request = NULL,
    silent_404 = FALSE,
    simplifyVector = FALSE
) {
  resp <- .scholidonline_http_perform(
    req = req,
    quiet = quiet,
    before_request = before_request
  )

  .scholidonline_http_json_from_response(
    resp = resp,
    quiet = quiet,
    provider_label = provider_label,
    silent_404 = silent_404,
    simplifyVector = simplifyVector
  )
}
