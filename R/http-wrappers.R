# These wrappers where made to make the offline testing much easier. 


#' HTTP request wrapper
#'
#' @param url A single URL string.
#'
#' @return An httr2 request object.
#'
#' @noRd
.scholidonline_request <- function(url) {
  httr2::request(url)
}


#' HTTP query wrapper
#'
#' @param req An httr2 request object.
#' @param ... Query parameters.
#'
#' @return An httr2 request object.
#'
#' @noRd
.scholidonline_req_url_query <- function(
    req,
    ...
) {
  query <- rlang::list2(...)
  
  httr2::req_url_query(
    .req = req,
    !!!
      query
  )
}


#' HTTP header wrapper
#'
#' @param req An httr2 request object.
#' @param ... Header fields.
#'
#' @return An httr2 request object.
#'
#' @noRd
.scholidonline_req_headers <- function(
    req,
    ...
) {
  httr2::req_headers(
    .req = req,
    ...
  )
}


#' HTTP error policy wrapper
#'
#' @param req An httr2 request object.
#' @param is_error A function taking a response and returning logical.
#'
#' @return An httr2 request object.
#'
#' @noRd
.scholidonline_req_error <- function(
    req,
    is_error = function(resp) FALSE
) {
  httr2::req_error(
    req = req,
    is_error = is_error
  )
}


#' HTTP perform wrapper
#'
#' @param req An httr2 request object.
#'
#' @return An httr2 response object.
#'
#' @noRd
.scholidonline_req_perform <- function(req) {
  httr2::req_perform(req = req)
}


#' Safe HTTP perform wrapper
#'
#' @param req An httr2 request object.
#'
#' @return An httr2 response object, or `NULL` on error.
#'
#' @noRd
.scholidonline_req_perform_safe <- function(req) {
  tryCatch(
    .scholidonline_req_perform(req),
    error = function(e) NULL
  )
}


#' HTTP status wrapper
#'
#' @param resp An httr2 response object.
#'
#' @return A single integer HTTP status code.
#'
#' @noRd
.scholidonline_resp_status <- function(resp) {
  httr2::resp_status(resp = resp)
}


#' HTTP JSON body wrapper
#'
#' @param resp An httr2 response object.
#' @param ... Passed to `httr2::resp_body_json()`.
#'
#' @return A parsed JSON object.
#'
#' @noRd
.scholidonline_resp_body_json <- function(
    resp,
    ...
) {
  httr2::resp_body_json(
    resp = resp,
    ...
  )
}


#' HTTP string body wrapper
#'
#' @param resp An httr2 response object.
#' @param ... Passed to `httr2::resp_body_string()`.
#'
#' @return A single character string.
#'
#' @noRd
.scholidonline_resp_body_string <- function(
    resp,
    ...
) {
  httr2::resp_body_string(
    resp = resp,
    ...
  )
}