# Level 2 function (functions called by level 1 functions) definitions ---------


#' Get a scholidonline dispatcher function
#'
#' @description
#' Internal helper used by the scholidonline dispatch engine to resolve a
#' dispatcher function by name.
#'
#' This helper looks up a function in the package namespace and throws a clean
#' error if the implementation is missing.
#'
#' It is used by both unary and binary engines to resolve dispatcher functions
#' declared in the registry metadata.
#'
#' @param name A single dispatcher function name string.
#'
#' @return A function object.
#'
#' @noRd
.scholidonline_get_dispatcher <- function(
    name
) {
  
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop(
      "`name` must be a single, non-missing character string.",
      call. = FALSE
    )
  }
  
  fun <- get0(
    x = name,
    mode = "function",
    inherits = TRUE
  )
  
  if (is.null(fun)) {
    stop(
      "Missing implementation: ",
      name,
      "().",
      call. = FALSE
    )
  }
  
  fun
}


# Level 3 function (functions called by level 2 functions) definitions ---------


#' Enforce logical scalar return contract
#'
#' @description
#' Internal helper used by the scholidonline engine to enforce that a provider
#' implementation returns a single logical value.
#'
#' This helper validates that the result returned by a provider adapter is a
#' logical scalar (`TRUE`, `FALSE`, or `NA`). If the return value does not
#' satisfy this contract, a descriptive error is thrown.
#'
#' @param x The value returned by a provider implementation.
#'
#' @return A single logical value.
#'
#' @noRd
.scholidonline_as_logical_scalar <- function(
    x
) {
  
  if (!is.logical(x) || length(x) != 1L) {
    stop(
      "Provider implementation must return a single logical value.",
      call. = FALSE
    )
  }
  
  x
}

  
#' Enforce character scalar return contract
#'
#' @description
#' Internal helper used by the scholidonline engine to enforce that a provider
#' implementation returns a single character value.
#'
#' This helper validates that the result returned by a provider adapter is a
#' character scalar. If the return value does not satisfy this contract, a
#' descriptive error is thrown.
#'
#' @param x The value returned by a provider implementation.
#'
#' @return A single character value.
#'
#' @noRd
.scholidonline_as_character_scalar <- function(
    x
) {
  
  if (!is.character(x) || length(x) != 1L) {
    stop(
      "Provider implementation must return a single character value.",
      call. = FALSE
    )
  }
  
  x
}