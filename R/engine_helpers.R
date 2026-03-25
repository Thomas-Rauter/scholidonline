# Level 2 function (functions called by level 1 functions) definitions ---------


#' Get a scholidonline dispatcher function
#'
#' @description
#' Internal helper used by the scholidonline dispatch engine to resolve a
#' dispatcher function by name.
#'
#' This helper looks up a function in the package namespace and throws a
#' descriptive error if the implementation is missing.
#'
#' It is used by both unary and binary execution engines to resolve
#' dispatcher functions declared in the registry metadata.
#'
#' @param name A single dispatcher function name string.
#'
#' @return A function object corresponding to the requested dispatcher.
#'
#' @noRd
.scholidonline_get_dispatcher <- function(
    name
) {
  
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    rlang::abort(
      message = "`name` must be a single, non-missing character string."
    )
  }
  
  fun <- get0(
    x = name,
    mode = "function",
    inherits = TRUE
  )
  
  if (is.null(fun)) {
    rlang::abort(
      message = paste0(
        "Missing implementation: ",
        name,
        "()."
      )
    )
  }
  
  fun
}


# Level 3 function (functions called by level 2 functions) definitions ---------


#' Get a scholidonline dispatcher function
#'
#' @description
#' Internal helper used by the scholidonline dispatch engine to resolve a
#' dispatcher function by name.
#'
#' This helper looks up a function in the package namespace and throws a
#' descriptive error if the implementation is missing.
#'
#' It is used by both unary and binary execution engines to resolve
#' dispatcher functions declared in the registry metadata.
#'
#' @param name A single dispatcher function name string.
#'
#' @return A function object corresponding to the requested dispatcher.
#'
#' @noRd
.scholidonline_as_logical_scalar <- function(
    x
) {
  
  if (!is.logical(x) || length(x) != 1L) {
    rlang::abort(
      message = "Provider implementation must return a single logical value."
    )
  }
  
  x
}


#' Get a scholidonline dispatcher function
#'
#' @description
#' Internal helper used by the scholidonline dispatch engine to resolve a
#' dispatcher function by name.
#'
#' This helper looks up a function in the package namespace and throws a
#' descriptive error if the implementation is missing.
#'
#' It is used by both unary and binary execution engines to resolve
#' dispatcher functions declared in the registry metadata.
#'
#' @param name A single dispatcher function name string.
#'
#' @return A function object corresponding to the requested dispatcher.
#'
#' @noRd
.scholidonline_as_character_scalar <- function(
    x
) {
  
  if (!is.character(x) || length(x) != 1L) {
    rlang::abort(
      message = "Provider implementation must return a single character value."
    )
  }
  
  x
}