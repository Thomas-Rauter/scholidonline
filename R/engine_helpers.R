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


#' Resolve a provider for a scholidonline operation
#'
#' @description
#' Internal helper used by the unary and binary dispatch engines to validate
#' the provider argument against registry metadata.
#'
#' For both engines, `"auto"` is preserved so that dispatchers can implement
#' operation-specific fallback behavior.
#'
#' @param provider A single provider string or `"auto"`.
#' @param meta A named list of operation metadata containing `providers`.
#'
#' @return A single validated provider string.
#'
#' @noRd
.scholidonline_resolve_provider <- function(
    provider,
    meta
) {
  if (!is.list(meta)) {
    rlang::abort("`meta` must be a list.")
  }

  if (is.null(meta$providers)) {
    rlang::abort("`meta` must contain `providers`.")
  }

  choices <- unique(meta$providers)

  if (!is.character(provider) || length(provider) != 1L || is.na(provider)) {
    rlang::abort(
      "`provider` must be a single, non-missing character string."
    )
  }

  if (!provider %in% choices) {
    rlang::abort(
      message = paste0(
        "Provider `",
        provider,
        "` is not supported. Available providers: ",
        paste0("`", choices, "`", collapse = ", "),
        "."
      )
    )
  }

  provider
}


#' Get a scholidonline batch dispatcher
#'
#' @description
#' Internal helper used by the unary and binary engines to resolve an optional
#' batch dispatcher for an operation.
#'
#' Batch dispatcher names follow the scalar dispatcher naming convention with
#' a `_batch` suffix.
#'
#' @param meta A named list of operation metadata containing `dispatcher`.
#'
#' @return A function if a batch dispatcher exists, otherwise `NULL`.
#'
#' @noRd
.scholidonline_get_batch_dispatcher <- function(meta) {
  if (!is.list(meta)) {
    rlang::abort("`meta` must be a list.")
  }

  if (is.null(meta$dispatcher)) {
    rlang::abort("`meta` must contain `dispatcher`.")
  }

  name <- paste0(meta$dispatcher, "_batch")

  if (!exists(name, mode = "function", inherits = TRUE)) {
    return(NULL)
  }

  get(name, mode = "function", inherits = TRUE)
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