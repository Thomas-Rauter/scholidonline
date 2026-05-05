# Level 1 function (functions called by exported functions) definitions --------


#' Run a unary scholidonline operation
#'
#' @description
#' Internal execution engine for unary scholidonline operations such as
#' `exists`, `meta`, and `links`.
#'
#' This function assumes that all inputs have already been validated and
#' normalized by the exported front-end functions. In particular:
#'
#' - `x` must contain normalized identifiers
#' - `type` must contain supported identifier types
#' - `provider` must be a valid provider choice or `"auto"`
#'
#' The engine performs the following steps:
#'
#' 1. Retrieve operation metadata from the registry
#' 2. Resolve the provider (including `"auto"` defaults)
#' 3. Resolve the dispatcher implementation
#' 4. Execute the operation elementwise
#' 5. Enforce the declared return contract
#'
#' The engine itself performs no validation or normalization.
#'
#' @param x A character vector of normalized identifiers.
#' @param operation A single string giving the unary operation
#'   (e.g. `"exists"`).
#' @param type A character vector of identifier types. Must be either
#'   length 1 or the same length as `x`.
#' @param provider Provider choice or `"auto"`.
#' @param ... Passed to provider implementations.
#' @param quiet Logical flag forwarded to provider implementations.
#'
#' @return A list of scalar results, one per input element. The scalar type
#'   depends on the operation.
#'
#' @noRd
.scholidonline_run_unary <- function(
    x,
    operation,
    type,
    provider,
    ...,
    quiet
) {
  
  n <- length(x)
  
  if (length(type) == 1L) {
    type_vec <- rep(
      x = type,
      times = n
    )
  } else if (length(type) == n) {
    type_vec <- type
  } else {
    rlang::abort("`type` must have length 1 or length(x).")
  }
  
  out <- vector(
    mode = "list",
    length = n
  )
  
  batch <- .run_unary_batch(
    x = x,
    operation = operation,
    type = type_vec,
    provider = provider,
    ...,
    quiet = quiet
  )
  
  if (!is.null(batch)) {
    return(batch)
  }
  
  for (i in seq_len(n)) {
    
    xi <- x[[i]]
    type_i <- type_vec[[i]]
    
    if (is.na(xi) || is.na(type_i)) {
      out[[i]] <- NA
      next
    }
    
    meta <- .scholidonline_get_unary_meta(
      type = type_i,
      operation = operation
    )
    
    provider_i <- .scholidonline_resolve_unary_provider(
      provider = provider,
      meta = meta
    )
    
    dispatcher <- .scholidonline_get_dispatcher(
      name = meta$dispatcher
    )
    
    out[[i]] <- .scholidonline_run_unary_one(
      x = xi,
      dispatcher = dispatcher,
      provider = provider_i,
      operation = operation,
      quiet = quiet,
      ...
    )
  }
  
  out
}


# Level 2 function (functions called by level 1 functions) definitions ---------


#' Get unary operation metadata from registry
#'
#' @description
#' Internal helper used by the unary engine to retrieve capability metadata
#' for a given identifier `type` and unary `operation`.
#'
#' The metadata is read from the scholidonline registry and contains the
#' provider set, default provider, and dispatcher function name.
#'
#' @param type A single identifier type string.
#' @param operation A single unary operation string (e.g. `"exists"`).
#'
#' @return A named list containing:
#' \itemize{
#'   \item `providers`
#'   \item `default_provider`
#'   \item `dispatcher`
#' }
#'
#' @noRd
.scholidonline_get_unary_meta <- function(
    type,
    operation
) {
  
  reg <- .scholidonline_registry()
  
  type_block <- reg[[type]]
  
  if (is.null(type_block)) {
    rlang::abort(paste0("Unknown identifier type: ", type, "."))
  }
  
  op_block <- type_block[[operation]]
  
  if (is.null(op_block)) {
    rlang::abort(
      paste0(
        "Operation `",
        operation,
        "` not supported for type `",
        type,
        "`."
      )
    )
  }
  
  providers <- op_block$providers
  default_provider <- op_block$default_provider
  dispatcher <- op_block$dispatcher
  
  if (is.null(providers) || !length(providers)) {
    rlang::abort(
      paste0(
        "Registry error: missing `providers` for ",
        type,
        " -> ",
        operation,
        "."
      )
    )
  }
  
  if (is.null(default_provider)) {
    rlang::abort(
      paste0(
        "Registry error: missing `default_provider` for ",
        type,
        " -> ",
        operation,
        "."
      )
    )
  }
  
  if (is.null(dispatcher)) {
    rlang::abort(
      paste0(
        "Registry error: missing `dispatcher` for ",
        type,
        " -> ",
        operation,
        "."
      )
    )
  }
  
  list(
    providers = providers,
    default_provider = default_provider,
    dispatcher = dispatcher
  )
}


#' Resolve a provider for a unary scholidonline operation
#'
#' @description
#' Internal helper used by the unary dispatch engine to validate the
#' provider argument for a unary operation.
#'
#' For unary operations, `"auto"` is preserved so that dispatchers can
#' implement operation-specific fallback behavior.
#'
#' @param provider A single provider string or `"auto"`.
#' @param meta A named list of unary operation metadata.
#'
#' @return A single validated provider string.
#'
#' @noRd
.scholidonline_resolve_unary_provider <- function(
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
        "Provider `", provider, "` is not supported for this identifier type. ",
        "Available providers: ",
        paste0("`", choices, "`", collapse = ", "),
        "."
      )
    )
  }
  
  provider
}


#' Run one unary scholidonline operation
#'
#' @description
#' Internal helper used by `.scholidonline_run_unary()` to execute a unary
#' operation for a single normalized identifier.
#'
#' This helper:
#'
#' - calls the unary dispatcher with the resolved provider
#' - forwards `...` and `quiet` to the dispatcher
#' - validates the returned value against the unary return contract
#'
#' It assumes that:
#'
#' - `x` is a single normalized identifier
#' - `dispatcher` is a valid function object
#' - `provider` is already resolved
#' - `operation` is a supported unary operation
#'
#' @param x A single normalized identifier string.
#' @param dispatcher A unary dispatcher function.
#' @param provider A single resolved provider string.
#' @param operation A single unary operation string.
#' @param ... Passed to the dispatcher.
#' @param quiet Logical flag forwarded to the dispatcher.
#'
#' @return A single value matching the unary operation return contract.
#'
#' @noRd
.scholidonline_run_unary_one <- function(
    x,
    dispatcher,
    provider,
    operation,
    ...,
    quiet
) {
  
  if (!is.function(dispatcher)) {
    rlang::abort("`dispatcher` must be a function.")
  }
  
  return_mode <- .scholidonline_unary_return_mode(
    operation = operation
  )
  
  result <- dispatcher(
    x = x,
    provider = provider,
    ...,
    quiet = quiet
  )
  
  .scholidonline_validate_unary_result(
    x = result,
    return_mode = return_mode
  )
}


#' Get the return mode for a unary scholidonline operation
#'
#' @description
#' Internal helper used by the unary dispatch engine to determine the
#' expected return mode for a unary operation.
#'
#' The return mode defines the value shape that provider implementations
#' must satisfy after dispatch. It is used by the engine to validate and
#' standardize provider results.
#'
#' Current unary return modes are:
#'
#' - `"logical_scalar"` for `exists`
#' - `"list_scalar"` for `meta`
#' - `"list_scalar"` for `links`
#'
#' @param operation A single unary operation string.
#'
#' @return A single return mode string.
#'
#' @noRd
.scholidonline_unary_return_mode <- function(
    operation
) {
  if (!is.character(operation) || length(operation) != 1L ||
      is.na(operation)) {
    rlang::abort(
      "`operation` must be a single, non-missing character string."
    )
  }
  
  switch(
    operation,
    exists = "logical_scalar",
    meta = "list_scalar",
    links = "list_scalar",
    rlang::abort(paste0("Unknown unary operation: `", operation, "`."))
  )
}


#' Get a unary batch dispatcher
#'
#' @description
#' Internal helper used by the unary engine to resolve an optional batch
#' dispatcher for a unary operation/type/provider combination.
#'
#' Batch dispatcher names follow the scalar dispatcher naming convention with
#' a `_batch` suffix. For example, `.links_pmid_batch`.
#'
#' @param meta A named list of unary operation metadata.
#'
#' @return A function if a batch dispatcher exists, otherwise `NULL`.
#'
#' @noRd
.get_unary_batch_dispatcher <- function(meta) {
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


#' Run a unary scholidonline operation in batch mode
#'
#' @description
#' Internal helper used by `.scholidonline_run_unary()` to execute a unary
#' operation through an optional batch dispatcher.
#'
#' Batch execution is only attempted when all inputs have the same identifier
#' type and resolve to the same provider. If no batch dispatcher exists, this
#' helper returns `NULL` and the scalar engine path is used.
#'
#' @param x A character vector of normalized identifiers.
#' @param operation A single unary operation string.
#' @param type A character vector of identifier types with length `length(x)`.
#' @param provider Provider choice or `"auto"`.
#' @param ... Passed to the batch dispatcher.
#' @param quiet Logical flag forwarded to the batch dispatcher.
#'
#' @return A list of scalar results, one per input, or `NULL` if no batch path
#'   is available.
#'
#' @noRd
.run_unary_batch <- function(
    x,
    operation,
    type,
    provider,
    ...,
    quiet
) {
  n <- length(x)
  
  if (n < 1L) {
    return(NULL)
  }
  
  if (length(type) != n) {
    rlang::abort("`type` must have length `length(x)`.")
  }
  
  if (any(is.na(x)) || any(is.na(type))) {
    return(NULL)
  }
  
  if (length(unique(type)) != 1L) {
    return(NULL)
  }
  
  type_i <- type[[1L]]
  
  meta <- .scholidonline_get_unary_meta(
    type = type_i,
    operation = operation
  )
  
  provider_i <- .scholidonline_resolve_unary_provider(
    provider = provider,
    meta = meta
  )
  
  dispatcher <- .get_unary_batch_dispatcher(
    meta = meta
  )
  
  if (is.null(dispatcher)) {
    return(NULL)
  }
  
  out <- dispatcher(
    x = x,
    provider = provider_i,
    ...,
    quiet = quiet
  )
  
  if (is.null(out)) {
    return(NULL)
  }
  
  .validate_unary_batch_result(
    x = out,
    operation = operation,
    n = n
  )
}


# Level 3 function (functions called by level 2 functions) definitions ---------


#' Validate the result of a unary scholidonline operation
#'
#' @description
#' Internal helper used by the unary engine to validate and standardize the
#' return value of a unary operation according to its declared return mode.
#'
#' @param x The value returned by a unary dispatcher.
#' @param return_mode A single return mode string.
#'
#' @return A validated scalar value.
#'
#' @noRd
.scholidonline_validate_unary_result <- function(
    x,
    return_mode
) {
  
  if (!is.character(return_mode) || length(return_mode) != 1L ||
      is.na(return_mode)) {
    rlang::abort(
      "`return_mode` must be a single, non-missing character string."
    )
  }
  
  switch(
    return_mode,
    logical_scalar = .scholidonline_as_logical_scalar(x),
    list_scalar = .scholidonline_as_list_scalar(x),
    rlang::abort(
      paste0("Unknown unary `return_mode`: `", return_mode, "`.")
    )
  )
}


#' Validate a unary batch result
#'
#' @description
#' Internal helper used by the unary engine to validate and standardize the
#' return value of a batch dispatcher.
#'
#' Batch dispatchers must return a list with one element per input. Each element
#' is validated against the scalar return contract for the operation.
#'
#' @param x The value returned by a unary batch dispatcher.
#' @param operation A single unary operation string.
#' @param n Expected output length.
#'
#' @return A validated list with one element per input.
#'
#' @noRd
.validate_unary_batch_result <- function(
    x,
    operation,
    n
) {
  if (!is.list(x) || is.data.frame(x)) {
    rlang::abort("Unary batch dispatchers must return a list.")
  }
  
  if (length(x) != n) {
    rlang::abort(
      "Unary batch dispatcher output must have length `length(x)`."
    )
  }
  
  return_mode <- .scholidonline_unary_return_mode(
    operation = operation
  )
  
  lapply(
    x,
    .scholidonline_validate_unary_result,
    return_mode = return_mode
  )
}


# Level 4 function (functions called by level 3 functions) definitions ---------


#' Coerce value to a list-like scalar result
#'
#' @param x An object returned by a unary dispatcher.
#'
#' @return A validated scalar list-like object.
#'
#' @noRd
.scholidonline_as_list_scalar <- function(x) {
  if (is.null(x)) {
    return(data.frame())
  }
  
  if (is.data.frame(x)) {
    return(x)
  }
  
  if (is.list(x) && !is.data.frame(x) && length(x) == 1L) {
    return(x)
  }
  
  rlang::abort("`x` must be a data.frame, NULL, or a scalar list object.")
}