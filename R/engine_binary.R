# Level 1 function (functions called by exported functions) definitions --------


#' Run a binary scholidonline operation
#'
#' @description
#' Internal execution engine for binary scholidonline operations such as
#' identifier conversion.
#'
#' This function assumes that all inputs have already been validated and
#' normalized by the exported front-end functions. In particular:
#'
#' - `x` must contain normalized identifiers
#' - `from` must contain supported source identifier types
#' - `to` must be a single supported target identifier type
#' - `provider` must be a valid provider choice or `"auto"`
#'
#' The engine performs the following steps:
#'
#' 1. Retrieve conversion metadata from the registry
#' 2. Resolve the provider (including `"auto"`)
#' 3. Resolve the dispatcher implementation
#' 4. Execute the operation elementwise
#' 5. Enforce the declared return contract
#'
#' The engine itself performs no validation or normalization.
#'
#' @param x A character vector of normalized identifiers.
#' @param from A character vector of source identifier types. Must be either
#'   length 1 or the same length as `x`.
#' @param to A single target identifier type string.
#' @param provider Provider choice or `"auto"`.
#' @param ... Passed to provider implementations.
#' @param quiet Logical flag forwarded to provider implementations.
#'
#' @return A character vector of scalar results, one per input element.
#'
#' @noRd
.scholidonline_run_binary <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet
) {
  
  n <- length(x)
  
  if (length(from) == 1L) {
    from_vec <- rep(
      x = from,
      times = n
    )
  } else if (length(from) == n) {
    from_vec <- from
  } else {
    rlang::abort("`from` must have length 1 or length(x).")
  }
  
  out <- rep(
    x = NA_character_,
    times = n
  )
  
  batch <- .run_binary_batch(
    x = x,
    from = from_vec,
    to = to,
    provider = provider,
    ...,
    quiet = quiet
  )
  
  if (!is.null(batch)) {
    return(batch)
  }
  
  for (i in seq_len(n)) {
    
    xi <- x[[i]]
    from_i <- from_vec[[i]]
    
    if (is.na(xi) || is.na(from_i)) {
      out[[i]] <- NA_character_
      next
    }
    
    if (.scholidonline_binary_identity(from = from_i, to = to)) {
      out[[i]] <- xi
      next
    }
    
    meta <- .scholidonline_get_binary_meta(
      from = from_i,
      to = to
    )
    
    provider_i <- .scholidonline_resolve_binary_provider(
      provider = provider,
      meta = meta
    )
    
    dispatcher <- .scholidonline_get_dispatcher(
      name = meta$dispatcher
    )
    
    out[[i]] <- .scholidonline_run_binary_one(
      x = xi,
      dispatcher = dispatcher,
      from = from_i,
      to = to,
      provider = provider_i,
      quiet = quiet,
      ...
    )
  }
  
  out
}


# Level 2 function (functions called by level 1 functions) definitions --------


#' Get binary operation metadata from registry
#'
#' @description
#' Internal helper used by the binary engine to retrieve capability metadata
#' for a given source identifier type `from` and target identifier type `to`.
#'
#' The metadata is read from the scholidonline registry and contains the
#' provider set, default provider, and dispatcher function name.
#'
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#'
#' @return A named list containing:
#' \itemize{
#'   \item `providers`
#'   \item `default_provider`
#'   \item `dispatcher`
#' }
#'
#' @noRd
.scholidonline_get_binary_meta <- function(
    from,
    to
) {
  
  reg <- .scholidonline_registry()
  
  from_block <- reg[[from]]
  
  if (is.null(from_block)) {
    rlang::abort(paste0("Unknown source identifier type: ", from, "."))
  }
  
  conv_block <- from_block$convert
  
  if (is.null(conv_block)) {
    rlang::abort(
      paste0("No binary operations supported for type `", from, "`.")
    )
  }
  
  pair_block <- conv_block[[to]]
  
  if (is.null(pair_block)) {
    rlang::abort(
      paste0("Unsupported conversion: `", from, "` -> `", to, "`.")
    )
  }
  
  providers <- pair_block$providers
  default_provider <- pair_block$default_provider
  dispatcher <- pair_block$dispatcher
  
  if (is.null(providers) || !length(providers)) {
    rlang::abort(
      paste0(
        "Registry error: missing `providers` for ",
        from,
        " -> ",
        to,
        "."
      )
    )
  }
  
  if (is.null(default_provider)) {
    rlang::abort(
      paste0(
        "Registry error: missing `default_provider` for ",
        from,
        " -> ",
        to,
        "."
      )
    )
  }
  
  if (is.null(dispatcher)) {
    rlang::abort(
      paste0(
        "Registry error: missing `dispatcher` for ",
        from,
        " -> ",
        to,
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


#' Resolve a provider for a binary scholidonline operation
#'
#' @description
#' Internal helper used by the binary dispatch engine to validate the
#' provider argument for a binary operation.
#'
#' For binary operations, `"auto"` is preserved so that dispatchers can
#' implement pair-specific fallback behavior.
#'
#' @param provider A single provider string or `"auto"`.
#' @param meta A named list of binary operation metadata.
#'
#' @return A single validated provider string.
#'
#' @noRd
.scholidonline_resolve_binary_provider <- function(
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
      paste0(
        "Unknown provider: `",
        provider,
        "`. Must be one of: ",
        paste0("`", choices, "`", collapse = ", ")
      )
    )
  }
  
  provider
}


#' Run a binary scholidonline operation in batch mode
#'
#' @description
#' Internal helper used by `.scholidonline_run_binary()` to execute a binary
#' operation through an optional batch dispatcher.
#'
#' Batch execution is only attempted when all inputs have the same source
#' identifier type, the conversion is not an identity mapping, and a batch
#' dispatcher exists. If no batch dispatcher exists, this helper returns
#' `NULL` and the scalar engine path is used.
#'
#' @param x A character vector of normalized identifiers.
#' @param from A character vector of source identifier types with length
#'   `length(x)`.
#' @param to A single target identifier type string.
#' @param provider Provider choice or `"auto"`.
#' @param ... Passed to the batch dispatcher.
#' @param quiet Logical flag forwarded to the batch dispatcher.
#'
#' @return A character vector with one value per input, or `NULL` if no batch
#'   path is available.
#'
#' @noRd
.run_binary_batch <- function(
    x,
    from,
    to,
    provider,
    ...,
    quiet
) {
  n <- length(x)
  
  if (n < 1L) {
    return(NULL)
  }
  
  if (length(from) != n) {
    rlang::abort("`from` must have length `length(x)`.")
  }
  
  if (any(is.na(x)) || any(is.na(from))) {
    return(NULL)
  }
  
  if (length(unique(from)) != 1L) {
    return(NULL)
  }
  
  from_i <- from[[1L]]
  
  if (.scholidonline_binary_identity(from = from_i, to = to)) {
    return(NULL)
  }
  
  meta <- .scholidonline_get_binary_meta(
    from = from_i,
    to = to
  )
  
  provider_i <- .scholidonline_resolve_binary_provider(
    provider = provider,
    meta = meta
  )
  
  dispatcher <- .get_binary_batch_dispatcher(
    meta = meta
  )
  
  if (is.null(dispatcher)) {
    return(NULL)
  }
  
  out <- dispatcher(
    x = x,
    from = from_i,
    to = to,
    provider = provider_i,
    ...,
    quiet = quiet
  )
  
  if (is.null(out)) {
    return(NULL)
  }
  
  .validate_binary_batch_result(
    x = out,
    n = n
  )
}


#' Check whether a binary scholidonline operation is an identity mapping
#'
#' @description
#' Internal helper used by the binary engine to determine whether a binary
#' operation is an identity mapping, i.e. source and target identifier
#' types are the same.
#'
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#'
#' @return A single logical value.
#'
#' @noRd
.scholidonline_binary_identity <- function(
    from,
    to
) {
  
  if (!is.character(from) || length(from) != 1L || is.na(from)) {
    rlang::abort("`from` must be a single, non-missing character string.")
  }
  
  if (!is.character(to) || length(to) != 1L || is.na(to)) {
    rlang::abort("`to` must be a single, non-missing character string.")
  }
  
  identical(from, to)
}


#' Run one binary scholidonline operation
#'
#' @description
#' Internal helper used by `.scholidonline_run_binary()` to execute a
#' binary operation for a single normalized identifier.
#'
#' This helper:
#'
#' - calls the binary dispatcher with the resolved provider
#' - forwards `...` and `quiet` to the dispatcher
#' - validates the returned value against the binary return contract
#'
#' It assumes that:
#'
#' - `x` is a single normalized identifier
#' - `dispatcher` is a valid function object
#' - `provider` is already resolved
#' - `from` and `to` describe a supported conversion pair
#'
#' @param x A single normalized identifier string.
#' @param dispatcher A binary dispatcher function.
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#' @param provider A single resolved provider string.
#' @param ... Passed to the dispatcher.
#' @param quiet Logical flag forwarded to the dispatcher.
#'
#' @return A single character value matching the binary return contract.
#'
#' @noRd
.scholidonline_run_binary_one <- function(
    x,
    dispatcher,
    from,
    to,
    provider,
    ...,
    quiet
) {
  
  if (!is.function(dispatcher)) {
    rlang::abort("`dispatcher` must be a function.")
  }
  
  result <- dispatcher(
    x = x,
    from = from,
    to = to,
    provider = provider,
    ...,
    quiet = quiet
  )
  
  .scholidonline_as_character_scalar(result)
}


# Level 2 functions (functions called by level 1 functions) --------------------


#' Get a binary batch dispatcher
#'
#' @description
#' Internal helper used by the binary engine to resolve an optional batch
#' dispatcher for a source/target/provider conversion combination.
#'
#' Batch dispatcher names follow the scalar dispatcher naming convention with
#' a `_batch` suffix. For example, `.convert_pmid_to_pmcid_batch`.
#'
#' @param meta A named list of binary operation metadata.
#'
#' @return A function if a batch dispatcher exists, otherwise `NULL`.
#'
#' @noRd
.get_binary_batch_dispatcher <- function(meta) {
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


#' Validate a binary batch result
#'
#' @description
#' Internal helper used by the binary engine to validate and standardize the
#' return value of a batch dispatcher.
#'
#' Batch dispatchers must return a character vector with one value per input.
#'
#' @param x The value returned by a binary batch dispatcher.
#' @param n Expected output length.
#'
#' @return A character vector with one value per input.
#'
#' @noRd
.validate_binary_batch_result <- function(
    x,
    n
) {
  if (!is.character(x)) {
    rlang::abort("Binary batch dispatchers must return a character vector.")
  }
  
  if (length(x) != n) {
    rlang::abort(
      "Binary batch dispatcher output must have length `length(x)`."
    )
  }
  
  x
}