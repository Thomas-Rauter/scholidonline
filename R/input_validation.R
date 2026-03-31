# Input validation helpers for scholidonline


# Level 1 function (functions called by exported functions) definitions --------


#' Validate identifier input vector
#'
#' @description
#' Internal helper for validating inputs expected to be character vectors of
#' identifiers.
#'
#' @param x An input expected to be a character vector.
#' @param arg Name of the argument, used in error messages.
#'
#' @return Invisibly returns `NULL` if validation succeeds.
#'
#' @noRd
.scholidonline_check_x <- function(
    x,
    arg = "x"
) {
  if (missing(x)) {
    rlang::abort(
      paste0("`", arg, "` is required.")
    )
  }
  
  if (is.null(x)) {
    rlang::abort(
      paste0("`", arg, "` must not be NULL.")
    )
  }
  
  if (is.data.frame(x)) {
    rlang::abort(
      paste0("`", arg, "` must not be a data frame.")
    )
  }
  
  if (!is.atomic(x) || !is.character(x)) {
    cls <- paste(class(x), collapse = "/")
    rlang::abort(
      paste0("`", arg, "` must be a character vector, not ", cls, ".")
    )
  }
  
  invisible(NULL)
}


#' Lookup allowed providers for a conversion pair
#'
#' @description
#' Internal helper used by the scholidonline engine to determine the set of
#' supported providers for a given identifier conversion.
#'
#' This helper returns the providers that implement a conversion from `from`
#' to `to`, including `"auto"` where applicable. The mapping is defined
#' explicitly and reflects the currently supported provider implementations.
#'
#' If the conversion pair is not supported, a descriptive error is thrown.
#'
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#'
#' @return A character vector of provider names.
#'
#' @noRd
.scholidonline_conversion_providers <- function(
    from,
    to
) {
  key <- paste(from, to, sep = "->")
  
  providers <- switch(
    key,
    "pmid->doi"   = c("auto", "ncbi", "epmc", "mock"),
    "doi->pmid"   = c("auto", "ncbi", "epmc", "mock"),
    "doi->pmcid"  = c("auto", "ncbi", "epmc", "mock"),
    "pmcid->pmid" = c("auto", "ncbi", "epmc", "mock"),
    "pmcid->doi"  = c("auto", "ncbi", "epmc", "mock"),
    "pmid->pmcid" = c("auto", "ncbi", "epmc", "mock"),
    stop("Unsupported conversion: ", from, " -> ", to, ".", call. = FALSE)
  )
  
  providers
}


#' Validate provider argument
#'
#' @description
#' Internal helper that validates a provider argument against a set of
#' allowed providers.
#'
#' This function performs strict validation only. It does not modify or
#' normalize the input.
#'
#' @param provider A candidate provider string.
#' @param arg Name of the argument used in error messages.
#' @param choices Character vector of allowed providers.
#'
#' @return Invisibly returns `NULL` if validation succeeds.
#'
#' @noRd
.scholidonline_check_provider <- function(
    provider,
    arg = "provider",
    choices
) {
  
  if (!is.character(provider) || length(provider) != 1L ||
      is.na(provider) || !nzchar(provider)) {
    rlang::abort(
      paste0("`", arg, "` must be a single, non-empty character string.")
    )
  }
  
  if (!is.character(choices) || length(choices) == 0L ||
      anyNA(choices) || any(!nzchar(choices))) {
    rlang::abort(
      "`choices` must be a non-empty character vector ",
      "without missing or empty values."
    )
  }
  
  if (!provider %in% choices) {
    rlang::abort(
      paste0(
        "`", arg, "` must be one of: ",
        paste0("`", choices, "`", collapse = ", "),
        "."
      )
    )
  }
  
  invisible(NULL)
}


#' Validate a quiet flag
#'
#' Internal helper used to validate `quiet` arguments in front-end
#' functions.
#'
#' @param quiet Logical flag indicating whether warnings/messages should
#'   be suppressed.
#' @param arg Name of the calling argument.
#'
#' @return Invisibly returns `NULL` if validation succeeds.
#'
#' @details
#' Ensures that the `quiet` argument is a single logical value.
#'
#' @noRd
.scholidonline_check_quiet <- function(
    quiet,
    arg = "quiet"
) {
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    rlang::abort(
      paste0("`", arg, "` must be a single TRUE or FALSE value.")
    )
  }
  
  invisible(NULL)
}


#' Check whether `type` and `provider` are compatible
#'
#' Validates that a given identifier `type` can be used with a given
#' `provider`, according to the internal Scholidonline registry returned
#' by .scholidonline_registry().
#'
#' This function assumes that `type` and `provider` have already passed
#' general input validation (for example via `match.arg()`). It only checks
#' whether the combination is allowed by the registry.
#'
#' The special value `"auto"` is treated like any other provider value and
#' is valid only if it appears in the registry for the given `type`.
#'
#' @param type A length-1 character string giving the identifier type.
#' @param provider A length-1 character string giving the provider.
#'
#' @return `TRUE` invisibly if the combination is valid.
#'
#' @noRd
.scholidonline_check_type_provider <- function(
    type,
    provider
) {
  reg <- .scholidonline_registry()
  
  if (identical(type, "auto")) {
    invisible(NULL)
    return()
  }
  
  allowed_providers <- unique(c(
    reg[[type]][["exists"]][["providers"]],
    reg[[type]][["links"]][["providers"]],
    unlist(
      lapply(
        reg[[type]][["convert"]],
        function(x) x[["providers"]]
      ),
      use.names = FALSE
    )
  ))
  
  if (!provider %in% allowed_providers) {
    rlang::abort(
      paste0(
        "Provider '", provider, "' is not supported for type '",
        type, "'. Allowed providers are: ",
        paste(allowed_providers, collapse = ", "),
        "."
      )
    )
  }
  
  invisible(NULL)
}


#' Return all providers defined in the registry
#'
#' Extracts all providers from the internal Scholidonline registry returned
#' by .scholidonline_registry().
#'
#' Providers are collected across `exists`, `links`, and all `convert`
#' entries, deduplicated, sorted, and returned. The value `"auto"` is
#' always removed because it represents a dispatch mode rather than a
#' real provider.
#'
#' @return A character vector of provider names.
#'
#' @noRd
.scholidonline_providers <- function() {
  reg <- .scholidonline_registry()
  providers <- unlist(
    lapply(reg, function(type_entry) {
      c(
        type_entry[["exists"]][["providers"]],
        type_entry[["links"]][["providers"]],
        unlist(
          lapply(
            type_entry[["convert"]],
            function(convert_entry) convert_entry[["providers"]]
          ),
          use.names = FALSE
        )
      )
    }),
    use.names = FALSE
  )
  
  providers <- sort(unique(providers))
  providers <- setdiff(providers, "auto")
  
  providers
}


#' Validate a scholidonline conversion pair
#'
#' @description
#' Internal helper used by the scholidonline engine to validate that a
#' conversion between two identifier types is supported.
#'
#' This helper checks whether a conversion from `from` to `to` is defined in
#' the central registry. If the conversion is not available, a descriptive
#' error is thrown.
#'
#' Identity mappings (i.e. `from == to`) are always considered valid and are
#' returned without further checks.
#'
#' @param from A single source identifier type string.
#' @param to A single target identifier type string.
#'
#' @return Invisibly returns `TRUE` if the conversion is valid.
#'
#' @noRd
.scholidonline_check_conversion_pair <- function(
    from,
    to
) {
  reg <- .scholidonline_registry()
  
  if (identical(from, to)) {
    return(invisible(TRUE))
  }
  
  if (is.null(reg[[from]]$convert[[to]])) {
    stop(
      "Unsupported conversion: ",
      from,
      " -> ",
      to,
      ".",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


#' Validate a scalar character input
#'
#' @description
#' Internal helper used to enforce that an input argument is a single,
#' non-missing character string.
#'
#' This helper is typically used in provider implementations and dispatchers
#' to validate normalized identifier inputs before further processing.
#'
#' @param x The value to validate.
#' @param arg A single string giving the argument name, used in error messages.
#'
#' @return Invisibly returns `x` if validation succeeds.
#'
#' @noRd
.scholidonline_check_scalar_chr <- function(
    x,
    arg = "x"
) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(
      "`",
      arg,
      "` must be a single, non-missing character string.",
      call. = FALSE
    )
  }
  
  invisible(x)
}


# Level 2 function (functions called by lvl 1 functions) definitions -----------


#' Coerce input to a single trimmed character value
#'
#' @description
#' Internal helper for validating scalar character arguments. Factors are
#' converted to character, whitespace is trimmed, and empty strings are
#' converted to `NA_character_`. Errors are thrown for missing, `NULL`,
#' non-scalar, or non-character inputs.
#'
#' @param x An input value expected to be a scalar character.
#' @param arg Name of the argument, used in error messages.
#'
#' @return A length-one character vector, or `NA_character_` if the input
#'   is an empty string.
#'
#' @noRd
.scholidonline_as_scalar_character <- function(
        x,
        arg
) {
    if (missing(x)) {
        stop("`", arg, "` is required.", call. = FALSE)
    }

    if (is.null(x)) {
        stop("`", arg, "` must not be NULL.", call. = FALSE)
    }

    if (length(x) != 1L) {
        stop("`", arg, "` must be length 1.", call. = FALSE)
    }

    if (is.factor(x)) {
        x <- as.character(x)
    }

    if (!is.character(x)) {
        stop("`", arg, "` must be a character string.", call. = FALSE)
    }

    x <- trimws(x)
    if (!nzchar(x)) {
        return(NA_character_)
    }

    x
}
