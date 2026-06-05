# Shared helpers for NCBI / Europe PMC dispatcher fallback logic


#' Dispatch a scalar operation with NCBI then Europe PMC fallback
#'
#' @param x A single normalized identifier.
#' @param ncbi_fn NCBI provider function.
#' @param epmc_fn Europe PMC provider function.
#' @param is_success Predicate returning `TRUE` when a provider result is usable.
#' @param empty_value Value returned when both providers fail.
#' @param warn_message Warning text when both providers fail and `quiet` is
#'   `FALSE`.
#' @param warn_style Either `"base"` (`warning()`) or `"rlang"` (`rlang::warn()`).
#' @param quiet Logical; suppress warnings when `TRUE`.
#' @param ... Passed to provider functions.
#'
#' @return Provider result or `empty_value`.
#'
#' @noRd
.dispatch_ncbi_epmc_auto <- function(
    x,
    ncbi_fn,
    epmc_fn,
    is_success,
    empty_value,
    warn_message,
    warn_style = c("rlang", "base"),
    quiet = FALSE,
    ...
) {
  warn_style <- match.arg(warn_style)

  out_ncbi <- ncbi_fn(
    x = x,
    ...,
    quiet = TRUE
  )

  if (is_success(out_ncbi)) {
    return(out_ncbi)
  }

  out_epmc <- epmc_fn(
    x = x,
    ...,
    quiet = TRUE
  )

  if (is_success(out_epmc)) {
    return(out_epmc)
  }

  if (!isTRUE(quiet)) {
    if (identical(warn_style, "base")) {
      warning(
        warn_message,
        call. = FALSE
      )
    } else {
      rlang::warn(warn_message)
    }
  }

  empty_value
}


#' Dispatch a scalar operation to an explicit NCBI or Europe PMC provider
#'
#' @param x A single normalized identifier.
#' @param provider Provider name (`"ncbi"` or `"epmc"`).
#' @param ncbi_fn NCBI provider function.
#' @param epmc_fn Europe PMC provider function.
#' @param on_unknown Function called with the unknown provider name.
#' @param quiet Logical; forwarded to provider functions.
#' @param ... Passed to provider functions.
#'
#' @return Provider result.
#'
#' @noRd
.dispatch_ncbi_epmc_provider <- function(
    x,
    provider,
    ncbi_fn,
    epmc_fn,
    on_unknown,
    quiet = FALSE,
    ...
) {
  switch(
    provider,
    ncbi = ncbi_fn(
      x = x,
      ...,
      quiet = quiet
    ),
    epmc = epmc_fn(
      x = x,
      ...,
      quiet = quiet
    ),
    on_unknown(provider)
  )
}


#' Predicate for non-empty data.frame provider results
#'
#' @param out Provider result.
#'
#' @return Logical scalar.
#'
#' @noRd
.dispatch_ncbi_epmc_df_success <- function(out) {
  !is.null(out) && nrow(out) > 0L
}


#' Dispatch batch conversion with NCBI batch then Europe PMC scalar fallback
#'
#' @param x Character vector of normalized identifiers.
#' @param ncbi_batch_fn NCBI batch conversion function.
#' @param epmc_fn Europe PMC scalar conversion function.
#' @param ... Passed to provider functions.
#'
#' @return Character vector with one value per input.
#'
#' @noRd
.dispatch_convert_ncbi_epmc_auto_batch <- function(
    x,
    ncbi_batch_fn,
    epmc_fn,
    ...
) {
  out <- ncbi_batch_fn(
    x = x,
    ...,
    quiet = TRUE
  )

  missing <- is.na(out)

  if (any(missing)) {
    out[missing] <- vapply(
      x[missing],
      epmc_fn,
      character(1),
      ...,
      quiet = TRUE
    )
  }

  out
}
