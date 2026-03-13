#' Provider resolution helpers for scholidonline
#'
#' @description
#' Internal utilities used to resolve and validate provider selections
#' for registry-backed operations.
#'
#' These helpers ensure that provider arguments are valid for a given
#' identifier type or conversion and implement the `"auto"` provider
#' behavior using registry-defined defaults.
#'
#' They are used by front-end functions such as `id_exists()` and
#' `id_convert()` to dispatch work to provider-specific implementations.
#'
#' @keywords internal
NULL


# Level 1 function (functions called by exported functions) definitions --------


#' Match and validate a provider
#'
#' Internal helper used by front-end functions to validate provider
#' arguments and resolve `"auto"` selections.
#'
#' @param provider A single provider string.
#' @param choices A character vector of valid providers for the operation.
#' @param default_provider A single provider string to use when
#'   `provider = "auto"`. Must be one of the concrete providers in `choices`.
#'
#' @return A single provider string.
#'
#' @noRd
.scholidonline_match_provider <- function(
    provider,
    choices,
    default_provider = NULL
) {
  
  if (!is.character(provider) || length(provider) != 1L || is.na(provider)) {
    stop(
      "`provider` must be a single, non-missing character string.",
      call. = FALSE
    )
  }
  
  if (!is.character(choices) || length(choices) < 1L || anyNA(choices)) {
    stop(
      "`choices` must be a non-empty character vector without NA.",
      call. = FALSE
    )
  }
  
  choices <- unique(choices)
  concrete <- choices[choices != "auto"]
  
  if (length(concrete) < 1L) {
    stop("No concrete providers available.", call. = FALSE)
  }
  
  if (is.null(default_provider)) {
    default_provider <- concrete[[1L]]
  }
  
  if (!is.character(default_provider) ||
      length(default_provider) != 1L ||
      is.na(default_provider)) {
    stop(
      "`default_provider` must be a single, non-missing character string.",
      call. = FALSE
    )
  }
  
  if (!default_provider %in% concrete) {
    stop(
      "`default_provider` must be one of: ",
      paste0("`", concrete, "`", collapse = ", "),
      call. = FALSE
    )
  }
  
  if (identical(provider, "auto")) {
    return(default_provider)
  }
  
  if (!provider %in% concrete) {
    stop(
      "Unknown provider: `", provider, "`. Must be one of: ",
      paste0("`", concrete, "`", collapse = ", "),
      call. = FALSE
    )
  }
  
  provider
}