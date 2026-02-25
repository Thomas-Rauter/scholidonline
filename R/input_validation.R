# Level 1 function (functions called by exported functions) definitions --------


#' Validate vector-like inputs
#'
#' @description
#' Internal helper for validating inputs expected to be vector-like. The
#' function checks that the argument is present, not `NULL`, and is an
#' atomic vector. Errors are thrown for invalid inputs.
#'
#' @param x An input expected to be an atomic vector.
#' @param arg Name of the argument, used in error messages.
#'
#' @return Invisibly returns `TRUE` if validation succeeds.
#'
#' @noRd
.scholidonline_check_x <- function(
        x,
        arg
) {
    if (missing(x)) {
        stop("`", arg, "` is required.", call. = FALSE)
    }

    if (is.null(x)) {
        stop("`", arg, "` must not be NULL.", call. = FALSE)
    }

    if (is.data.frame(x)) {
        stop("`", arg, "` must not be a data frame.", call. = FALSE)
    }

    if (!is.atomic(x)) {
        cls <- paste(class(x), collapse = "/")
        stop(
            "`", arg, "` must be an atomic vector, not ",
            cls, ".",
            call. = FALSE
        )
    }

    invisible(TRUE)
}


#' Lookup allowed providers for a conversion
#' @noRd
.scholidonline_conversion_providers <- function(
        from,
        to
        ) {
    key <- paste(from, to, sep = "_to_")
    switch(
        key,
        pmid_to_doi   = c("auto", "ncbi", "epmc"),
        pmcid_to_pmid = c("auto", "ncbi"),
        doi_to_pmid   = c("auto", "epmc"),
        # default
        stop(
            "Unsupported conversion: ", from, " -> ", to, ".",
            call. = FALSE
        )
    )
}


#' Match and validate scholidonline identifier type
#'
#' @description
#' Internal helper that validates a user-supplied identifier type against
#' the set of types supported by \code{scholidonline_types()}.
#'
#' Ensures that \code{type} is a single, non-empty character string and
#' corresponds to a supported online identifier type. If validation fails,
#' a descriptive error is raised.
#'
#' This function centralizes type validation logic for exported helpers
#' such as \code{id_exists()}, \code{id_convert()},
#' \code{id_metadata()}, and \code{id_links()}.
#'
#' @param type A candidate identifier type.
#' @param arg A single string giving the argument name to use in error
#'   messages (e.g., \code{"type"}, \code{"from"}, \code{"to"}).
#'
#' @return A validated identifier type string.
#'
#' @noRd
.scholidonline_match_type <- function(
        type,
        arg = "type"
) {
    type_chr <- .scholidonline_as_scalar_character(
        x   = type,
        arg = arg
    )
    if (is.na(type_chr)) {
        stop("`", arg, "` must be a non-empty string.", call. = FALSE)
    }

    choices <- scholidonline_types()
    out <- match.arg(
        type_chr,
        choices = choices,
        several.ok = FALSE
    )

    if (!identical(type_chr, out)) {
        stop(
            "`", arg, "` must match exactly; abbreviations are not allowed.",
            call. = FALSE
        )
    }

    out
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
