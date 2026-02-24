#' Convert scholarly identifiers across systems
#'
#' @description
#' Convert identifiers across registries (crosswalk), e.g. PMID -> DOI.
#'
#' `id_convert()` is vectorized over `x`. If `from` is `NULL`, the identifier
#' type is inferred per element using `scholid::detect_scholid_type()` after
#' normalizing (where possible). Inputs that cannot be classified or normalized
#' yield `NA_character_`.
#'
#' Provider-/ID-specific logic lives in internal helpers named
#' `convert_<from>_to_<to>()` (e.g., `convert_pmid_to_doi()`), which are
#' dispatched to from this front-door function.
#'
#' @param x A character vector of identifiers.
#' @param to A single string giving the target identifier type. See
#'   `scholid::scholid_types()` for supported values.
#' @param from A single string giving the source identifier type, or `NULL` to
#'   infer per element.
#' @param provider Provider to use (e.g. "auto", "ncbi", "epmc", ...).
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A character vector of converted identifiers. Unconvertible or
#'   unclassified inputs yield `NA_character_`.
#'
#' @examples
#' \dontrun{
#' id_convert("12345678", to = "doi", from = "pmid")
#' id_convert(c("10.1000/182", "PMC12345"), to = "pmid") # infer `from`
#' }
#'
#' @export
id_convert <- function(
        x,
        to,
        from = NULL,
        provider = "auto",
        ...,
        quiet = FALSE
) {
    .scholid_online_check_x(x, arg = "x")

    to <- .scholid_online_match_type(to, arg = "to")

    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    # Determine source types per element (if needed)
    if (is.null(from)) {
        # Normalize with best-effort inference:
        # detect type first, then normalize per detected type.
        from_vec <- scholid::detect_scholid_type(x)

        # If detect fails, keep NA and skip conversion
        for (i in seq_along(x)) {
            if (is.na(x[i]) || is.na(from_vec[i])) {
                next
            }

            xi <- scholid::normalize_scholid(x[i], type = from_vec[i])
            if (is.na(xi)) {
                next
            }

            fun_name <- paste0("convert_", from_vec[i], "_to_", to)
            fun <- get0(fun_name, mode = "function", inherits = TRUE)

            # nocov start
            if (is.null(fun)) {
                stop("Missing implementation: ", fun_name, "().", call. = FALSE)
            }
            # nocov end

            out[i] <- fun(
                x = xi,
                provider = provider,
                ...,
                quiet = quiet
            )
        }

        return(out)
    }

    # Single declared `from` for all elements
    from <- .scholid_online_match_type(from, arg = "from")

    # Normalize all inputs to canonical form for `from`
    x_norm <- scholid::normalize_scholid(x, type = from)

    fun_name <- paste0("convert_", from, "_to_", to)
    fun <- get0(fun_name, mode = "function", inherits = TRUE)

    # nocov start
    if (is.null(fun)) {
        stop("Missing implementation: ", fun_name, "().", call. = FALSE)
    }
    # nocov end

    for (i in seq_along(x_norm)) {
        if (is.na(x_norm[i])) {
            next
        }
        out[i] <- fun(
            x = x_norm[i],
            provider = provider,
            ...,
            quiet = quiet
        )
    }

    out
}


# Level 1 functions (functions called by exported functions) definitions -------


#' Validate input vector
#' @noRd
.scholid_online_check_x <- function(
        x,
        arg
) {
    if (missing(x)) {
        stop("Missing argument: ", arg, ".", call. = FALSE)
    }
    if (!is.atomic(x)) {
        stop("`", arg, "` must be an atomic vector.", call. = FALSE)
    }
    invisible(TRUE)
}


#' Match/validate identifier type
#' @noRd
.scholid_online_match_type <- function(
        type,
        arg = "type"
) {
    type <- as.character(type)
    if (length(type) != 1L || is.na(type) || !nzchar(type)) {
        stop("`", arg, "` must be a single, non-empty string.", call. = FALSE)
    }

    types <- scholid::scholid_types()
    if (!(type %in% types)) {
        stop(
            "Unknown `", arg, "`: ", type, ". Supported types are: ",
            paste(types, collapse = ", "),
            ".",
            call. = FALSE
        )
    }

    type
}
