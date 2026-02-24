#' Check whether scholarly identifiers exist
#'
#' @description
#' Check whether identifiers resolve or are found in their respective
#' registries.
#'
#' `id_exists()` is vectorized over `x`. If `type` is `NULL`, the identifier
#' type is inferred per element using `scholid::detect_scholid_type()` after
#' normalization (where possible). Inputs that cannot be classified or
#' normalized yield `NA`.
#'
#' Provider-/ID-specific logic lives in internal helpers named
#' `exists_<type>()` (e.g., `exists_doi()`), which are dispatched to from this
#' front-door function.
#'
#' @param x A character vector of identifiers.
#' @param type A single string giving the identifier type, or `NULL` to infer
#'   per element. See `scholid::scholid_types()` for supported values.
#' @param provider Provider to use (e.g. "auto", "doi.org", "crossref",
#'   "ncbi", "epmc", "orcid", "arxiv").
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A logical vector. `TRUE` if the identifier exists, `FALSE` if it is
#'   confirmed not found, and `NA` if the input cannot be classified or
#'   normalized.
#'
#' @examples
#' \dontrun{
#' id_exists("10.1000/182", type = "doi")
#' id_exists(c("12345678", "PMC12345"))  # infer type
#' }
#'
#' @export
id_exists <- function(
        x,
        type = NULL,
        provider = "auto",
        ...,
        quiet = FALSE
) {
    .scholid_online_check_x(x, arg = "x")

    x <- as.character(x)
    out <- rep(NA, length(x))

    # If type is inferred per element
    if (is.null(type)) {

        type_vec <- scholid::detect_scholid_type(x)

        for (i in seq_along(x)) {

            if (is.na(x[i]) || is.na(type_vec[i])) {
                next
            }

            xi <- scholid::normalize_scholid(x[i], type = type_vec[i])
            if (is.na(xi)) {
                next
            }

            fun_name <- paste0("exists_", type_vec[i])
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

    # Single declared type for all elements
    type <- .scholid_online_match_type(type, arg = "type")

    x_norm <- scholid::normalize_scholid(x, type = type)

    fun_name <- paste0("exists_", type)
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
