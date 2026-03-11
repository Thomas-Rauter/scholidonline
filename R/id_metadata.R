#' Fetch metadata for scholarly identifiers
#'
#' @description
#' Fetch structured metadata for identifiers using online registries.
#'
#' `id_metadata()` is vectorized over `x` and returns a data.frame with one row
#' per input identifier. If `type` is `NULL`, the identifier type is inferred
#' per element using `scholid::detect_scholid_type()` after normalization (where
#' possible). Inputs that cannot be classified or normalized yield one row with
#' `NA` metadata fields.
#'
#' Provider-/ID-specific logic lives in internal helpers named
#' `metadata_<type>()` (e.g., `metadata_doi()`), which are dispatched to from
#' this front-door function.
#'
#' @param x A character vector of identifiers.
#' @param type A single string giving the identifier type, or `NULL` to infer
#'   per element. See `scholid::scholid_types()` for supported values.
#' @param provider Provider to use (e.g. "auto", "crossref", "doi.org", "ncbi",
#'   "epmc", "orcid", "arxiv").
#' @param fields Optional character vector of fields to return. If `NULL`,
#'   returns a stable default set.
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame with one row per input, containing at least:
#'   `input`, `type`, `provider`, `title`, `year`, `authors`, `container`,
#'   `doi`, `pmid`, `pmcid`, `url`.
#'
#' @examples
#' \dontrun{
#' id_metadata("10.1000/182", type = "doi")
#' id_metadata(c("12345678", "PMC12345"))  # infer type
#' }
#'
#' @export
id_metadata <- function(
        x,
        type = NULL,
        provider = "auto",
        fields = NULL,
        ...,
        quiet = FALSE
) {
    .scholid_online_check_x(x, arg = "x")

    x <- as.character(x)

    # stable default output fields (order matters)
    default_fields <- c(
        "input", "type", "provider",
        "title", "year", "authors", "container",
        "doi", "pmid", "pmcid", "url"
    )

    if (is.null(fields)) {
        fields <- default_fields
    } else {
        fields <- as.character(fields)
        if (!length(fields)) {
            stop(
                "`fields` must be NULL or a non-empty character vector.",
                call. = FALSE
                )
        }
        # ensure required keys for stable joins
        must_have <- c("input", "type", "provider")
        if (!all(must_have %in% fields)) {
            fields <- unique(c(must_have, fields))
        }
    }

    # infer per element
    if (is.null(type)) {

        type_vec <- scholid::detect_scholid_type(x)
        out_list <- vector("list", length(x))

        for (i in seq_along(x)) {

            if (is.na(x[i]) || is.na(type_vec[i])) {
                out_list[[i]] <- .metadata_row_empty(
                    input = x[i],
                    type = NA_character_,
                    provider = NA_character_,
                    fields = fields
                )
                next
            }

            xi <- scholid::normalize_scholid(x[i], type = type_vec[i])
            if (is.na(xi)) {
                out_list[[i]] <- .metadata_row_empty(
                    input = x[i],
                    type = type_vec[i],
                    provider = NA_character_,
                    fields = fields
                )
                next
            }

            fun_name <- paste0("metadata_", type_vec[i])
            fun <- get0(fun_name, mode = "function", inherits = TRUE)

            # nocov start
            if (is.null(fun)) {
                stop("Missing implementation: ", fun_name, "().", call. = FALSE)
            }
            # nocov end

            df <- fun(
                x = xi,
                provider = provider,
                fields = fields,
                ...,
                quiet = quiet
            )

            out_list[[i]] <- .metadata_wrap_output(
                input = xi,
                type = type_vec[i],
                df = df,
                fields = fields
            )
        }

        return(.metadata_rbind(out_list, fields = fields))
    }

    # single declared type for all elements
    type <- .scholid_online_match_type(type, arg = "type")
    x_norm <- scholid::normalize_scholid(x, type = type)

    fun_name <- paste0("metadata_", type)
    fun <- get0(fun_name, mode = "function", inherits = TRUE)

    # nocov start
    if (is.null(fun)) {
        stop("Missing implementation: ", fun_name, "().", call. = FALSE)
    }
    # nocov end

    out_list <- vector("list", length(x_norm))

    for (i in seq_along(x_norm)) {

        if (is.na(x_norm[i])) {
            out_list[[i]] <- .metadata_row_empty(
                input = x[i],
                type = type,
                provider = NA_character_,
                fields = fields
            )
            next
        }

        df <- fun(
            x = x_norm[i],
            provider = provider,
            fields = fields,
            ...,
            quiet = quiet
        )

        out_list[[i]] <- .metadata_wrap_output(
            input = x_norm[i],
            type = type,
            df = df,
            fields = fields
        )
    }

    .metadata_rbind(out_list, fields = fields)
}


# ---- Level 1 helpers (dispatched by id_metadata) -----------------------------
# Each metadata_<type>() must return a single-row data.frame with at least:
# provider, and any of the requested `fields` (besides input/type/provider which
# are filled at the front door if missing).
#
# It must accept a single normalized identifier `x`.

#' Metadata for DOI identifiers
#' @noRd
metadata_doi <- function(
        x,
        provider = "auto",
        fields = NULL,
        ...,
        quiet = FALSE
) {
    provider <- .scholid_online_match_provider(
        provider,
        choices = c("auto", "crossref", "doi.org")
    )

    if (identical(provider, "auto")) {
        # Prefer Crossref for structured metadata.
        out <- try(.metadata_doi_crossref(
            x,
            fields = fields,
            ... ,
            quiet = quiet
            ), silent = TRUE)
        if (!inherits(out, "try-error")) {
            return(out)
        }
        return(.metadata_doi_doi_org(x, fields = fields, ... , quiet = quiet))
    }

    if (identical(provider, "crossref")) {
        return(.metadata_doi_crossref(x, fields = fields, ... , quiet = quiet))
    }

    if (identical(provider, "doi.org")) {
        return(.metadata_doi_doi_org(x, fields = fields, ... , quiet = quiet))
    }

    # nocov start
    stop("Unsupported provider: ", provider, ".", call. = FALSE)
    # nocov end
}

#' Metadata for PubMed identifiers (PMID)
#' @noRd
metadata_pmid <- function(
        x,
        provider = "auto",
        fields = NULL,
        ...,
        quiet = FALSE
) {
    provider <- .scholid_online_match_provider(
        provider,
        choices = c("auto", "ncbi", "epmc")
    )

    if (identical(provider, "auto")) {
        out <- try(.metadata_pmid_ncbi(
            x,
            fields = fields,
            ... ,
            quiet = quiet
            ), silent = TRUE
            )
        if (!inherits(out, "try-error")) {
            return(out)
        }
        return(.metadata_pmid_epmc(x, fields = fields, ... , quiet = quiet))
    }

    if (identical(provider, "ncbi")) {
        return(.metadata_pmid_ncbi(x, fields = fields, ... , quiet = quiet))
    }

    if (identical(provider, "epmc")) {
        return(.metadata_pmid_epmc(x, fields = fields, ... , quiet = quiet))
    }

    # nocov start
    stop("Unsupported provider: ", provider, ".", call. = FALSE)
    # nocov end
}

#' Metadata for PubMed Central identifiers (PMCID)
#' @noRd
metadata_pmcid <- function(
        x,
        provider = "auto",
        fields = NULL,
        ...,
        quiet = FALSE
) {
    provider <- .scholid_online_match_provider(
        provider,
        choices = c("auto", "ncbi", "epmc")
    )

    if (identical(provider, "auto")) {
        out <- try(.metadata_pmcid_ncbi(
            x,
            fields = fields,
            ... ,
            quiet = quiet
            ), silent = TRUE)
        if (!inherits(out, "try-error")) {
            return(out)
        }
        return(.metadata_pmcid_epmc(x, fields = fields, ... , quiet = quiet))
    }

    if (identical(provider, "ncbi")) {
        return(.metadata_pmcid_ncbi(x, fields = fields, ... , quiet = quiet))
    }

    if (identical(provider, "epmc")) {
        return(.metadata_pmcid_epmc(x, fields = fields, ... , quiet = quiet))
    }

    # nocov start
    stop("Unsupported provider: ", provider, ".", call. = FALSE)
    # nocov end
}

#' Metadata for arXiv identifiers
#' @noRd
metadata_arxiv <- function(
        x,
        provider = "auto",
        fields = NULL,
        ...,
        quiet = FALSE
) {
    provider <- .scholid_online_match_provider(
        provider,
        choices = c("auto", "arxiv")
    )

    if (identical(provider, "auto") || identical(provider, "arxiv")) {
        return(.metadata_arxiv_arxiv(x, fields = fields, ... , quiet = quiet))
    }

    # nocov start
    stop("Unsupported provider: ", provider, ".", call. = FALSE)
    # nocov end
}

#' Metadata for ORCID identifiers
#' @noRd
metadata_orcid <- function(
        x,
        provider = "auto",
        fields = NULL,
        ...,
        quiet = FALSE
) {
    provider <- .scholid_online_match_provider(
        provider,
        choices = c("auto", "orcid")
    )

    if (identical(provider, "auto") || identical(provider, "orcid")) {
        return(.metadata_orcid_orcid(x, fields = fields, ... , quiet = quiet))
    }

    # nocov start
    stop("Unsupported provider: ", provider, ".", call. = FALSE)
    # nocov end
}


# Level 2 provider implementations (stubs; fill with httr2 later) --------------

# DOI metadata via Crossref works endpoint
#' @noRd
.metadata_doi_crossref <- function(x, fields = NULL, ... , quiet = FALSE) {
    stop("Not implemented: .metadata_doi_crossref().", call. = FALSE)
}

# DOI metadata via doi.org content negotiation (e.g., CSL JSON)
#' @noRd
.metadata_doi_doi_org <- function(x, fields = NULL, ... , quiet = FALSE) {
    stop("Not implemented: .metadata_doi_doi_org().", call. = FALSE)
}

# PMID metadata via NCBI E-utilities
#' @noRd
.metadata_pmid_ncbi <- function(x, fields = NULL, ... , quiet = FALSE) {
    stop("Not implemented: .metadata_pmid_ncbi().", call. = FALSE)
}

# PMID metadata via Europe PMC
#' @noRd
.metadata_pmid_epmc <- function(x, fields = NULL, ... , quiet = FALSE) {
    stop("Not implemented: .metadata_pmid_epmc().", call. = FALSE)
}

# PMCID metadata via NCBI E-utilities
#' @noRd
.metadata_pmcid_ncbi <- function(x, fields = NULL, ... , quiet = FALSE) {
    stop("Not implemented: .metadata_pmcid_ncbi().", call. = FALSE)
}

# PMCID metadata via Europe PMC
#' @noRd
.metadata_pmcid_epmc <- function(x, fields = NULL, ... , quiet = FALSE) {
    stop("Not implemented: .metadata_pmcid_epmc().", call. = FALSE)
}

# arXiv metadata via arXiv Atom API
#' @noRd
.metadata_arxiv_arxiv <- function(x, fields = NULL, ... , quiet = FALSE) {
    stop("Not implemented: .metadata_arxiv_arxiv().", call. = FALSE)
}

# ORCID record metadata via ORCID public API
#' @noRd
.metadata_orcid_orcid <- function(x, fields = NULL, ... , quiet = FALSE) {
    stop("Not implemented: .metadata_orcid_orcid().", call. = FALSE)
}


# ---- Utilities used by id_metadata ------------------------------------------

#' Validate input vector
#' @noRd
.scholid_online_check_x <- function(x, arg) {
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
.scholid_online_match_type <- function(type, arg = "type") {
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

#' Match/validate provider
#' @noRd
.scholid_online_match_provider <- function(provider, choices) {
    provider <- as.character(provider)
    if (length(provider) != 1L || is.na(provider) || !nzchar(provider)) {
        stop("`provider` must be a single, non-empty string.", call. = FALSE)
    }
    if (!(provider %in% choices)) {
        stop(
            "Unknown `provider`: ", provider, ". Supported providers are: ",
            paste(choices, collapse = ", "),
            ".",
            call. = FALSE
        )
    }
    provider
}

#' Standard empty metadata row (one-row data.frame)
#' @noRd
.metadata_row_empty <- function(input, type, provider, fields) {
    # Start with all fields as NA_character_
    out <- as.list(rep(NA_character_, length(fields)))
    names(out) <- fields

    # Always fill the keys if present in requested fields
    if ("input" %in% fields)    out[["input"]]    <- input
    if ("type" %in% fields)     out[["type"]]     <- type
    if ("provider" %in% fields) out[["provider"]] <- provider

    as.data.frame(out, stringsAsFactors = FALSE)
}

#' Wrap provider output to one stable row with required keys
#' @noRd
.metadata_wrap_output <- function(input, type, df, fields) {
    if (is.null(df) || !nrow(df)) {
        return(.metadata_row_empty(
            input = input,
            type = type,
            provider = NA_character_,
            fields = fields
        ))
    }

    df <- as.data.frame(df, stringsAsFactors = FALSE)

    # Provider output should be single-row
    if (nrow(df) != 1L) {
        stop("Provider implementation must return exactly one row.", call. = FALSE)
    }

    # Ensure keys exist
    if (!("provider" %in% names(df))) {
        df[["provider"]] <- NA_character_
    }

    # Construct stable row in requested field order
    out <- as.list(rep(NA_character_, length(fields)))
    names(out) <- fields

    # Fill keys
    if ("input" %in% fields) out[["input"]] <- input
    if ("type" %in% fields) out[["type"]] <- type
    if ("provider" %in% fields) out[["provider"]] <- as.character(df[["provider"]])

    # Fill the rest from provider output when present
    for (nm in setdiff(fields, c("input", "type", "provider"))) {
        if (nm %in% names(df)) {
            val <- df[[nm]]
            # coerce to character for stable schema; provider can choose otherwise later
            out[[nm]] <- if (length(val) && !is.na(val)) as.character(val) else NA_character_
        }
    }

    as.data.frame(out, stringsAsFactors = FALSE)
}

#' Row-bind list of one-row data.frames (base R, no deps)
#' @noRd
.metadata_rbind <- function(x, fields) {
    if (!length(x)) {
        return(.metadata_row_empty(
            input = character(0),
            type = character(0),
            provider = character(0),
            fields = fields
        )[0, , drop = FALSE])
    }

    x <- x[!vapply(x, is.null, logical(1))]

    if (!length(x)) {
        return(.metadata_row_empty(
            input = character(0),
            type = character(0),
            provider = character(0),
            fields = fields
        )[0, , drop = FALSE])
    }

    out <- do.call(rbind, x)
    rownames(out) <- NULL
    out
}
