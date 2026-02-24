#' Return linked scholarly identifiers
#'
#' @description
#' Return “all linked identifiers we can find” for each input identifier.
#'
#' `id_links()` is vectorized over `x` and returns a long data.frame with one
#' row per (input, linked_type, linked_value).
#'
#' If `type` is `NULL`, the identifier type is inferred per element using
#' `scholid::detect_scholid_type()` after normalization (where possible).
#' Inputs that cannot be classified or normalized yield zero rows.
#'
#' Provider-/ID-specific logic lives in internal helpers named `links_<type>()`
#' (e.g., `links_pmid()`), which are dispatched to from this front-door
#' function.
#'
#' @param x A character vector of identifiers.
#' @param type A single string giving the identifier type, or `NULL` to infer
#'   per element. See `scholid::scholid_types()` for supported values.
#' @param provider Provider to use (e.g. "auto", "crossref", "ncbi", "epmc",
#'   "orcid", "arxiv").
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A data.frame with columns:
#'   `input`, `input_type`, `linked_type`, `linked_value`, `provider`.
#'
#' @examples
#' \dontrun{
#' id_links("10.1000/182", type = "doi")
#' id_links(c("12345678", "PMC12345"))  # infer type
#' }
#'
#' @export
id_links <- function(
        x,
        type = NULL,
        provider = "auto",
        ...,
        quiet = FALSE
) {
    .scholid_online_check_x(x, arg = "x")

    x <- as.character(x)

    # infer type per element
    if (is.null(type)) {

        type_vec <- scholid::detect_scholid_type(x)
        out_list <- vector("list", length(x))

        for (i in seq_along(x)) {

            if (is.na(x[i]) || is.na(type_vec[i])) {
                out_list[[i]] <- .links_empty()
                next
            }

            xi <- scholid::normalize_scholid(x[i], type = type_vec[i])
            if (is.na(xi)) {
                out_list[[i]] <- .links_empty()
                next
            }

            fun_name <- paste0("links_", type_vec[i])
            fun <- get0(fun_name, mode = "function", inherits = TRUE)

            # nocov start
            if (is.null(fun)) {
                stop("Missing implementation: ", fun_name, "().", call. = FALSE)
            }
            # nocov end

            df <- fun(
                x = xi,
                provider = provider,
                ...,
                quiet = quiet
            )

            out_list[[i]] <- .links_wrap_output(
                input      = xi,
                input_type = type_vec[i],
                df         = df
            )
        }

        return(.links_rbind(out_list))
    }

    # single declared type for all elements
    type <- .scholid_online_match_type(type, arg = "type")

    x_norm <- scholid::normalize_scholid(x, type = type)
    out_list <- vector("list", length(x_norm))

    fun_name <- paste0("links_", type)
    fun <- get0(fun_name, mode = "function", inherits = TRUE)

    # nocov start
    if (is.null(fun)) {
        stop("Missing implementation: ", fun_name, "().", call. = FALSE)
    }
    # nocov end

    for (i in seq_along(x_norm)) {

        if (is.na(x_norm[i])) {
            out_list[[i]] <- .links_empty()
            next
        }

        df <- fun(
            x = x_norm[i],
            provider = provider,
            ...,
            quiet = quiet
        )

        out_list[[i]] <- .links_wrap_output(
            input      = x_norm[i],
            input_type = type,
            df         = df
        )
    }

    .links_rbind(out_list)
}


# ---- Level 1 helpers (dispatched by id_links) --------------------------------
# Each links_<type>() must return a data.frame with columns:
# linked_type, linked_value, provider
#
# It must accept a single normalized identifier `x`.

#' Links for DOI identifiers
#' @noRd
links_doi <- function(
        x,
        provider = "auto",
        ...,
        quiet = FALSE
) {
    provider <- .scholid_online_match_provider(
        provider,
        choices = c("auto", "crossref", "epmc")
    )

    if (identical(provider, "auto")) {
        # Prefer Crossref for DOI-centered linking, fallback to EPMC.
        out <- try(.links_doi_crossref(x, ... , quiet = quiet), silent = TRUE)
        if (!inherits(out, "try-error")) {
            return(out)
        }
        return(.links_doi_epmc(x, ... , quiet = quiet))
    }

    if (identical(provider, "crossref")) {
        return(.links_doi_crossref(x, ... , quiet = quiet))
    }

    if (identical(provider, "epmc")) {
        return(.links_doi_epmc(x, ... , quiet = quiet))
    }

    # nocov start
    stop("Unsupported provider: ", provider, ".", call. = FALSE)
    # nocov end
}

#' Links for PubMed identifiers (PMID)
#' @noRd
links_pmid <- function(
        x,
        provider = "auto",
        ...,
        quiet = FALSE
) {
    provider <- .scholid_online_match_provider(
        provider,
        choices = c("auto", "ncbi", "epmc")
    )

    if (identical(provider, "auto")) {
        out <- try(.links_pmid_ncbi(x, ... , quiet = quiet), silent = TRUE)
        if (!inherits(out, "try-error")) {
            return(out)
        }
        return(.links_pmid_epmc(x, ... , quiet = quiet))
    }

    if (identical(provider, "ncbi")) {
        return(.links_pmid_ncbi(x, ... , quiet = quiet))
    }

    if (identical(provider, "epmc")) {
        return(.links_pmid_epmc(x, ... , quiet = quiet))
    }

    # nocov start
    stop("Unsupported provider: ", provider, ".", call. = FALSE)
    # nocov end
}

#' Links for PubMed Central identifiers (PMCID)
#' @noRd
links_pmcid <- function(
        x,
        provider = "auto",
        ...,
        quiet = FALSE
) {
    provider <- .scholid_online_match_provider(
        provider,
        choices = c("auto", "ncbi", "epmc")
    )

    if (identical(provider, "auto")) {
        out <- try(.links_pmcid_ncbi(x, ... , quiet = quiet), silent = TRUE)
        if (!inherits(out, "try-error")) {
            return(out)
        }
        return(.links_pmcid_epmc(x, ... , quiet = quiet))
    }

    if (identical(provider, "ncbi")) {
        return(.links_pmcid_ncbi(x, ... , quiet = quiet))
    }

    if (identical(provider, "epmc")) {
        return(.links_pmcid_epmc(x, ... , quiet = quiet))
    }

    # nocov start
    stop("Unsupported provider: ", provider, ".", call. = FALSE)
    # nocov end
}

#' Links for arXiv identifiers
#' @noRd
links_arxiv <- function(
        x,
        provider = "auto",
        ...,
        quiet = FALSE
) {
    provider <- .scholid_online_match_provider(
        provider,
        choices = c("auto", "arxiv")
    )

    if (identical(provider, "auto") || identical(provider, "arxiv")) {
        return(.links_arxiv_arxiv(x, ... , quiet = quiet))
    }

    # nocov start
    stop("Unsupported provider: ", provider, ".", call. = FALSE)
    # nocov end
}

#' Links for ORCID identifiers
#' @noRd
links_orcid <- function(
        x,
        provider = "auto",
        ...,
        quiet = FALSE
) {
    provider <- .scholid_online_match_provider(
        provider,
        choices = c("auto", "orcid")
    )

    if (identical(provider, "auto") || identical(provider, "orcid")) {
        return(.links_orcid_orcid(x, ... , quiet = quiet))
    }

    # nocov start
    stop("Unsupported provider: ", provider, ".", call. = FALSE)
    # nocov end
}


# ---- Level 2 provider implementations (stubs; fill with httr2 later) ----------

# DOI -> (PMID/PMCID/...) via Crossref
#' @noRd
.links_doi_crossref <- function(x, ... , quiet = FALSE) {
    # Implement with:
    # - httr2::request("https://api.crossref.org/works/<doi>")
    # - parse "alternative-id", "relation", etc. as available
    stop("Not implemented: .links_doi_crossref().", call. = FALSE)
}

# DOI -> (PMID/PMCID/...) via Europe PMC
#' @noRd
.links_doi_epmc <- function(x, ... , quiet = FALSE) {
    # Implement with:
    # - Europe PMC search by DOI
    # - parse pmid/pmcid fields if present
    stop("Not implemented: .links_doi_epmc().", call. = FALSE)
}

# PMID -> (DOI/PMCID/...) via NCBI
#' @noRd
.links_pmid_ncbi <- function(x, ... , quiet = FALSE) {
    # Implement with:
    # - E-utilities esummary/efetch/elink
    # - parse DOI, PMCID, etc.
    stop("Not implemented: .links_pmid_ncbi().", call. = FALSE)
}

# PMID -> (DOI/PMCID/...) via Europe PMC
#' @noRd
.links_pmid_epmc <- function(x, ... , quiet = FALSE) {
    stop("Not implemented: .links_pmid_epmc().", call. = FALSE)
}

# PMCID -> (PMID/DOI/...) via NCBI
#' @noRd
.links_pmcid_ncbi <- function(x, ... , quiet = FALSE) {
    stop("Not implemented: .links_pmcid_ncbi().", call. = FALSE)
}

# PMCID -> (PMID/DOI/...) via Europe PMC
#' @noRd
.links_pmcid_epmc <- function(x, ... , quiet = FALSE) {
    stop("Not implemented: .links_pmcid_epmc().", call. = FALSE)
}

# arXiv -> DOI (when present) via arXiv
#' @noRd
.links_arxiv_arxiv <- function(x, ... , quiet = FALSE) {
    stop("Not implemented: .links_arxiv_arxiv().", call. = FALSE)
}

# ORCID -> DOIs for works via ORCID
#' @noRd
.links_orcid_orcid <- function(x, ... , quiet = FALSE) {
    stop("Not implemented: .links_orcid_orcid().", call. = FALSE)
}


# ---- Utilities used by id_links ---------------------------------------------

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

#' Standard empty return
#' @noRd
.links_empty <- function() {
    data.frame(
        linked_type  = character(0),
        linked_value = character(0),
        provider     = character(0),
        stringsAsFactors = FALSE
    )
}

#' Wrap provider output with required front-door columns
#' @noRd
.links_wrap_output <- function(input, input_type, df) {
    if (is.null(df)) {
        df <- .links_empty()
    }

    df <- as.data.frame(df, stringsAsFactors = FALSE)

    # Defensive checks (kept simple; you can harden later)
    if (!all(c("linked_type", "linked_value", "provider") %in% names(df))) {
        stop(
            "Provider implementation must return columns: ",
            "linked_type, linked_value, provider.",
            call. = FALSE
        )
    }

    if (!nrow(df)) {
        return(data.frame(
            input        = character(0),
            input_type   = character(0),
            linked_type  = character(0),
            linked_value = character(0),
            provider     = character(0),
            stringsAsFactors = FALSE
        ))
    }

    data.frame(
        input        = rep(input, nrow(df)),
        input_type   = rep(input_type, nrow(df)),
        linked_type  = as.character(df[["linked_type"]]),
        linked_value = as.character(df[["linked_value"]]),
        provider     = as.character(df[["provider"]]),
        stringsAsFactors = FALSE
    )
}

#' Row-bind list of data.frames (base R, no deps)
#' @noRd
.links_rbind <- function(x) {
    if (!length(x)) {
        return(data.frame(
            input        = character(0),
            input_type   = character(0),
            linked_type  = character(0),
            linked_value = character(0),
            provider     = character(0),
            stringsAsFactors = FALSE
        ))
    }

    # Filter NULLs defensively
    x <- x[!vapply(x, is.null, logical(1))]

    if (!length(x)) {
        return(data.frame(
            input        = character(0),
            input_type   = character(0),
            linked_type  = character(0),
            linked_value = character(0),
            provider     = character(0),
            stringsAsFactors = FALSE
        ))
    }

    out <- do.call(rbind, x)
    rownames(out) <- NULL
    out
}
