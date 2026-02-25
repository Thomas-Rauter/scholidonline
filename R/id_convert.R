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
#'   `scholidonline::scholidonline_types()` for supported values.
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
    .scholidonline_check_x(
        x,
        arg = "x"
    )
    to <- .scholidonline_match_type(
        to,
        arg = "to"
    )
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    # Determine source types per element (if needed)
    if (is.null(from)) {
        from_vec <- scholid::detect_scholid_type(x)
        for (i in seq_along(x)) {
            if (is.na(x[i]) || is.na(from_vec[i])) {
                next
            }
            xi <- scholid::normalize_scholid(
                x[i],
                type = from_vec[i]
            )
            if (is.na(xi)) {
                next
            }
            if (identical(from_vec[i], to)) {
                out[i] <- xi
                next
            }
            fun_name <- paste0(
                ".convert_",
                from_vec[i],
                "_to_",
                to
            )
            fun <- get0(
                fun_name,
                mode = "function",
                inherits = TRUE
            )

            # nocov start
            if (is.null(fun)) {
                stop(
                    "Missing implementation: ",
                    fun_name,
                    "().",
                    call. = FALSE
                )
            }
            # nocov end

            choices <- .scholidonline_conversion_providers(
                from = from_vec[i],
                to   = to
            )
            provider_i <- .scholidonline_match_provider(
                provider = provider,
                choices  = choices
            )
            out[i] <- fun(
                x = xi,
                provider = provider_i,
                ...,
                quiet = quiet
            )
        }
        return(out)
    }

    # Single declared `from` for all elements
    from <- .scholidonline_match_type(
        from,
        arg = "from"
    )

    choices <- .scholidonline_conversion_providers(
        from = from,
        to   = to
    )

    provider <- .scholidonline_match_provider(
        provider = provider,
        choices  = choices
    )

    x_norm <- scholid::normalize_scholid(
        x,
        type = from
    )
    if (identical(from, to)) {
        return(x_norm)
    }

    fun_name <- paste0(
        ".convert_",
        from,
        "_to_",
        to
    )

    fun <- get0(
        fun_name,
        mode = "function",
        inherits = TRUE
    )

    # nocov start
    if (is.null(fun)) {
        stop(
            "Missing implementation: ",
            fun_name,
            "().",
            call. = FALSE
        )
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


#' Match and validate a conversion provider
#'
#' Internal helper used by `id_convert()` to validate a user-supplied provider
#' against a set of providers supported for a given conversion. If `provider`
#' is `"auto"`, the first element of `choices` is selected.
#'
#' @param provider A single provider string (e.g. "auto", "ncbi", "epmc").
#' @param choices A character vector of valid providers for this conversion.
#'
#' @return A single provider string.
#'
#' @noRd
.scholidonline_match_provider <- function(
        provider,
        choices
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
    choices <- choices[choices != "auto"]

    if (length(choices) < 1L) {
        stop("No concrete providers available.", call. = FALSE)
    }

    if (identical(provider, "auto")) {
        return(choices[[1L]])
    }

    if (!provider %in% choices) {
        stop(
            "Unknown provider: `", provider, "`. Must be one of: ",
            paste0("`", choices, "`", collapse = ", "),
            call. = FALSE
        )
    }

    provider
}


#' Convert a PMID to a DOI
#'
#' Provider-specific implementations live in helpers named
#' `convert_pmid_to_doi_<provider>()` (e.g., `convert_pmid_to_doi_ncbi()`).
#'
#' @param x A single, normalized PMID string.
#' @param provider A single provider string (e.g. "ncbi", "epmc", "mock").
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single DOI string, or `NA_character_` if unconvertible.
#'
#' @noRd
.convert_pmid_to_doi <- function(x, provider, ..., quiet = FALSE) {
    stopifnot(is.character(x), length(x) == 1L)

    switch(
        provider,
        ncbi = .convert_pmid_to_doi_ncbi(x = x, ..., quiet = quiet),
        epmc = .convert_pmid_to_doi_epmc(x = x, ..., quiet = quiet),
        mock = .convert_pmid_to_doi_mock(x = x, ..., quiet = quiet),
        stop("Unknown provider: ", provider, call. = FALSE)
    )
}


#' Convert a DOI to a PMID
#'
#' Provider-specific implementations live in helpers named
#' `convert_doi_to_pmid_<provider>()` (e.g., `convert_doi_to_pmid_ncbi()`).
#'
#' @param x A single, normalized DOI string.
#' @param provider A single provider string (e.g. "ncbi", "epmc", "mock").
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single PMID string, or `NA_character_` if unconvertible.
#'
#' @noRd
.convert_doi_to_pmid <- function(x, provider, ..., quiet = FALSE) {
    stopifnot(is.character(x), length(x) == 1L)

    switch(
        provider,
        ncbi = .convert_doi_to_pmid_ncbi(x = x, ..., quiet = quiet),
        epmc = .convert_doi_to_pmid_epmc(x = x, ..., quiet = quiet),
        mock = .convert_doi_to_pmid_mock(x = x, ..., quiet = quiet),
        stop("Unknown provider: ", provider, call. = FALSE)
    )
}


#' Convert a PMCID to a PMID
#'
#' Provider-specific implementations live in helpers named
#' `convert_pmcid_to_pmid_<provider>()` (e.g., `convert_pmcid_to_pmid_ncbi()`).
#'
#' @param x A single, normalized PMCID string.
#' @param provider A single provider string (e.g. "ncbi", "epmc", "mock").
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single PMID string, or `NA_character_` if unconvertible.
#'
#' @noRd
.convert_pmcid_to_pmid <- function(x, provider, ..., quiet = FALSE) {
    stopifnot(is.character(x), length(x) == 1L)

    switch(
        provider,
        ncbi = .convert_pmcid_to_pmid_ncbi(x = x, ..., quiet = quiet),
        epmc = .convert_pmcid_to_pmid_epmc(x = x, ..., quiet = quiet),
        mock = .convert_pmcid_to_pmid_mock(x = x, ..., quiet = quiet),
        stop("Unknown provider: ", provider, call. = FALSE)
    )
}


#' Convert a PMCID to a DOI
#'
#' Provider-specific implementations live in helpers named
#' `convert_pmcid_to_doi_<provider>()` (e.g., `convert_pmcid_to_doi_epmc()`).
#'
#' @param x A single, normalized PMCID string.
#' @param provider A single provider string (e.g. "ncbi", "epmc", "mock").
#' @param ... Passed to provider-specific implementations.
#' @param quiet Logical; if `TRUE`, suppress provider warnings/messages where
#'   possible.
#'
#' @return A single DOI string, or `NA_character_` if unconvertible.
#'
#' @noRd
.convert_pmcid_to_doi <- function(x, provider, ..., quiet = FALSE) {
    stopifnot(is.character(x), length(x) == 1L)

    switch(
        provider,
        ncbi = .convert_pmcid_to_doi_ncbi(x = x, ..., quiet = quiet),
        epmc = .convert_pmcid_to_doi_epmc(x = x, ..., quiet = quiet),
        mock = .convert_pmcid_to_doi_mock(x = x, ..., quiet = quiet),
        stop("Unknown provider: ", provider, call. = FALSE)
    )
}


# Level 2 functions (functions called by level 1 functions) definitions --------


#' NCBI: PMID -> DOI
#'
#' @param x A single PMID string.
#' @param ... Passed to NCBI E-utilities (e.g. `api_key`, `tool`, `email`).
#' @param quiet Logical.
#'
#' @return A single DOI string or `NA_character_`.
#'
#' @noRd
.convert_pmid_to_doi_ncbi <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    js <- .scholidonline_esummary_pubmed(id = x, ..., quiet = quiet)
    if (is.null(js) || is.null(js$result) || is.null(js$result[[x]])) {
        return(NA_character_)
    }

    .scholidonline_extract_doi_from_esummary(js, pmid = x)
}


#' Europe PMC: PMID -> DOI
#'
#' @param x A single PMID string.
#' @param ... Passed to Europe PMC search (e.g. `pageSize`).
#' @param quiet Logical.
#'
#' @return A single DOI string or `NA_character_`.
#'
#' @noRd
.convert_pmid_to_doi_epmc <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    q <- paste0("EXT_ID:", x, " AND SRC:MED")
    js <- .scholidonline_epmc_search(query = q, ..., quiet = quiet)

    if (is.null(js)) {
        return(NA_character_)
    }

    rec <- .scholidonline_epmc_first_result(js)
    doi <- rec$doi %||% NA_character_

    if (is.na(doi) || !nzchar(doi)) {
        return(NA_character_)
    }

    as.character(doi)
}


#' MOCK: PMID -> DOI
#'
#' @param x A single PMID string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single DOI string.
#'
#' @noRd
.convert_pmid_to_doi_mock <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)
    paste0("10.1234/mockdoi.", x)
}


#' NCBI: DOI -> PMID
#'
#' @param x A single DOI string.
#' @param ... Passed to NCBI E-utilities (e.g. `api_key`, `tool`, `email`).
#' @param quiet Logical.
#'
#' @return A single PMID string or `NA_character_`.
#'
#' @noRd
.convert_doi_to_pmid_ncbi <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    term <- paste0("\"", x, "\"[DOI]")
    js <- .scholidonline_esearch_pubmed(term = term, ..., quiet = quiet)

    ids <- js$esearchresult$idlist
    if (is.null(ids) || length(ids) < 1L) {
        return(NA_character_)
    }

    as.character(ids[[1]])
}


#' Europe PMC: DOI -> PMID
#'
#' @param x A single DOI string.
#' @param ... Passed to Europe PMC search (e.g. `pageSize`).
#' @param quiet Logical.
#'
#' @return A single PMID string or `NA_character_`.
#'
#' @noRd
.convert_doi_to_pmid_epmc <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    q <- paste0("DOI:\"", x, "\"")
    js <- .scholidonline_epmc_search(query = q, ..., quiet = quiet)

    if (is.null(js)) {
        return(NA_character_)
    }

    rec <- .scholidonline_epmc_first_result(js)
    pmid <- rec$pmid %||% NA_character_

    if (is.na(pmid) || !nzchar(pmid)) {
        return(NA_character_)
    }

    as.character(pmid)
}


#' MOCK: DOI -> PMID
#'
#' @param x A single DOI string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single PMID string.
#'
#' @noRd
.convert_doi_to_pmid_mock <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    digits <- gsub("[^0-9]", "", x)

    if (!nzchar(digits)) {
        return(NA_character_)
    }

    substr(digits, 1L, 8L)
}


#' NCBI: PMCID -> PMID
#'
#' @param x A single PMCID string.
#' @param ... Passed to PMC ID Converter (e.g. `tool`, `email`).
#' @param quiet Logical.
#'
#' @return A single PMID string or `NA_character_`.
#'
#' @noRd
.convert_pmcid_to_pmid_ncbi <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    js <- .scholidonline_pmc_idconv(ids = x, ..., quiet = quiet)
    if (is.null(js)) {
        return(NA_character_)
    }

    .scholidonline_extract_idconv(js, field = "pmid")
}


#' Europe PMC: PMCID -> PMID
#'
#' @param x A single PMCID string.
#' @param ... Passed to Europe PMC search (e.g. `pageSize`).
#' @param quiet Logical.
#'
#' @return A single PMID string or `NA_character_`.
#'
#' @noRd
.convert_pmcid_to_pmid_epmc <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    q <- paste0("PMCID:", x)
    js <- .scholidonline_epmc_search(query = q, ..., quiet = quiet)

    if (is.null(js)) {
        return(NA_character_)
    }

    rec <- .scholidonline_epmc_first_result(js)
    pmid <- rec$pmid %||% NA_character_

    if (is.na(pmid) || !nzchar(pmid)) {
        return(NA_character_)
    }

    as.character(pmid)
}


#' MOCK: PMCID -> PMID
#'
#' @param x A single PMCID string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single PMID string.
#'
#' @noRd
.convert_pmcid_to_pmid_mock <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    digits <- gsub("[^0-9]", "", x)

    if (!nzchar(digits)) {
        return(NA_character_)
    }

    substr(digits, 1L, 8L)
}


#' NCBI: PMCID -> DOI
#'
#' @param x A single PMCID string.
#' @param ... Passed to PMC ID Converter (e.g. `tool`, `email`).
#' @param quiet Logical.
#'
#' @return A single DOI string or `NA_character_`.
#'
#' @noRd
.convert_pmcid_to_doi_ncbi <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    js <- .scholidonline_pmc_idconv(ids = x, ..., quiet = quiet)
    if (is.null(js)) {
        return(NA_character_)
    }

    .scholidonline_extract_idconv(js, field = "doi")
}


#' Europe PMC: PMCID -> DOI
#'
#' @param x A single PMCID string.
#' @param ... Passed to Europe PMC search (e.g. `pageSize`).
#' @param quiet Logical.
#'
#' @return A single DOI string or `NA_character_`.
#'
#' @noRd
.convert_pmcid_to_doi_epmc <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    q <- paste0("PMCID:", x)
    js <- .scholidonline_epmc_search(query = q, ..., quiet = quiet)

    if (is.null(js)) {
        return(NA_character_)
    }

    rec <- .scholidonline_epmc_first_result(js)
    doi <- rec$doi %||% NA_character_

    if (is.na(doi) || !nzchar(doi)) {
        return(NA_character_)
    }

    as.character(doi)
}


#' MOCK: PMCID -> DOI
#'
#' @param x A single PMCID string.
#' @param ... Unused.
#' @param quiet Logical.
#'
#' @return A single DOI string.
#'
#' @noRd
.convert_pmcid_to_doi_mock <- function(x, ..., quiet = FALSE) {
    .scholidonline_check_scalar_chr(x)

    digits <- gsub("[^0-9]", "", x)

    if (!nzchar(digits)) {
        return(NA_character_)
    }

    paste0("10.1234/mockpmc.", digits)
}


# Level 3 functions (functions called by level 2 functions) definitions --------


.scholidonline_check_scalar_chr <- function(x, arg = "x") {
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
        stop(
            "`", arg, "` must be a single, non-missing character string.",
            call. = FALSE
        )
    }
    invisible(x)
}


.scholidonline_req_json <- function(url, query, quiet) {
    req <- httr2::request(url)
    req <- httr2::req_url_query(req, !!!query)
    req <- httr2::req_error(req, is_error = function(resp) FALSE)

    resp <- httr2::req_perform(req)

    if (httr2::resp_status(resp) >= 400) {
        if (!isTRUE(quiet)) {
            warning(
                "HTTP request failed (", httr2::resp_status(resp), "): ",
                url,
                call. = FALSE
            )
        }
        return(NULL)
    }

    txt <- httr2::resp_body_string(resp)
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
}


.scholidonline_first <- function(x) {
    if (is.null(x) || length(x) < 1L) {
        return(NULL)
    }
    x[[1]]
}


.scholidonline_epmc_search <- function(query, ..., quiet = FALSE) {
    dots <- list(...)
    page_size <- dots$pageSize %||% 1L
    format <- dots$format %||% "json"

    .scholidonline_req_json(
        url = "https://www.ebi.ac.uk/europepmc/webservices/rest/search",
        query = list(query = query, format = format, pageSize = page_size),
        quiet = quiet
    )
}


`%||%` <- function(x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}


.scholidonline_esearch_pubmed <- function(term, ..., quiet = FALSE) {
    dots <- list(...)

    .scholidonline_req_json(
        url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
        query = c(
            list(db = "pubmed", term = term, retmode = "json"),
            dots
        ),
        quiet = quiet
    )
}


.scholidonline_esummary_pubmed <- function(id, ..., quiet = FALSE) {
    dots <- list(...)

    .scholidonline_req_json(
        url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
        query = c(
            list(db = "pubmed", id = id, retmode = "json"),
            dots
        ),
        quiet = quiet
    )
}


.scholidonline_pmc_idconv <- function(ids, ..., quiet = FALSE) {
    dots <- list(...)

    .scholidonline_req_json(
        url = "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/",
        query = c(list(format = "json", ids = ids), dots),
        quiet = quiet
    )
}


.scholidonline_extract_doi_from_esummary <- function(x, pmid) {
    rec <- x$result[[pmid]]

    if (is.null(rec$articleids)) {
        return(NA_character_)
    }

    ids <- rec$articleids
    if (is.data.frame(ids) && "idtype" %in% names(ids)) {
        hit <- ids[ids$idtype == "doi", , drop = FALSE]
        if (nrow(hit) < 1L) {
            return(NA_character_)
        }
        return(as.character(hit$value[[1]]))
    }

    if (is.list(ids)) {
        for (i in seq_along(ids)) {
            if (isTRUE(ids[[i]]$idtype == "doi")) {
                return(as.character(ids[[i]]$value))
            }
        }
    }

    NA_character_
}


.scholidonline_extract_idconv <- function(x, field) {
    recs <- x$records

    if (is.null(recs) || length(recs) < 1L) {
        return(NA_character_)
    }

    val <- recs[[1]][[field]]
    if (is.null(val) || is.na(val) || !nzchar(val)) {
        return(NA_character_)
    }

    as.character(val)
}


.scholidonline_epmc_first_result <- function(x) {
    res <- x$resultList$result
    if (is.null(res) || length(res) < 1L) {
        return(NULL)
    }
    res[[1]]
}
