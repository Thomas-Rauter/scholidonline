#' Perform an HTTP GET request and parse JSON response
#'
#' @description
#' Internal helper used by scholidonline provider implementations to perform
#' HTTP GET requests and parse JSON responses.
#'
#' This helper constructs a request using the internal HTTP wrapper layer,
#' executes it, and returns the parsed JSON body as an R object. Query
#' parameters are passed via `query` and added using
#' `.scholidonline_req_url_query()`.
#'
#' HTTP responses with status codes greater than or equal to 400 are treated
#' as failures. In such cases, a warning is emitted (unless `quiet = TRUE`)
#' and `NULL` is returned.
#'
#' The function is designed to be testable in offline environments by relying
#' exclusively on internal HTTP wrapper functions rather than calling
#' `httr2` directly.
#'
#' @param url A single character string specifying the request URL.
#' @param query A named list of query parameters.
#' @param quiet A single logical value; if `TRUE`, suppress warnings on
#'   failed HTTP requests.
#'
#' @return A parsed JSON object (typically a list), or `NULL` on failure.
#'
#' @noRd
.scholidonline_req_json <- function(
    url,
    query,
    quiet
) {
  req <- .scholidonline_request(url)
  req <- .scholidonline_req_url_query(req, !!!query)
  req <- .scholidonline_req_error(
    req,
    is_error = function(resp) FALSE
  )
  
  resp <- .scholidonline_req_perform(req)
  
  if (.scholidonline_resp_status(resp) >= 400) {
    if (!isTRUE(quiet)) {
      rlang::warn(paste0(
        "HTTP request failed (", .scholidonline_resp_status(resp), "): ", url
        ))
    }
    return(NULL)
  }
  
  .scholidonline_resp_body_json(
    resp,
    simplifyVector = FALSE
  )
}


#' Europe PMC search helper
#'
#' @description
#' Internal helper for querying the Europe PMC REST search API.
#'
#' This function constructs and executes a search query against Europe PMC and
#' returns the parsed JSON response. It is used by provider implementations for
#' identifier conversion and metadata retrieval.
#'
#' The helper supports a minimal subset of query parameters via `...`, notably:
#' - `pageSize`: number of results to return (default: 1)
#' - `format`: response format (default: `"json"`)
#'
#' @param query A single search query string.
#' @param ... Additional query parameters passed to the API.
#' @param quiet Logical; if `TRUE`, suppress warnings on failed requests.
#'
#' @return A parsed JSON object (list), or `NULL` on failure.
#'
#' @importFrom rlang %||%
#'
#' @noRd
.scholidonline_epmc_search <- function(query, ..., quiet = FALSE) {
  dots <- list(...)
  page_size <- dots$pageSize %||% 1L
  format <- dots$format %||% "json"
  
  .epmc_rate_limit(quiet = quiet)
  
  .scholidonline_req_json(
    url = "https://www.ebi.ac.uk/europepmc/webservices/rest/search",
    query = list(query = query, format = format, pageSize = page_size),
    quiet = quiet
  )
}


#' Query the NCBI PMC ID conversion API
#'
#' @description
#' Internal helper used by scholidonline provider implementations to query
#' the NCBI PMC ID conversion API.
#'
#' This helper performs a request to the PMC `idconv` endpoint and returns
#' the parsed JSON response. It is typically used to retrieve mappings
#' between identifiers such as PMCID, PMID, and DOI.
#'
#' Additional query parameters can be supplied via `...` and are forwarded
#' to the API request.
#'
#' @param ids A character vector of identifiers to convert.
#' @param ... Additional query parameters passed to the API.
#' @param quiet A single logical value; if `TRUE`, suppress warnings on
#'   failed HTTP requests.
#'
#' @return A parsed JSON object (list), or `NULL` on failure.
#'
#' @noRd
.scholidonline_pmc_idconv <- function(
    ids,
    ...,
    quiet = FALSE
) {
  dots <- list(...)
  
  .ncbi_rate_limit(
    quiet = quiet
  )
  
  .scholidonline_req_json(
    url = "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/",
    query = c(list(format = "json", ids = ids), dots),
    quiet = quiet
  )
}


#' Query the NCBI PubMed ESummary API
#'
#' @description
#' Internal helper used by scholidonline provider implementations to query
#' the NCBI Entrez ESummary API for PubMed records.
#'
#' This helper performs a request to the PubMed `esummary` endpoint and
#' returns the parsed JSON response. It is typically used to retrieve
#' structured metadata summaries for one or more PubMed identifiers.
#'
#' Additional query parameters can be supplied via `...` and are forwarded
#' to the API request.
#'
#' @param id A character vector of PubMed identifiers.
#' @param ... Additional query parameters passed to the API.
#' @param quiet A single logical value; if `TRUE`, suppress warnings on
#'   failed HTTP requests.
#'
#' @return A parsed JSON object (list), or `NULL` on failure.
#'
#' @noRd
.scholidonline_esummary_pubmed <- function(
    id,
    ...,
    quiet = FALSE
) {
  dots <- list(...)
  
  .ncbi_rate_limit(
    quiet = quiet
  )
  
  .scholidonline_req_json(
    url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
    query = c(
      list(
        db = "pubmed",
        id = id,
        retmode = "json"
      ),
      dots
    ),
    quiet = quiet
  )
}
