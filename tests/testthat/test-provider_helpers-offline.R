empty_df <- function() {
  data.frame(stringsAsFactors = FALSE)
}

req_json_http_bindings <- function(
    status = 200L,
    body = "{\"ok\":true}"
) {
  request_calls <- new.env(parent = emptyenv())
  request_calls$url <- NULL
  request_calls$query <- NULL
  request_calls$error_is_error <- NULL
  request_calls$perform_req <- NULL
  request_calls$status_n <- 0L
  request_calls$body_n <- 0L
  
  bindings <- list(
    .scholidonline_request = function(url) {
      request_calls$url <- url
      structure(
        list(url = url),
        class = "mock_req"
      )
    },
    .scholidonline_req_url_query = function(req, ...) {
      request_calls$query <- rlang::list2(...)
      req$query <- request_calls$query
      req
    },
    .scholidonline_req_error = function(req, is_error) {
      request_calls$error_is_error <- is_error
      req
    },
    .scholidonline_req_perform = function(req) {
      request_calls$perform_req <- req
      structure(
        list(
          status = status,
          body = body
        ),
        class = "mock_resp"
      )
    },
    .scholidonline_resp_status = function(resp) {
      request_calls$status_n <- request_calls$status_n + 1L
      resp$status
    },
    .scholidonline_resp_body_json = function(resp, ...) {
      request_calls$body_n <- request_calls$body_n + 1L
      
      if (identical(resp$body, "{\"a\":1,\"b\":[\"x\"]}")) {
        return(list(a = 1L, b = list("x")))
      }
      
      if (identical(resp$body, "{\"ok\":true}")) {
        return(list(ok = TRUE))
      }
      
      if (identical(resp$body, "{\"error\":\"not found\"}")) {
        return(list(error = "not found"))
      }
      
      if (identical(resp$body, "{\"error\":\"server\"}")) {
        return(list(error = "server"))
      }
      
      stop("Unexpected mock JSON body.", call. = FALSE)
    }
  )
  
  list(
    bindings = bindings,
    calls = request_calls
  )
}


testthat::test_that(
  ".scholidonline_req_json() returns parsed JSON on success",
  {
    mock <- req_json_http_bindings(
      status = 200L,
      body = "{\"a\":1,\"b\":[\"x\"]}"
    )
    
    bindings <- c(
      mock$bindings,
      list(.package = "scholidonline")
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    out <- scholidonline:::.scholidonline_req_json(
      url = "https://example.org/api",
      query = list(
        a = "x",
        b = 2L
      ),
      quiet = FALSE
    )
    
    testthat::expect_type(out, "list")
    testthat::expect_identical(out$a, 1L)
    testthat::expect_identical(out$b, list("x"))
    testthat::expect_identical(
      mock$calls$url,
      "https://example.org/api"
    )
    testthat::expect_identical(
      mock$calls$query,
      list(
        a = "x",
        b = 2L
      )
    )
    testthat::expect_true(
      is.function(mock$calls$error_is_error)
    )
    testthat::expect_s3_class(
      mock$calls$perform_req,
      "mock_req"
    )
    testthat::expect_identical(mock$calls$status_n, 1L)
    testthat::expect_identical(mock$calls$body_n, 1L)
  }
)


testthat::test_that(
  ".scholidonline_req_json() warns and returns NULL on HTTP error",
  {
    mock <- req_json_http_bindings(
      status = 404L,
      body = "{\"error\":\"not found\"}"
    )
    
    bindings <- c(
      mock$bindings,
      list(.package = "scholidonline")
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    testthat::expect_warning(
      out <- scholidonline:::.scholidonline_req_json(
        url = "https://example.org/api",
        query = list(id = "x"),
        quiet = FALSE
      ),
      "HTTP request failed \\(404\\): https://example.org/api"
    )
    
    testthat::expect_null(out)
    testthat::expect_identical(mock$calls$status_n, 2L)
    testthat::expect_identical(mock$calls$body_n, 0L)
  }
)


testthat::test_that(
  ".scholidonline_req_json() is silent on HTTP error when quiet is TRUE",
  {
    mock <- req_json_http_bindings(
      status = 500L,
      body = "{\"error\":\"server\"}"
    )
    
    bindings <- c(
      mock$bindings,
      list(.package = "scholidonline")
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    testthat::expect_no_warning(
      out <- scholidonline:::.scholidonline_req_json(
        url = "https://example.org/api",
        query = list(id = "x"),
        quiet = TRUE
      )
    )
    
    testthat::expect_null(out)
    testthat::expect_identical(mock$calls$status_n, 1L)
    testthat::expect_identical(mock$calls$body_n, 0L)
  }
)


testthat::test_that(
  ".scholidonline_epmc_search() uses defaults and forwards args",
  {
    req_json_calls <- new.env(parent = emptyenv())
    req_json_calls$url <- NULL
    req_json_calls$query <- NULL
    req_json_calls$quiet <- NULL
    
    bindings <- list(
      .scholidonline_req_json = function(url, query, quiet) {
        req_json_calls$url <- url
        req_json_calls$query <- query
        req_json_calls$quiet <- quiet
        list(hitCount = 1L)
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    out <- scholidonline:::.scholidonline_epmc_search(
      query = "EXT_ID:123"
    )
    
    testthat::expect_identical(out, list(hitCount = 1L))
    testthat::expect_identical(
      req_json_calls$url,
      paste0(
        "https://www.ebi.ac.uk/europepmc/",
        "webservices/rest/search"
      )
    )
    testthat::expect_identical(
      req_json_calls$query,
      list(
        query = "EXT_ID:123",
        format = "json",
        pageSize = 1L
      )
    )
    testthat::expect_false(req_json_calls$quiet)
  }
)


testthat::test_that(
  ".scholidonline_epmc_search() respects pageSize, format, and quiet",
  {
    req_json_calls <- new.env(parent = emptyenv())
    req_json_calls$query <- NULL
    req_json_calls$quiet <- NULL
    
    bindings <- list(
      .scholidonline_req_json = function(url, query, quiet) {
        req_json_calls$query <- query
        req_json_calls$quiet <- quiet
        list(ok = TRUE)
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    out <- scholidonline:::.scholidonline_epmc_search(
      query = "PMID:12345",
      pageSize = 5L,
      format = "xml",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, list(ok = TRUE))
    testthat::expect_identical(
      req_json_calls$query,
      list(
        query = "PMID:12345",
        format = "xml",
        pageSize = 5L
      )
    )
    testthat::expect_true(req_json_calls$quiet)
  }
)


testthat::test_that(
  ".scholidonline_pmc_idconv() builds expected request",
  {
    req_json_calls <- new.env(parent = emptyenv())
    req_json_calls$url <- NULL
    req_json_calls$query <- NULL
    req_json_calls$quiet <- NULL
    
    bindings <- list(
      .scholidonline_req_json = function(url, query, quiet) {
        req_json_calls$url <- url
        req_json_calls$query <- query
        req_json_calls$quiet <- quiet
        list(records = empty_df())
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    out <- scholidonline:::.scholidonline_pmc_idconv(
      ids = "PMC12345",
      tool = "scholidonline",
      email = "x@example.org",
      quiet = TRUE
    )
    
    testthat::expect_identical(out, list(records = empty_df()))
    testthat::expect_identical(
      req_json_calls$url,
      "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
    )
    testthat::expect_identical(
      req_json_calls$query,
      list(
        format = "json",
        ids = "PMC12345",
        tool = "scholidonline",
        email = "x@example.org"
      )
    )
    testthat::expect_true(req_json_calls$quiet)
  }
)


testthat::test_that(
  ".scholidonline_esummary_pubmed() builds expected request",
  {
    req_json_calls <- new.env(parent = emptyenv())
    req_json_calls$url <- NULL
    req_json_calls$query <- NULL
    req_json_calls$quiet <- NULL
    
    bindings <- list(
      .scholidonline_req_json = function(url, query, quiet) {
        req_json_calls$url <- url
        req_json_calls$query <- query
        req_json_calls$quiet <- quiet
        list(result = list())
      },
      .package = "scholidonline"
    )
    do.call(testthat::local_mocked_bindings, bindings)
    
    out <- scholidonline:::.scholidonline_esummary_pubmed(
      id = "12345",
      api_key = "abc",
      version = "2.0",
      quiet = FALSE
    )
    
    testthat::expect_identical(out, list(result = list()))
    testthat::expect_identical(
      req_json_calls$url,
      paste0(
        "https://eutils.ncbi.nlm.nih.gov/",
        "entrez/eutils/esummary.fcgi"
      )
    )
    testthat::expect_identical(
      req_json_calls$query,
      list(
        db = "pubmed",
        id = "12345",
        retmode = "json",
        api_key = "abc",
        version = "2.0"
      )
    )
    testthat::expect_false(req_json_calls$quiet)
  }
)