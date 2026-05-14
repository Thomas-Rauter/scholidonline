testthat::test_that(
  ".arxiv_id_list_url() rejects non-character input",
  {
    testthat::expect_error(
      .arxiv_id_list_url(1),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".arxiv_id_list_url() constructs id_list query URL",
  {
    out <- .arxiv_id_list_url(
      c(
        "hep-ex/0307015",
        "0706.0001",
        "1503.07589"
      )
    )
    
    testthat::expect_identical(
      out,
      paste0(
        "https://export.arxiv.org/api/query?",
        "id_list=hep-ex%2F0307015,0706.0001,1503.07589"
      )
    )
  }
)

testthat::test_that(
  ".arxiv_query_id_list() rejects unused dots",
  {
    testthat::expect_error(
      .arxiv_query_id_list(
        x = "0706.0001",
        unused = TRUE,
        quiet = TRUE
      )
    )
  }
)

testthat::test_that(
  ".arxiv_query_id_list() rejects non-character input",
  {
    testthat::expect_error(
      .arxiv_query_id_list(
        x = 706.0001,
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".arxiv_query_id_list() returns NULL for no valid input",
  {
    out <- .arxiv_query_id_list(
      x = c(NA_character_, ""),
      quiet = TRUE
    )
    
    testthat::expect_null(out)
  }
)

testthat::test_that(
  ".arxiv_query_id_list() returns NULL on request failure",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        NULL
      },
      .arxiv_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .arxiv_query_id_list(
      x = "0706.0001",
      quiet = TRUE
    )
    
    testthat::expect_null(out)
  }
)

testthat::test_that(
  ".arxiv_query_id_list() returns NULL on HTTP error",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        503L
      },
      .arxiv_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .arxiv_query_id_list(
      x = "0706.0001",
      quiet = TRUE
    )
    
    testthat::expect_null(out)
  }
)

testthat::test_that(
  ".arxiv_query_id_list() returns NULL when body cannot be read",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_string = function(resp) {
        stop("cannot read body", call. = FALSE)
      },
      .arxiv_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .arxiv_query_id_list(
      x = "0706.0001",
      quiet = TRUE
    )
    
    testthat::expect_null(out)
  }
)

testthat::test_that(
  ".arxiv_query_id_list() returns response body string",
  {
    testthat::local_mocked_bindings(
      .scholidonline_request = function(url) {
        testthat::expect_identical(
          url,
          paste0(
            "https://export.arxiv.org/api/query?",
            "id_list=hep-ex%2F0307015,0706.0001"
          )
        )
        
        "request"
      },
      .scholidonline_req_error = function(req, is_error) {
        testthat::expect_identical(req, "request")
        testthat::expect_true(is.function(is_error))
        "request-with-error-handler"
      },
      .scholidonline_req_perform_safe = function(req) {
        testthat::expect_identical(req, "request-with-error-handler")
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        testthat::expect_identical(resp, "response")
        200L
      },
      .scholidonline_resp_body_string = function(resp) {
        testthat::expect_identical(resp, "response")
        "<feed></feed>"
      },
      .arxiv_rate_limit = function(quiet = FALSE) {
        testthat::expect_identical(quiet, TRUE)
        invisible(NULL)
      }
    )
    
    out <- .arxiv_query_id_list(
      x = c(
        "hep-ex/0307015",
        NA_character_,
        "",
        "0706.0001"
      ),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      "<feed></feed>"
    )
  }
)