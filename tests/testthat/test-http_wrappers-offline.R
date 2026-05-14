testthat::test_that(
  ".scholidonline_resp_body_json_safe() returns parsed JSON on success",
  {
    testthat::local_mocked_bindings(
      .scholidonline_resp_body_json = function(resp, ...) {
        testthat::expect_identical(resp, "response")
        
        list(
          ok = TRUE
        )
      }
    )
    
    out <- .scholidonline_resp_body_json_safe(
      resp = "response",
      simplifyVector = FALSE,
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      list(ok = TRUE)
    )
  }
)

testthat::test_that(
  ".scholidonline_resp_body_json_safe() returns NULL quietly on parse failure",
  {
    testthat::local_mocked_bindings(
      .scholidonline_resp_body_json = function(resp, ...) {
        stop("bad json", call. = FALSE)
      }
    )
    
    out <- .scholidonline_resp_body_json_safe(
      resp = "response",
      quiet = TRUE
    )
    
    testthat::expect_null(out)
  }
)

testthat::test_that(
  ".scholidonline_resp_body_json_safe() warns on parse failure when quiet is FALSE",
  {
    testthat::local_mocked_bindings(
      .scholidonline_resp_body_json = function(resp, ...) {
        stop("bad json", call. = FALSE)
      }
    )
    
    testthat::expect_warning(
      out <- .scholidonline_resp_body_json_safe(
        resp = "response",
        quiet = FALSE
      ),
      "HTTP response could not be parsed as JSON.",
      fixed = TRUE
    )
    
    testthat::expect_null(out)
  }
)

testthat::test_that(
  ".scholidonline_resp_body_string() forwards to httr2",
  {
    resp <- httr2::response(
      status_code = 200L,
      body = charToRaw("hello")
    )
    
    out <- .scholidonline_resp_body_string(
      resp = resp
    )
    
    testthat::expect_identical(
      out,
      "hello"
    )
  }
)

testthat::test_that(
  ".scholidonline_req_headers() forwards headers to httr2",
  {
    req <- httr2::request("https://example.org")
    
    out <- .scholidonline_req_headers(
      req = req,
      `User-Agent` = "scholidonline-test"
    )
    
    testthat::expect_s3_class(
      out,
      "httr2_request"
    )
    
    testthat::expect_true(
      "User-Agent" %in% names(out$headers)
    )
    
    testthat::expect_identical(
      out$headers[["User-Agent"]],
      "scholidonline-test"
    )
  }
)