empty_df <- function() {
  data.frame(stringsAsFactors = FALSE)
}


scalar_check_bindings <- function() {
  list(
    .scholidonline_check_scalar_chr = function(x) {
      invisible(TRUE)
    },
    .package = "scholidonline"
  )
}


arxiv_http_bindings <- function(
    resp = structure(list(), class = "fake_resp"),
    status = 200L,
    body_string = NULL,
    perform_null = FALSE
) {
  list(
    .scholidonline_request = function(url) {
      structure(list(url = url), class = "fake_req")
    },
    .scholidonline_req_error = function(req, is_error) {
      req
    },
    .scholidonline_req_perform_safe = function(req) {
      if (isTRUE(perform_null)) {
        return(NULL)
      }
      resp
    },
    .scholidonline_resp_status = function(resp) {
      status
    },
    .scholidonline_resp_body_string = function(resp, ...) {
      body_string
    },
    .package = "scholidonline"
  )
}


testthat::test_that(
  ".exists_arxiv_arxiv() returns TRUE when entry is present",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 200L,
        body_string = "<feed><entry><id>http://arxiv.org/abs/1234.5678</id></entry></feed>"
      )
    )
    
    testthat::expect_true(
      .exists_arxiv_arxiv("1234.5678", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_arxiv_arxiv() returns FALSE when entry is absent",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 200L,
        body_string = "<feed></feed>"
      )
    )
    
    testthat::expect_false(
      .exists_arxiv_arxiv("1234.5678", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_arxiv_arxiv() returns NA on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .exists_arxiv_arxiv("1234.5678", quiet = FALSE),
      "arXiv request failed\\."
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".exists_arxiv_arxiv() returns NA on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 500L,
        body_string = "<feed></feed>"
      )
    )
    
    testthat::expect_warning(
      out <- .exists_arxiv_arxiv("1234.5678", quiet = FALSE),
      "arXiv request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".exists_arxiv_arxiv() returns NA on NULL body string",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 200L,
        body_string = NULL
      )
    )
    
    testthat::expect_identical(
      .exists_arxiv_arxiv("1234.5678", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".links_arxiv_arxiv() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .links_arxiv_arxiv("1234.5678", quiet = FALSE),
      "arXiv request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_arxiv_arxiv() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 503L,
        body_string = "<feed></feed>"
      )
    )
    
    testthat::expect_warning(
      out <- .links_arxiv_arxiv("1234.5678", quiet = FALSE),
      "arXiv request returned HTTP 503\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_arxiv_arxiv() returns empty data.frame on NULL body string",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 200L,
        body_string = NULL
      )
    )
    
    testthat::expect_identical(
      .links_arxiv_arxiv("1234.5678", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_arxiv_arxiv() returns empty data.frame when DOI is absent",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 200L,
        body_string = "<feed><entry><title>x</title></entry></feed>"
      )
    )
    
    testthat::expect_identical(
      .links_arxiv_arxiv("1234.5678", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_arxiv_arxiv() returns DOI link when present",
  {
    xml <- paste0(
      "<feed><entry>",
      "<id>http://arxiv.org/abs/1234.5678</id>",
      "<arxiv:doi>10.1000/test</arxiv:doi>",
      "</entry></feed>"
    )
    
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 200L,
        body_string = xml
      )
    )
    
    out <- .links_arxiv_arxiv("1234.5678", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$linked_type, "doi")
    testthat::expect_identical(out$linked_value, "10.1000/test")
    testthat::expect_identical(out$provider, "arxiv")
  }
)


testthat::test_that(
  ".meta_arxiv_arxiv() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .meta_arxiv_arxiv("1234.5678", quiet = FALSE),
      "arXiv request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_arxiv_arxiv() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 500L,
        body_string = "<feed></feed>"
      )
    )
    
    testthat::expect_warning(
      out <- .meta_arxiv_arxiv("1234.5678", quiet = FALSE),
      "arXiv request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_arxiv_arxiv() returns empty data.frame when entry is absent",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 200L,
        body_string = "<feed><title>No entry</title></feed>"
      )
    )
    
    testthat::expect_identical(
      .meta_arxiv_arxiv("1234.5678", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_arxiv_arxiv() returns harmonized metadata",
  {
    xml <- paste0(
      "<feed><entry>",
      "<title>Paper title</title>",
      "<published>2021-05-06T00:00:00Z</published>",
      "<id>http://arxiv.org/abs/1234.5678</id>",
      "</entry></feed>"
    )
    
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 200L,
        body_string = xml
      )
    )
    
    out <- .meta_arxiv_arxiv("1234.5678", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$title, "Paper title")
    testthat::expect_identical(out$year, 2021L)
    testthat::expect_identical(out$container, "arXiv")
    testthat::expect_true(is.na(out$doi))
    testthat::expect_true(is.na(out$pmid))
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_identical(
      out$url,
      "http://arxiv.org/abs/1234.5678"
    )
    testthat::expect_identical(out$provider, "arxiv")
  }
)


testthat::test_that(
  ".meta_arxiv_arxiv() returns NA year when year is not parseable",
  {
    xml <- paste0(
      "<feed><entry>",
      "<title>Paper title</title>",
      "<published>xxxx</published>",
      "<id>http://arxiv.org/abs/1234.5678</id>",
      "</entry></feed>"
    )
    
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      arxiv_http_bindings(
        status = 200L,
        body_string = xml
      )
    )
    
    out <- .meta_arxiv_arxiv("1234.5678", quiet = TRUE)
    
    testthat::expect_true(is.na(out$year))
    testthat::expect_identical(out$title, "Paper title")
    testthat::expect_identical(
      out$url,
      "http://arxiv.org/abs/1234.5678"
    )
  }
)