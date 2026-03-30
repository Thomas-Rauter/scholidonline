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


doi_org_http_bindings <- function(
    resp = structure(list(), class = "fake_resp"),
    status = 200L,
    json = NULL,
    perform_null = FALSE
) {
  list(
    .scholidonline_request = function(url) {
      structure(list(url = url), class = "fake_req")
    },
    .scholidonline_req_headers = function(req, ...) {
      req
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
    .scholidonline_resp_body_json = function(resp, ...) {
      json
    },
    .package = "scholidonline"
  )
}


testthat::test_that(
  ".exists_doi_doi_org() returns TRUE on 2xx status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      doi_org_http_bindings(status = 200L)
    )
    
    testthat::expect_true(
      .exists_doi_doi_org("10.1000/test", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_doi_doi_org() returns FALSE on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      doi_org_http_bindings(status = 404L)
    )
    
    testthat::expect_false(
      .exists_doi_doi_org("10.1000/test", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_doi_doi_org() returns NA on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      doi_org_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .exists_doi_doi_org("10.1000/test", quiet = FALSE),
      "DOI.org request failed\\."
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".exists_doi_doi_org() returns NA on non-404 non-2xx status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      doi_org_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .exists_doi_doi_org("10.1000/test", quiet = FALSE),
      "DOI.org request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".meta_doi_doi_org() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      doi_org_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .meta_doi_doi_org("10.1000/test", quiet = FALSE),
      "DOI.org request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_doi_doi_org() returns empty data.frame on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      doi_org_http_bindings(status = 404L)
    )
    
    testthat::expect_identical(
      .meta_doi_doi_org("10.1000/test", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_doi_doi_org() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      doi_org_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .meta_doi_doi_org("10.1000/test", quiet = FALSE),
      "DOI.org request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_doi_doi_org() returns harmonized metadata",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      doi_org_http_bindings(
        status = 200L,
        json = list(
          title = "Paper title",
          issued = list(`date-parts` = list(c(2021L, 1L, 1L))),
          `container-title` = "Journal Name",
          DOI = "10.1000/test",
          URL = "https://doi.org/10.1000/test"
        )
      )
    )
    
    out <- .meta_doi_doi_org("10.1000/test", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$title, "Paper title")
    testthat::expect_identical(out$year, 2021L)
    testthat::expect_identical(out$container, "Journal Name")
    testthat::expect_identical(out$doi, "10.1000/test")
    testthat::expect_true(is.na(out$pmid))
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_identical(
      out$url,
      "https://doi.org/10.1000/test"
    )
    testthat::expect_identical(out$provider, "doi.org")
  }
)


testthat::test_that(
  ".meta_doi_doi_org() falls back to input DOI",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      doi_org_http_bindings(
        status = 200L,
        json = list(
          title = "Paper title",
          issued = list(`date-parts` = list(c(2021L))),
          `container-title` = "Journal Name",
          URL = "https://doi.org/10.1000/test"
        )
      )
    )
    
    out <- .meta_doi_doi_org("10.1000/test", quiet = TRUE)
    
    testthat::expect_identical(out$doi, "10.1000/test")
  }
)


testthat::test_that(
  ".meta_doi_doi_org() returns NA for missing optional fields",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      doi_org_http_bindings(
        status = 200L,
        json = list()
      )
    )
    
    out <- .meta_doi_doi_org("10.1000/test", quiet = TRUE)
    
    testthat::expect_true(is.na(out$title))
    testthat::expect_true(is.na(out$year))
    testthat::expect_true(is.na(out$container))
    testthat::expect_identical(out$doi, "10.1000/test")
    testthat::expect_true(is.na(out$pmid))
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_true(is.na(out$url))
    testthat::expect_identical(out$provider, "doi.org")
  }
)