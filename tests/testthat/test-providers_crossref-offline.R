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


crossref_http_bindings <- function(
    resp = structure(list(), class = "fake_resp"),
    status = 200L,
    json = NULL,
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
    .scholidonline_resp_body_json = function(resp, ...) {
      json
    },
    .package = "scholidonline"
  )
}


testthat::test_that(
  ".exists_doi_crossref() returns TRUE on 2xx status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(status = 200L)
    )
    
    testthat::expect_true(
      .exists_doi_crossref("10.1000/test", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_doi_crossref() returns FALSE on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(status = 404L)
    )
    
    testthat::expect_false(
      .exists_doi_crossref("10.1000/test", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_doi_crossref() returns NA on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .exists_doi_crossref("10.1000/test", quiet = FALSE),
      "Crossref request failed\\."
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".exists_doi_crossref() returns NA on non-404 non-2xx status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .exists_doi_crossref("10.1000/test", quiet = FALSE),
      "Crossref request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, NA)
  }
)


testthat::test_that(
  ".links_doi_crossref() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .links_doi_crossref("10.1000/test", quiet = FALSE),
      "Crossref request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_doi_crossref() returns empty data.frame on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(status = 404L)
    )
    
    testthat::expect_identical(
      .links_doi_crossref("10.1000/test", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_doi_crossref() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(status = 503L)
    )
    
    testthat::expect_warning(
      out <- .links_doi_crossref("10.1000/test", quiet = FALSE),
      "Crossref request returned HTTP 503\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_doi_crossref() returns empty data.frame on NULL json",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(
        status = 200L,
        json = NULL
      )
    )
    
    testthat::expect_identical(
      .links_doi_crossref("10.1000/test", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_doi_crossref() returns empty data.frame with no links",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(
        status = 200L,
        json = list(
          message = list()
        )
      )
    )
    
    testthat::expect_identical(
      .links_doi_crossref("10.1000/test", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_doi_crossref() returns PMID and PMCID links",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(
        status = 200L,
        json = list(
          message = list(
            `pubmed-id` = "12345",
            pmcid = "PMC123"
          )
        )
      )
    )
    
    out <- .links_doi_crossref("10.1000/test", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      out$linked_type,
      c("pmid", "pmcid")
    )
    testthat::expect_identical(
      out$linked_value,
      c("12345", "PMC123")
    )
    testthat::expect_identical(
      out$provider,
      c("crossref", "crossref")
    )
  }
)


testthat::test_that(
  ".links_doi_crossref() returns relation DOI links",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(
        status = 200L,
        json = list(
          message = list(
            relation = list(
              `is-preprint-of` = list(
                list(id = "10.1000/a"),
                list(id = "10.1000/b")
              )
            )
          )
        )
      )
    )
    
    out <- .links_doi_crossref("10.1000/test", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      out$linked_type,
      c("doi", "doi")
    )
    testthat::expect_identical(
      out$linked_value,
      c("10.1000/a", "10.1000/b")
    )
    testthat::expect_identical(
      out$provider,
      c("crossref", "crossref")
    )
  }
)


testthat::test_that(
  ".links_doi_crossref() returns mixed identifier links",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(
        status = 200L,
        json = list(
          message = list(
            `pubmed-id` = "12345",
            pmcid = "PMC123",
            relation = list(
              `is-preprint-of` = list(
                list(id = "10.1000/a")
              )
            )
          )
        )
      )
    )
    
    out <- .links_doi_crossref("10.1000/test", quiet = TRUE)
    
    testthat::expect_identical(
      out$linked_type,
      c("pmid", "pmcid", "doi")
    )
    testthat::expect_identical(
      out$linked_value,
      c("12345", "PMC123", "10.1000/a")
    )
    testthat::expect_identical(
      out$provider,
      c("crossref", "crossref", "crossref")
    )
  }
)


testthat::test_that(
  ".meta_doi_crossref() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .meta_doi_crossref("10.1000/test", quiet = FALSE),
      "Crossref request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_doi_crossref() returns empty data.frame on 404 status",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(status = 404L)
    )
    
    testthat::expect_identical(
      .meta_doi_crossref("10.1000/test", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_doi_crossref() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .meta_doi_crossref("10.1000/test", quiet = FALSE),
      "Crossref request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_doi_crossref() returns harmonized metadata",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(
        status = 200L,
        json = list(
          message = list(
            title = list("Paper title"),
            issued = list(`date-parts` = list(c(2021L, 1L, 1L))),
            `container-title` = list("Journal Name"),
            DOI = "10.1000/test",
            URL = "https://doi.org/10.1000/test"
          )
        )
      )
    )
    
    out <- .meta_doi_crossref("10.1000/test", quiet = TRUE)
    
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
    testthat::expect_identical(out$provider, "crossref")
  }
)


testthat::test_that(
  ".meta_doi_crossref() falls back to input DOI",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(
        status = 200L,
        json = list(
          message = list(
            title = list("Paper title"),
            issued = list(`date-parts` = list(c(2021L))),
            `container-title` = list("Journal Name"),
            URL = "https://doi.org/10.1000/test"
          )
        )
      )
    )
    
    out <- .meta_doi_crossref("10.1000/test", quiet = TRUE)
    
    testthat::expect_identical(out$doi, "10.1000/test")
  }
)


testthat::test_that(
  ".meta_doi_crossref() returns NA for missing optional fields",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      crossref_http_bindings(
        status = 200L,
        json = list(
          message = list()
        )
      )
    )
    
    out <- .meta_doi_crossref("10.1000/test", quiet = TRUE)
    
    testthat::expect_true(is.na(out$title))
    testthat::expect_true(is.na(out$year))
    testthat::expect_true(is.na(out$container))
    testthat::expect_identical(out$doi, "10.1000/test")
    testthat::expect_true(is.na(out$pmid))
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_true(is.na(out$url))
    testthat::expect_identical(out$provider, "crossref")
  }
)