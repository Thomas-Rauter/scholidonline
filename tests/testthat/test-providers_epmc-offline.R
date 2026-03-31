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


epmc_search_bindings <- function(result = NULL) {
  list(
    .scholidonline_epmc_search = function(query, ..., quiet = FALSE) {
      result
    },
    .package = "scholidonline"
  )
}


epmc_http_bindings <- function(
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
  ".exists_pmid_epmc() returns NA on NULL response",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(result = NULL)
    )
    
    testthat::expect_identical(
      .exists_pmid_epmc("12345", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".exists_pmid_epmc() returns NA on non-integer hitCount",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(hitCount = "abc")
      )
    )
    
    testthat::expect_identical(
      .exists_pmid_epmc("12345", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".exists_pmid_epmc() returns FALSE on zero hitCount",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(hitCount = "0")
      )
    )
    
    testthat::expect_false(
      .exists_pmid_epmc("12345", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_pmid_epmc() returns TRUE on positive hitCount",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(hitCount = "2")
      )
    )
    
    testthat::expect_true(
      .exists_pmid_epmc("12345", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_pmcid_epmc() returns NA on NULL response",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(result = NULL)
    )
    
    testthat::expect_identical(
      .exists_pmcid_epmc("PMC123", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".exists_pmcid_epmc() returns FALSE on zero hitCount",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(hitCount = "0")
      )
    )
    
    testthat::expect_false(
      .exists_pmcid_epmc("PMC123", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_pmcid_epmc() returns TRUE on positive hitCount",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(hitCount = "1")
      )
    )
    
    testthat::expect_true(
      .exists_pmcid_epmc("PMC123", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".links_pmid_epmc() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .links_pmid_epmc("12345", quiet = FALSE),
      "Europe PMC request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_pmid_epmc() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(status = 503L)
    )
    
    testthat::expect_warning(
      out <- .links_pmid_epmc("12345", quiet = FALSE),
      "Europe PMC request returned HTTP 503\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_pmid_epmc() returns empty data.frame on NULL json",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(
        status = 200L,
        json = NULL
      )
    )
    
    testthat::expect_identical(
      .links_pmid_epmc("12345", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_pmid_epmc() returns empty data.frame on no results",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(
        status = 200L,
        json = list(
          resultList = list(result = list())
        )
      )
    )
    
    testthat::expect_identical(
      .links_pmid_epmc("12345", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_pmid_epmc() returns linked identifiers",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(
        status = 200L,
        json = list(
          resultList = list(
            result = list(
              list(
                pmid = "12345",
                pmcid = "PMC123",
                doi = "10.1000/test"
              )
            )
          )
        )
      )
    )
    
    out <- .links_pmid_epmc("12345", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      out$linked_type,
      c("pmid", "pmcid", "doi")
    )
    testthat::expect_identical(
      out$linked_value,
      c("12345", "PMC123", "10.1000/test")
    )
    testthat::expect_identical(
      out$provider,
      c("epmc", "epmc", "epmc")
    )
  }
)


testthat::test_that(
  ".links_pmcid_epmc() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .links_pmcid_epmc("PMC123", quiet = FALSE),
      "Europe PMC request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_pmcid_epmc() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .links_pmcid_epmc("PMC123", quiet = FALSE),
      "Europe PMC request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_pmcid_epmc() returns linked identifiers",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(
        status = 200L,
        json = list(
          resultList = list(
            result = list(
              list(
                pmid = "12345",
                pmcid = "PMC123",
                doi = "10.1000/test"
              )
            )
          )
        )
      )
    )
    
    out <- .links_pmcid_epmc("PMC123", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      out$linked_type,
      c("pmid", "pmcid", "doi")
    )
    testthat::expect_identical(
      out$linked_value,
      c("12345", "PMC123", "10.1000/test")
    )
    testthat::expect_identical(
      out$provider,
      c("epmc", "epmc", "epmc")
    )
  }
)


testthat::test_that(
  ".meta_pmid_epmc() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .meta_pmid_epmc("12345", quiet = FALSE),
      "Europe PMC request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_pmid_epmc() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .meta_pmid_epmc("12345", quiet = FALSE),
      "Europe PMC request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_pmid_epmc() returns empty data.frame on no results",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(
        status = 200L,
        json = list(
          resultList = list(result = list())
        )
      )
    )
    
    testthat::expect_identical(
      .meta_pmid_epmc("12345", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_pmid_epmc() returns harmonized metadata from list result",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(
        status = 200L,
        json = list(
          resultList = list(
            result = list(
              list(
                title = "Paper title",
                pubYear = "2021",
                journalTitle = "Journal Name",
                doi = "10.1000/test",
                pmcid = "PMC123"
              )
            )
          )
        )
      )
    )
    
    out <- .meta_pmid_epmc("12345", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$title, "Paper title")
    testthat::expect_identical(out$year, 2021L)
    testthat::expect_identical(out$container, "Journal Name")
    testthat::expect_identical(out$doi, "10.1000/test")
    testthat::expect_identical(out$pmid, "12345")
    testthat::expect_identical(out$pmcid, "PMC123")
    testthat::expect_identical(
      out$url,
      "https://europepmc.org/article/MED/12345"
    )
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".meta_pmid_epmc() returns harmonized metadata from data.frame result",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(
        status = 200L,
        json = list(
          resultList = list(
            result = data.frame(
              title = "Paper title",
              pubYear = "2021",
              journalTitle = "Journal Name",
              doi = "10.1000/test",
              pmcid = "PMC123",
              stringsAsFactors = FALSE
            )
          )
        )
      )
    )
    
    out <- .meta_pmid_epmc("12345", quiet = TRUE)
    
    testthat::expect_identical(out$title, "Paper title")
    testthat::expect_identical(out$year, 2021L)
    testthat::expect_identical(out$container, "Journal Name")
    testthat::expect_identical(out$doi, "10.1000/test")
    testthat::expect_identical(out$pmid, "12345")
    testthat::expect_identical(out$pmcid, "PMC123")
  }
)


testthat::test_that(
  ".meta_pmcid_epmc() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .meta_pmcid_epmc("PMC123", quiet = FALSE),
      "Europe PMC request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_pmcid_epmc() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .meta_pmcid_epmc("PMC123", quiet = FALSE),
      "Europe PMC request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_pmcid_epmc() returns harmonized metadata",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_http_bindings(
        status = 200L,
        json = list(
          resultList = list(
            result = list(
              list(
                title = "Paper title",
                pubYear = "2020",
                journalTitle = "Journal Name",
                doi = "10.1000/test",
                pmid = "12345"
              )
            )
          )
        )
      )
    )
    
    out <- .meta_pmcid_epmc("PMC123", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$title, "Paper title")
    testthat::expect_identical(out$year, 2020L)
    testthat::expect_identical(out$container, "Journal Name")
    testthat::expect_identical(out$doi, "10.1000/test")
    testthat::expect_identical(out$pmid, "12345")
    testthat::expect_identical(out$pmcid, "PMC123")
    testthat::expect_identical(
      out$url,
      "https://europepmc.org/article/PMC/123"
    )
    testthat::expect_identical(out$provider, "epmc")
  }
)


testthat::test_that(
  ".convert_pmid_to_doi_epmc() returns DOI from first result",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(
          resultList = list(
            result = list(
              list(doi = "10.1000/test")
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmid_to_doi_epmc("12345", quiet = TRUE),
      "10.1000/test"
    )
  }
)


testthat::test_that(
  ".convert_pmid_to_doi_epmc() returns NA on missing DOI",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(
          resultList = list(
            result = list(
              list(doi = "")
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmid_to_doi_epmc("12345", quiet = TRUE),
      NA_character_
    )
  }
)


testthat::test_that(
  ".convert_doi_to_pmid_epmc() returns PMID from first result",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(
          resultList = list(
            result = list(
              list(pmid = "12345")
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_doi_to_pmid_epmc("10.1000/test", quiet = TRUE),
      "12345"
    )
  }
)


testthat::test_that(
  ".convert_pmcid_to_pmid_epmc() returns PMID from first result",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(
          resultList = list(
            result = list(
              list(pmid = "12345")
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmcid_to_pmid_epmc("PMC123", quiet = TRUE),
      "12345"
    )
  }
)


testthat::test_that(
  ".convert_pmcid_to_doi_epmc() returns DOI from first result",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(
          resultList = list(
            result = list(
              list(doi = "10.1000/test")
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmcid_to_doi_epmc("PMC123", quiet = TRUE),
      "10.1000/test"
    )
  }
)


testthat::test_that(
  ".convert_pmid_to_pmcid_epmc() returns PMCID from first result",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(
          resultList = list(
            result = list(
              list(pmcid = "PMC123")
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmid_to_pmcid_epmc("12345", quiet = TRUE),
      "PMC123"
    )
  }
)


testthat::test_that(
  ".convert_doi_to_pmcid_epmc() returns PMCID from first result",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      epmc_search_bindings(
        result = list(
          resultList = list(
            result = list(
              list(pmcid = "PMC123")
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_doi_to_pmcid_epmc("10.1000/test", quiet = TRUE),
      "PMC123"
    )
  }
)


testthat::test_that(
  ".scholidonline_epmc_first_result() returns NULL on missing results",
  {
    testthat::expect_null(
      .scholidonline_epmc_first_result(
        x = list(resultList = list(result = NULL))
      )
    )
  }
)


testthat::test_that(
  ".scholidonline_epmc_first_result() returns first result record",
  {
    out <- .scholidonline_epmc_first_result(
      x = list(
        resultList = list(
          result = list(
            list(doi = "10.1000/a"),
            list(doi = "10.1000/b")
          )
        )
      )
    )
    
    testthat::expect_identical(
      out$doi,
      "10.1000/a"
    )
  }
)

# tests/testthat/test-providers_epmc.R

mock_scalar_chr_check <- function() {
  testthat::local_mocked_bindings(
    .scholidonline_check_scalar_chr = function(x) invisible(x)
  )
}

make_resp <- function(status = 200L, body = list()) {
  structure(
    list(status = status, body = body),
    class = "mock_resp"
  )
}

mock_httr2 <- function(resp = NULL) {
  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_error = function(req, is_error) req,
    req_perform = function(req) {
      if (is.null(resp)) {
        stop("boom")
      }
      resp
    },
    resp_status = function(resp) resp$status,
    resp_body_json = function(resp, simplifyVector = TRUE) {
      resp$body
    },
    .package = "httr2"
  )
}


testthat::test_that(
  ".meta_pmcid_epmc returns empty data.frame on failure states",
  {
    mock_scalar_chr_check()
    
    mock_httr2(resp = NULL)
    
    testthat::expect_no_warning(
      out <- .meta_pmcid_epmc("PMC123", quiet = TRUE)
    )
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_equal(nrow(out), 0L)
    
    mock_httr2(make_resp(status = 500L, body = list()))
    
    testthat::expect_no_warning(
      out <- .meta_pmcid_epmc("PMC123", quiet = TRUE)
    )
    testthat::expect_equal(nrow(out), 0L)
    
    body <- list(resultList = list(result = list()))
    mock_httr2(make_resp(body = body))
    
    out <- .meta_pmcid_epmc("PMC123", quiet = TRUE)
    testthat::expect_equal(nrow(out), 0L)
  }
)


testthat::test_that(
  ".scholidonline_epmc_first_result handles list and empty results",
  {
    x_list <- list(
      resultList = list(
        result = list(
          list(
            title = "B title",
            pubYear = "2021",
            journalTitle = "Journal Y",
            doi = "10.2000/abc",
            pmid = "456"
          ),
          list(
            title = "C title",
            pmid = "789"
          )
        )
      )
    )
    
    out_list <- .scholidonline_epmc_first_result(x_list)
    
    testthat::expect_equal(out_list$title, "B title")
    testthat::expect_equal(out_list$pubYear, "2021")
    testthat::expect_equal(out_list$journalTitle, "Journal Y")
    testthat::expect_equal(out_list$doi, "10.2000/abc")
    testthat::expect_equal(out_list$pmid, "456")
    
    x_empty <- list(resultList = list(result = list()))
    testthat::expect_null(.scholidonline_epmc_first_result(x_empty))
  }
)


testthat::test_that(
  ".convert_*_epmc return NA when search is NULL or field is missing",
  {
    mock_scalar_chr_check()
    
    testthat::local_mocked_bindings(
      .scholidonline_epmc_search = function(query, ..., quiet = FALSE) {
        NULL
      }
    )
    
    testthat::expect_true(is.na(
      .convert_pmid_to_doi_epmc("123", quiet = TRUE)
    ))
    testthat::expect_true(is.na(
      .convert_doi_to_pmid_epmc("10.1000/xyz", quiet = TRUE)
    ))
    testthat::expect_true(is.na(
      .convert_pmcid_to_pmid_epmc("PMC123", quiet = TRUE)
    ))
    testthat::expect_true(is.na(
      .convert_pmcid_to_doi_epmc("PMC123", quiet = TRUE)
    ))
    testthat::expect_true(is.na(
      .convert_pmid_to_pmcid_epmc("123", quiet = TRUE)
    ))
    testthat::expect_true(is.na(
      .convert_doi_to_pmcid_epmc("10.1000/xyz", quiet = TRUE)
    ))
    
    testthat::local_mocked_bindings(
      .scholidonline_epmc_search = function(query, ..., quiet = FALSE) {
        list(resultList = list(result = list(list())))
      },
      .scholidonline_epmc_first_result = function(x) {
        list(
          doi = "",
          pmid = "",
          pmcid = ""
        )
      }
    )
    
    testthat::expect_true(is.na(
      .convert_pmid_to_doi_epmc("123", quiet = TRUE)
    ))
    testthat::expect_true(is.na(
      .convert_doi_to_pmid_epmc("10.1000/xyz", quiet = TRUE)
    ))
    testthat::expect_true(is.na(
      .convert_pmcid_to_pmid_epmc("PMC123", quiet = TRUE)
    ))
    testthat::expect_true(is.na(
      .convert_pmcid_to_doi_epmc("PMC123", quiet = TRUE)
    ))
    testthat::expect_true(is.na(
      .convert_pmid_to_pmcid_epmc("123", quiet = TRUE)
    ))
    testthat::expect_true(is.na(
      .convert_doi_to_pmcid_epmc("10.1000/xyz", quiet = TRUE)
    ))
  }
)


testthat::test_that(
  ".scholidonline_epmc_first_result returns first result or NULL",
  {
    x <- list(
      resultList = list(
        result = list(
          list(pmid = "123"),
          list(pmid = "456")
        )
      )
    )
    
    out <- .scholidonline_epmc_first_result(x)
    testthat::expect_equal(out$pmid, "123")
    
    empty <- list(resultList = list(result = list()))
    testthat::expect_null(.scholidonline_epmc_first_result(empty))
  }
)