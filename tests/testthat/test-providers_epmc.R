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

epmc_live_fixture <- function() {
  skip_if_no_internet_for_live_tests()
  
  pmcid <- "PMC2808187"
  out <- .meta_pmcid_epmc(pmcid, quiet = TRUE)
  
  testthat::skip_if_not(
    nrow(out) == 1L,
    message = "Europe PMC fixture article not available."
  )
  
  pmid <- out$pmid[[1]]
  doi <- out$doi[[1]]
  
  testthat::skip_if_not(
    is.character(pmid) && length(pmid) == 1L &&
      !is.na(pmid) && nzchar(pmid),
    message = "Fixture PMID unavailable."
  )
  
  testthat::skip_if_not(
    is.character(doi) && length(doi) == 1L &&
      !is.na(doi) && nzchar(doi),
    message = "Fixture DOI unavailable."
  )
  
  list(
    pmcid = pmcid,
    pmid = pmid,
    doi = doi
  )
}

testthat::test_that(".meta_pmcid_epmc returns expected live metadata", {
  ids <- epmc_live_fixture()
  
  out <- .meta_pmcid_epmc(ids$pmcid, quiet = TRUE)
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_true(nzchar(out$title))
  testthat::expect_true(!is.na(out$year))
  testthat::expect_true(is.numeric(out$year) || is.integer(out$year))
  testthat::expect_equal(out$pmcid, ids$pmcid)
  testthat::expect_true(is.na(out$pmid) || identical(out$pmid, ids$pmid))
  testthat::expect_true(is.na(out$doi) || identical(out$doi, ids$doi))
  testthat::expect_match(out$url, "europepmc.org/article/PMC/")
  testthat::expect_equal(out$provider, "epmc")
})

testthat::test_that(".convert_*_epmc work for a known live article", {
  ids <- epmc_live_fixture()
  
  testthat::expect_equal(
    .convert_pmid_to_doi_epmc(ids$pmid, quiet = TRUE),
    ids$doi
  )
  
  testthat::expect_equal(
    .convert_doi_to_pmid_epmc(ids$doi, quiet = TRUE),
    ids$pmid
  )
  
  testthat::expect_equal(
    .convert_pmcid_to_pmid_epmc(ids$pmcid, quiet = TRUE),
    ids$pmid
  )
  
  testthat::expect_equal(
    .convert_pmcid_to_doi_epmc(ids$pmcid, quiet = TRUE),
    ids$doi
  )
  
  testthat::expect_equal(
    .convert_pmid_to_pmcid_epmc(ids$pmid, quiet = TRUE),
    ids$pmcid
  )
  
  testthat::expect_equal(
    .convert_doi_to_pmcid_epmc(ids$doi, quiet = TRUE),
    ids$pmcid
  )
})

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