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


esummary_bindings <- function(result = NULL) {
  list(
    .scholidonline_esummary_pubmed = function(id, ..., quiet = FALSE) {
      result
    },
    .package = "scholidonline"
  )
}


pmc_idconv_bindings <- function(result = NULL) {
  list(
    .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
      result
    },
    .package = "scholidonline"
  )
}


ncbi_http_bindings <- function(
    resp = structure(list(), class = "fake_resp"),
    status = 200L,
    json = NULL,
    perform_null = FALSE
) {
  list(
    .scholidonline_request = function(url) {
      structure(list(url = url), class = "fake_req")
    },
    .scholidonline_req_url_query = function(req, ...) {
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
  ".exists_pmid_ncbi() returns NA on NULL response",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      esummary_bindings(result = NULL)
    )
    
    testthat::expect_identical(
      .exists_pmid_ncbi("12345", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".exists_pmid_ncbi() returns NA on missing result block",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      esummary_bindings(result = list(result = NULL))
    )
    
    testthat::expect_identical(
      .exists_pmid_ncbi("12345", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".exists_pmid_ncbi() returns FALSE when PMID is absent from uids",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      esummary_bindings(
        result = list(
          result = list(
            uids = c("99999")
          )
        )
      )
    )
    
    testthat::expect_false(
      .exists_pmid_ncbi("12345", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_pmid_ncbi() returns NA when record is NULL but uid is listed",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      esummary_bindings(
        result = list(
          result = list(
            uids = c("12345")
          )
        )
      )
    )
    
    testthat::expect_identical(
      .exists_pmid_ncbi("12345", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".exists_pmid_ncbi() returns FALSE on record error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      esummary_bindings(
        result = list(
          result = list(
            `12345` = list(
              error = "not found"
            )
          )
        )
      )
    )
    
    testthat::expect_false(
      .exists_pmid_ncbi("12345", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_pmid_ncbi() returns TRUE on exact uid match",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      esummary_bindings(
        result = list(
          result = list(
            `12345` = list(uid = "12345")
          )
        )
      )
    )
    
    testthat::expect_true(
      .exists_pmid_ncbi("12345", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_pmid_ncbi() returns FALSE on uid mismatch",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      esummary_bindings(
        result = list(
          result = list(
            `12345` = list(uid = "99999")
          )
        )
      )
    )
    
    testthat::expect_false(
      .exists_pmid_ncbi("12345", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_pmcid_ncbi() returns NA on missing records",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      pmc_idconv_bindings(result = list(records = NULL))
    )
    
    testthat::expect_identical(
      .exists_pmcid_ncbi("PMC123", quiet = TRUE),
      NA
    )
  }
)


testthat::test_that(
  ".exists_pmcid_ncbi() returns FALSE on error record",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      pmc_idconv_bindings(
        result = list(
          records = list(
            list(status = "error")
          )
        )
      )
    )
    
    testthat::expect_false(
      .exists_pmcid_ncbi("PMC123", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_pmcid_ncbi() returns TRUE when pmcid is present",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      pmc_idconv_bindings(
        result = list(
          records = list(
            list(pmcid = "PMC123")
          )
        )
      )
    )
    
    testthat::expect_true(
      .exists_pmcid_ncbi("PMC123", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".exists_pmcid_ncbi() returns FALSE when pmcid is empty",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      pmc_idconv_bindings(
        result = list(
          records = list(
            list(pmcid = "")
          )
        )
      )
    )
    
    testthat::expect_false(
      .exists_pmcid_ncbi("PMC123", quiet = TRUE)
    )
  }
)


testthat::test_that(
  ".links_pmid_ncbi() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .links_pmid_ncbi("12345", quiet = FALSE),
      "NCBI request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_pmid_ncbi() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .links_pmid_ncbi("12345", quiet = FALSE),
      "NCBI request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_pmid_ncbi() returns empty data.frame on NULL json",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = NULL
      )
    )
    
    testthat::expect_identical(
      .links_pmid_ncbi("12345", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_pmid_ncbi() returns empty data.frame on no records",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(records = list())
      )
    )
    
    testthat::expect_identical(
      .links_pmid_ncbi("12345", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".links_pmid_ncbi() returns linked identifiers",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          records = list(
            list(
              pmid = "12345",
              pmcid = "PMC123",
              doi = "10.1000/test"
            )
          )
        )
      )
    )
    
    out <- .links_pmid_ncbi("12345", quiet = TRUE)
    
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
      c("ncbi", "ncbi", "ncbi")
    )
  }
)


testthat::test_that(
  ".links_pmcid_ncbi() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .links_pmcid_ncbi("PMC123", quiet = FALSE),
      "NCBI request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_pmcid_ncbi() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .links_pmcid_ncbi("PMC123", quiet = FALSE),
      "NCBI request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".links_pmcid_ncbi() returns linked identifiers",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          records = list(
            list(
              pmid = "12345",
              pmcid = "PMC123",
              doi = "10.1000/test"
            )
          )
        )
      )
    )
    
    out <- .links_pmcid_ncbi("PMC123", quiet = TRUE)
    
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
      c("ncbi", "ncbi", "ncbi")
    )
  }
)


testthat::test_that(
  ".meta_pmid_ncbi() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .meta_pmid_ncbi("12345", quiet = FALSE),
      "NCBI request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_pmid_ncbi() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .meta_pmid_ncbi("12345", quiet = FALSE),
      "NCBI request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_pmid_ncbi() returns empty data.frame when record is missing",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          result = list()
        )
      )
    )
    
    testthat::expect_identical(
      .meta_pmid_ncbi("12345", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_pmid_ncbi() returns harmonized metadata",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          result = list(
            `12345` = list(
              title = "Paper title",
              pubdate = "2021 Dec",
              source = "Journal Name",
              elocationid = "10.1000/test"
            )
          )
        )
      )
    )
    
    out <- .meta_pmid_ncbi("12345", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$title, "Paper title")
    testthat::expect_identical(out$year, 2021L)
    testthat::expect_identical(out$container, "Journal Name")
    testthat::expect_identical(out$doi, "10.1000/test")
    testthat::expect_identical(out$pmid, "12345")
    testthat::expect_true(is.na(out$pmcid))
    testthat::expect_identical(
      out$url,
      "https://pubmed.ncbi.nlm.nih.gov/12345/"
    )
    testthat::expect_identical(out$provider, "ncbi")
  }
)


testthat::test_that(
  ".meta_pmid_ncbi() sets DOI to NA when elocationid is not a DOI",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          result = list(
            `12345` = list(
              title = "Paper title",
              pubdate = "2021 Dec",
              source = "Journal Name",
              elocationid = "e123456"
            )
          )
        )
      )
    )
    
    out <- .meta_pmid_ncbi("12345", quiet = TRUE)
    
    testthat::expect_true(is.na(out$doi))
  }
)


testthat::test_that(
  ".meta_pmcid_ncbi() returns empty data.frame on request failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .meta_pmcid_ncbi("PMC123", quiet = FALSE),
      "NCBI request failed\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_pmcid_ncbi() returns empty data.frame on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(status = 502L)
    )
    
    testthat::expect_warning(
      out <- .meta_pmcid_ncbi("PMC123", quiet = FALSE),
      "NCBI request returned HTTP 502\\."
    )
    
    testthat::expect_identical(out, empty_df())
  }
)


testthat::test_that(
  ".meta_pmcid_ncbi() returns empty data.frame when record is missing",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          result = list()
        )
      )
    )
    
    testthat::expect_identical(
      .meta_pmcid_ncbi("PMC123", quiet = TRUE),
      empty_df()
    )
  }
)


testthat::test_that(
  ".meta_pmcid_ncbi() returns harmonized metadata",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          result = list(
            `123` = list(
              title = "Paper title",
              pubdate = "2020 Jan",
              source = "Journal Name",
              elocationid = "10.1000/test",
              pmid = "99999"
            )
          )
        )
      )
    )
    
    out <- .meta_pmcid_ncbi("PMC123", quiet = TRUE)
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(out$title, "Paper title")
    testthat::expect_identical(out$year, 2020L)
    testthat::expect_identical(out$container, "Journal Name")
    testthat::expect_identical(out$doi, "10.1000/test")
    testthat::expect_identical(out$pmid, "99999")
    testthat::expect_identical(out$pmcid, "PMC123")
    testthat::expect_identical(
      out$url,
      "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC123/"
    )
    testthat::expect_identical(out$provider, "ncbi")
  }
)


testthat::test_that(
  ".meta_pmcid_ncbi() sets DOI to NA when elocationid is not a DOI",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          result = list(
            `123` = list(
              title = "Paper title",
              pubdate = "2020 Jan",
              source = "Journal Name",
              elocationid = "e123456",
              pmid = "99999"
            )
          )
        )
      )
    )
    
    out <- .meta_pmcid_ncbi("PMC123", quiet = TRUE)
    
    testthat::expect_true(is.na(out$doi))
  }
)


testthat::test_that(
  ".convert_pmid_to_doi_ncbi() returns DOI from data.frame articleids",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          result = list(
            `12345` = list(
              articleids = data.frame(
                idtype = c("pubmed", "doi"),
                value = c("12345", "10.1000/test"),
                stringsAsFactors = FALSE
              )
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmid_to_doi_ncbi("12345", quiet = TRUE),
      "10.1000/test"
    )
  }
)


testthat::test_that(
  ".convert_pmid_to_doi_ncbi() returns DOI from list articleids",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          result = list(
            `12345` = list(
              articleids = list(
                list(
                  idtype = "pubmed",
                  value = "12345"
                ),
                list(
                  idtype = "doi",
                  value = "10.1000/test"
                )
              )
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmid_to_doi_ncbi("12345", quiet = TRUE),
      "10.1000/test"
    )
  }
)


testthat::test_that(
  ".convert_pmid_to_doi_ncbi() returns NA when DOI is absent",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          result = list(
            `12345` = list(
              articleids = data.frame(
                idtype = "pubmed",
                value = "12345",
                stringsAsFactors = FALSE
              )
            )
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmid_to_doi_ncbi("12345", quiet = TRUE),
      NA_character_
    )
  }
)


testthat::test_that(
  ".convert_pmid_to_doi_ncbi() warns and returns NA on failure",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(perform_null = TRUE)
    )
    
    testthat::expect_warning(
      out <- .convert_pmid_to_doi_ncbi("12345", quiet = FALSE),
      "NCBI request failed\\."
    )
    
    testthat::expect_identical(out, NA_character_)
  }
)


testthat::test_that(
  ".convert_doi_to_pmid_ncbi() returns first PMID hit",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          esearchresult = list(
            idlist = list("12345", "99999")
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_doi_to_pmid_ncbi("10.1000/test", quiet = TRUE),
      "12345"
    )
  }
)


testthat::test_that(
  ".convert_doi_to_pmid_ncbi() returns NA on empty idlist",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          esearchresult = list(
            idlist = list()
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_doi_to_pmid_ncbi("10.1000/test", quiet = TRUE),
      NA_character_
    )
  }
)


testthat::test_that(
  ".convert_doi_to_pmid_ncbi() warns and returns NA on HTTP error",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(status = 500L)
    )
    
    testthat::expect_warning(
      out <- .convert_doi_to_pmid_ncbi("10.1000/test", quiet = FALSE),
      "NCBI request returned HTTP 500\\."
    )
    
    testthat::expect_identical(out, NA_character_)
  }
)


testthat::test_that(
  ".convert_pmcid_to_pmid_ncbi() returns PMID from idconv record",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          records = list(
            list(pmid = "12345")
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmcid_to_pmid_ncbi("PMC123", quiet = TRUE),
      "12345"
    )
  }
)


testthat::test_that(
  ".convert_pmcid_to_pmid_ncbi() returns NA for missing PMID",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          records = list(
            list(pmid = "")
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmcid_to_pmid_ncbi("PMC123", quiet = TRUE),
      NA_character_
    )
  }
)


testthat::test_that(
  ".convert_pmcid_to_doi_ncbi() returns DOI from idconv record",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          records = list(
            list(doi = "10.1000/test")
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmcid_to_doi_ncbi("PMC123", quiet = TRUE),
      "10.1000/test"
    )
  }
)


testthat::test_that(
  ".convert_pmcid_to_doi_ncbi() returns NA for missing DOI",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          records = list(
            list(doi = "")
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmcid_to_doi_ncbi("PMC123", quiet = TRUE),
      NA_character_
    )
  }
)


testthat::test_that(
  ".convert_pmid_to_pmcid_ncbi() returns PMCID from idconv record",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          records = list(
            list(pmcid = "PMC123")
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmid_to_pmcid_ncbi("12345", quiet = TRUE),
      "PMC123"
    )
  }
)


testthat::test_that(
  ".convert_pmid_to_pmcid_ncbi() returns NA for missing PMCID",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          records = list(
            list(pmcid = "")
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_pmid_to_pmcid_ncbi("12345", quiet = TRUE),
      NA_character_
    )
  }
)


testthat::test_that(
  ".convert_doi_to_pmcid_ncbi() returns PMCID from idconv record",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          records = list(
            list(pmcid = "PMC123")
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_doi_to_pmcid_ncbi("10.1000/test", quiet = TRUE),
      "PMC123"
    )
  }
)


testthat::test_that(
  ".convert_doi_to_pmcid_ncbi() returns NA for missing PMCID",
  {
    do.call(
      testthat::local_mocked_bindings,
      scalar_check_bindings()
    )
    do.call(
      testthat::local_mocked_bindings,
      ncbi_http_bindings(
        status = 200L,
        json = list(
          records = list(
            list(pmcid = "")
          )
        )
      )
    )
    
    testthat::expect_identical(
      .convert_doi_to_pmcid_ncbi("10.1000/test", quiet = TRUE),
      NA_character_
    )
  }
)