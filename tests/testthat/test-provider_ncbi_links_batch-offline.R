testthat::test_that(
  ".links_ncbi_idconv_batch() rejects unused dots",
  {
    testthat::expect_error(
      .links_ncbi_idconv_batch(
        x = "31469695",
        query_type = "pmid",
        unused = TRUE,
        quiet = TRUE
      )
    )
  }
)

testthat::test_that(
  ".links_ncbi_idconv_batch() rejects invalid query_type",
  {
    testthat::expect_error(
      .links_ncbi_idconv_batch(
        x = "31469695",
        query_type = "doi",
        quiet = TRUE
      ),
      "`query_type` must be either \"pmid\" or \"pmcid\".",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".links_ncbi_idconv_batch() rejects non-character input",
  {
    testthat::expect_error(
      .links_ncbi_idconv_batch(
        x = 31469695,
        query_type = "pmid",
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".links_ncbi_idconv_batch() returns empty data.frame for no valid input",
  {
    out <- .links_ncbi_idconv_batch(
      x = c(NA_character_, ""),
      query_type = "pmid",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".links_ncbi_idconv_batch() returns empty data.frame on request failure",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        NULL
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .links_ncbi_idconv_batch(
      x = "31469695",
      query_type = "pmid",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".links_ncbi_idconv_batch() returns empty data.frame on HTTP error",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        500L
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .links_ncbi_idconv_batch(
      x = "31469695",
      query_type = "pmid",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".links_ncbi_idconv_batch() returns empty data.frame on JSON parse failure",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp) {
        stop("bad json", call. = FALSE)
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .links_ncbi_idconv_batch(
      x = "31469695",
      query_type = "pmid",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".links_ncbi_idconv_batch() returns empty data.frame without records",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp) {
        list(records = list())
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .links_ncbi_idconv_batch(
      x = "31469695",
      query_type = "pmid",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".links_ncbi_idconv_batch() parses PMID query records",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp) {
        list(
          records = list(
            list(
              pmid = "31469695",
              pmcid = "PMC6784763",
              doi = "10.1097/EDE.0000000000001091"
            ),
            list(
              requested_id = "999999999",
              status = "error"
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .links_ncbi_idconv_batch(
      x = c("31469695", "999999999"),
      query_type = "pmid",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out$query_id,
      c("31469695", "31469695", "31469695")
    )
    
    testthat::expect_identical(
      out$linked_type,
      c("pmid", "pmcid", "doi")
    )
    
    testthat::expect_identical(
      out$linked_value,
      c(
        "31469695",
        "PMC6784763",
        "10.1097/EDE.0000000000001091"
      )
    )
    
    testthat::expect_identical(
      out$provider,
      c("ncbi", "ncbi", "ncbi")
    )
  }
)

testthat::test_that(
  ".links_ncbi_idconv_batch() parses PMCID query records",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp) {
        list(
          records = list(
            list(
              pmid = "31469695",
              pmcid = "PMC6784763",
              doi = "10.1097/EDE.0000000000001091"
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .links_ncbi_idconv_batch(
      x = "PMC6784763",
      query_type = "pmcid",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out$query_id,
      c("PMC6784763", "PMC6784763", "PMC6784763")
    )
    
    testthat::expect_identical(
      out$linked_type,
      c("pmid", "pmcid", "doi")
    )
    
    testthat::expect_identical(
      out$linked_value,
      c(
        "31469695",
        "PMC6784763",
        "10.1097/EDE.0000000000001091"
      )
    )
  }
)

testthat::test_that(
  ".links_ncbi_idconv_batch() skips records without query key",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp) {
        list(
          records = list(
            list(
              doi = "10.1097/EDE.0000000000001091"
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .links_ncbi_idconv_batch(
      x = "31469695",
      query_type = "pmid",
      quiet = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      nrow(out),
      0L
    )
  }
)

testthat::test_that(
  ".links_pmid_ncbi_batch() forwards to shared helper",
  {
    testthat::local_mocked_bindings(
      .links_ncbi_idconv_batch = function(
    x,
    query_type,
    ...,
    quiet = FALSE
      ) {
        testthat::expect_identical(
          x,
          c("31469695", "999999999")
        )
        testthat::expect_identical(
          query_type,
          "pmid"
        )
        
        data.frame(
          query_id = "31469695",
          linked_type = "pmcid",
          linked_value = "PMC6784763",
          provider = "ncbi",
          stringsAsFactors = FALSE
        )
      }
    )
    
    out <- .links_pmid_ncbi_batch(
      c("31469695", "999999999"),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out$linked_value,
      "PMC6784763"
    )
  }
)

testthat::test_that(
  ".links_pmcid_ncbi_batch() forwards to shared helper",
  {
    testthat::local_mocked_bindings(
      .links_ncbi_idconv_batch = function(
    x,
    query_type,
    ...,
    quiet = FALSE
      ) {
        testthat::expect_identical(
          x,
          c("PMC6784763", "PMC999999999")
        )
        testthat::expect_identical(
          query_type,
          "pmcid"
        )
        
        data.frame(
          query_id = "PMC6784763",
          linked_type = "pmid",
          linked_value = "31469695",
          provider = "ncbi",
          stringsAsFactors = FALSE
        )
      }
    )
    
    out <- .links_pmcid_ncbi_batch(
      c("PMC6784763", "PMC999999999"),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out$linked_value,
      "31469695"
    )
  }
)