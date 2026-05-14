testthat::test_that(
  "NCBI link batch helper warns on request failure and HTTP error",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        NULL
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    testthat::expect_warning(
      out <- .links_ncbi_idconv_batch(
        x = "31469695",
        query_type = "pmid",
        quiet = FALSE
      ),
      "NCBI request failed.",
      fixed = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(nrow(out), 0L)
    
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
    
    testthat::expect_warning(
      out <- .links_ncbi_idconv_batch(
        x = "31469695",
        query_type = "pmid",
        quiet = FALSE
      ),
      "NCBI request returned HTTP 500.",
      fixed = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(nrow(out), 0L)
  }
)

testthat::test_that(
  "NCBI PMID metadata batch helper warns on request failure and HTTP error",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        NULL
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    testthat::expect_warning(
      out <- .meta_pmid_ncbi_batch(
        x = "31452104",
        quiet = FALSE
      ),
      "NCBI request failed.",
      fixed = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(nrow(out), 0L)
    
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
    
    testthat::expect_warning(
      out <- .meta_pmid_ncbi_batch(
        x = "31452104",
        quiet = FALSE
      ),
      "NCBI request returned HTTP 500.",
      fixed = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(nrow(out), 0L)
  }
)

testthat::test_that(
  "NCBI PMCID metadata batch helper warns on request failure, HTTP error, and JSON parse failure",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        NULL
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    testthat::expect_warning(
      out <- .meta_pmcid_ncbi_batch(
        x = "PMC6784763",
        quiet = FALSE
      ),
      "NCBI request failed.",
      fixed = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(nrow(out), 0L)
    
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
    
    testthat::expect_warning(
      out <- .meta_pmcid_ncbi_batch(
        x = "PMC6784763",
        quiet = FALSE
      ),
      "NCBI request returned HTTP 500.",
      fixed = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(nrow(out), 0L)
    
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        stop("bad json", call. = FALSE)
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    testthat::expect_warning(
      out <- .meta_pmcid_ncbi_batch(
        x = "PMC6784763",
        quiet = FALSE
      ),
      "NCBI response could not be parsed as JSON.",
      fixed = TRUE
    )
    
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(nrow(out), 0L)
  }
)

testthat::test_that(
  "NCBI DOI to PMID scalar helper warns on request failure and JSON parse failure",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        NULL
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    testthat::expect_warning(
      out <- .convert_doi_to_pmid_ncbi(
        x = "10.1097/EDE.0000000000001091",
        quiet = FALSE
      ),
      "NCBI request failed.",
      fixed = TRUE
    )
    
    testthat::expect_identical(out, NA_character_)
    
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(
    resp,
    simplifyVector = FALSE
      ) {
        stop("bad json", call. = FALSE)
      },
    .ncbi_rate_limit = function(quiet = FALSE) {
      invisible(NULL)
    }
    )
    
    testthat::expect_warning(
      out <- .convert_doi_to_pmid_ncbi(
        x = "10.1097/EDE.0000000000001091",
        quiet = FALSE
      ),
      "NCBI response could not be parsed as JSON.",
      fixed = TRUE
    )
    
    testthat::expect_identical(out, NA_character_)
  }
)

testthat::test_that(
  "NCBI DOI to PMID batch helper warns on request failure, HTTP error, and JSON parse failure",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        NULL
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    testthat::expect_warning(
      out <- .convert_doi_to_pmid_ncbi_batch(
        x = "10.1097/EDE.0000000000001091",
        quiet = FALSE
      ),
      "NCBI request failed.",
      fixed = TRUE
    )
    
    testthat::expect_identical(out, NA_character_)
    
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
    
    testthat::expect_warning(
      out <- .convert_doi_to_pmid_ncbi_batch(
        x = "10.1097/EDE.0000000000001091",
        quiet = FALSE
      ),
      "NCBI request returned HTTP 500.",
      fixed = TRUE
    )
    
    testthat::expect_identical(out, NA_character_)
    
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(
    resp,
    simplifyVector = FALSE
      ) {
        stop("bad json", call. = FALSE)
      },
    .ncbi_rate_limit = function(quiet = FALSE) {
      invisible(NULL)
    }
    )
    
    testthat::expect_warning(
      out <- .convert_doi_to_pmid_ncbi_batch(
        x = "10.1097/EDE.0000000000001091",
        quiet = FALSE
      ),
      "NCBI response could not be parsed as JSON.",
      fixed = TRUE
    )
    
    testthat::expect_identical(out, NA_character_)
  }
)

testthat::test_that(
  "NCBI scalar ID converter helpers warn on request failure and HTTP error",
  {
    helpers <- list(
      .convert_pmcid_to_pmid_ncbi = list(
        fun = .convert_pmcid_to_pmid_ncbi,
        x = "PMC6784763"
      ),
      .convert_pmcid_to_doi_ncbi = list(
        fun = .convert_pmcid_to_doi_ncbi,
        x = "PMC6784763"
      ),
      .convert_pmid_to_pmcid_ncbi = list(
        fun = .convert_pmid_to_pmcid_ncbi,
        x = "31469695"
      ),
      .convert_doi_to_pmcid_ncbi = list(
        fun = .convert_doi_to_pmcid_ncbi,
        x = "10.1097/EDE.0000000000001091"
      )
    )
    
    for (helper in helpers) {
      testthat::local_mocked_bindings(
        .scholidonline_req_perform_safe = function(req) {
          NULL
        },
        .ncbi_rate_limit = function(quiet = FALSE) {
          invisible(NULL)
        }
      )
      
      testthat::expect_warning(
        out <- helper$fun(
          x = helper$x,
          quiet = FALSE
        ),
        "NCBI request failed.",
        fixed = TRUE
      )
      
      testthat::expect_identical(out, NA_character_)
      
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
      
      testthat::expect_warning(
        out <- helper$fun(
          x = helper$x,
          quiet = FALSE
        ),
        "NCBI request returned HTTP 500.",
        fixed = TRUE
      )
      
      testthat::expect_identical(out, NA_character_)
    }
  }
)