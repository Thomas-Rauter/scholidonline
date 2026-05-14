testthat::test_that(
  ".meta_pmid_ncbi_batch() rejects unused dots",
  {
    testthat::expect_error(
      .meta_pmid_ncbi_batch(
        x = "31452104",
        unused = TRUE,
        quiet = TRUE
      )
    )
  }
)

testthat::test_that(
  ".meta_pmid_ncbi_batch() rejects non-character input",
  {
    testthat::expect_error(
      .meta_pmid_ncbi_batch(
        x = 31452104,
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".meta_pmid_ncbi_batch() returns empty data.frame for no valid input",
  {
    out <- .meta_pmid_ncbi_batch(
      x = c(NA_character_, ""),
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
  ".meta_pmid_ncbi_batch() returns empty data.frame on request failure",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        NULL
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmid_ncbi_batch(
      x = "31452104",
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
  ".meta_pmid_ncbi_batch() returns empty data.frame on HTTP error",
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
    
    out <- .meta_pmid_ncbi_batch(
      x = "31452104",
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
  ".meta_pmid_ncbi_batch() returns empty data.frame without result",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list()
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmid_ncbi_batch(
      x = "31452104",
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
  ".meta_pmid_ncbi_batch() skips missing and error records",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list(
          result = list(
            `31452104` = list(
              error = "cannot get document summary"
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmid_ncbi_batch(
      x = c("31452104", "999999999"),
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
  ".meta_pmid_ncbi_batch() parses complete PMID metadata",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list(
          result = list(
            `31452104` = list(
              title = "Molegro Virtual Docker for Docking.",
              pubdate = "2019",
              source = "Methods Mol Biol",
              elocationid = "10.1007/978-1-4939-9752-7_19"
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmid_ncbi_batch(
      x = c("31452104", NA_character_),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      nrow(out),
      1L
    )
    
    testthat::expect_identical(
      out$pmid_key,
      "31452104"
    )
    
    testthat::expect_identical(
      out$title,
      "Molegro Virtual Docker for Docking."
    )
    
    testthat::expect_identical(
      out$year,
      2019L
    )
    
    testthat::expect_identical(
      out$container,
      "Methods Mol Biol"
    )
    
    testthat::expect_identical(
      out$doi,
      "10.1007/978-1-4939-9752-7_19"
    )
    
    testthat::expect_identical(
      out$pmid,
      "31452104"
    )
    
    testthat::expect_identical(
      out$pmcid,
      NA_character_
    )
    
    testthat::expect_identical(
      out$url,
      "https://pubmed.ncbi.nlm.nih.gov/31452104/"
    )
    
    testthat::expect_identical(
      out$provider,
      "ncbi"
    )
  }
)

testthat::test_that(
  ".meta_pmid_ncbi_batch() handles missing optional fields",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list(
          result = list(
            `31452104` = list()
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmid_ncbi_batch(
      x = "31452104",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      nrow(out),
      1L
    )
    
    testthat::expect_identical(
      out$title,
      NA_character_
    )
    
    testthat::expect_identical(
      out$year,
      NA_integer_
    )
    
    testthat::expect_identical(
      out$container,
      NA_character_
    )
    
    testthat::expect_identical(
      out$doi,
      NA_character_
    )
  }
)

testthat::test_that(
  ".meta_pmid_ncbi_batch() ignores non-DOI elocationid",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list(
          result = list(
            `31452104` = list(
              title = "Title",
              pubdate = "2019",
              source = "Journal",
              elocationid = "e12345"
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmid_ncbi_batch(
      x = "31452104",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out$doi,
      NA_character_
    )
  }
)


testthat::test_that(
  ".meta_pmcid_ncbi_batch() rejects unused dots",
  {
    testthat::expect_error(
      .meta_pmcid_ncbi_batch(
        x = "PMC6784763",
        unused = TRUE,
        quiet = TRUE
      )
    )
  }
)

testthat::test_that(
  ".meta_pmcid_ncbi_batch() rejects non-character input",
  {
    testthat::expect_error(
      .meta_pmcid_ncbi_batch(
        x = 6784763,
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".meta_pmcid_ncbi_batch() returns empty data.frame for no valid input",
  {
    out <- .meta_pmcid_ncbi_batch(
      x = c(NA_character_, ""),
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
  ".meta_pmcid_ncbi_batch() returns empty data.frame on request failure",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        NULL
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmcid_ncbi_batch(
      x = "PMC6784763",
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
  ".meta_pmcid_ncbi_batch() returns empty data.frame on HTTP error",
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
    
    out <- .meta_pmcid_ncbi_batch(
      x = "PMC6784763",
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
  ".meta_pmcid_ncbi_batch() returns empty data.frame on JSON parse failure",
  {
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
    
    out <- .meta_pmcid_ncbi_batch(
      x = "PMC6784763",
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
  ".meta_pmcid_ncbi_batch() returns empty data.frame without result",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list()
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmcid_ncbi_batch(
      x = "PMC6784763",
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
  ".meta_pmcid_ncbi_batch() skips missing and error records",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list(
          result = list(
            `6784763` = list(
              error = "cannot get document summary"
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmcid_ncbi_batch(
      x = c("PMC6784763", "PMC999999999"),
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
  ".meta_pmcid_ncbi_batch() skips records with no usable metadata",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list(
          result = list(
            `6784763` = list(
              title = "",
              source = "",
              pubdate = ""
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmcid_ncbi_batch(
      x = "PMC6784763",
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
  ".meta_pmcid_ncbi_batch() parses complete PMCID metadata",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list(
          result = list(
            `6784763` = list(
              title = paste(
                "Talc, Asbestos, and Epidemiology:",
                "Corporate Influence and Scientific Incognizance."
              ),
              pubdate = "2019 Nov",
              source = "Epidemiology",
              elocationid = "10.1097/EDE.0000000000001091",
              pmid = "31469695"
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmcid_ncbi_batch(
      x = c("PMC6784763", NA_character_),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      nrow(out),
      1L
    )
    
    testthat::expect_identical(
      out$pmcid_key,
      "PMC6784763"
    )
    
    testthat::expect_true(
      grepl("Talc, Asbestos", out$title)
    )
    
    testthat::expect_identical(
      out$year,
      2019L
    )
    
    testthat::expect_identical(
      out$container,
      "Epidemiology"
    )
    
    testthat::expect_identical(
      out$doi,
      "10.1097/EDE.0000000000001091"
    )
    
    testthat::expect_identical(
      out$pmid,
      "31469695"
    )
    
    testthat::expect_identical(
      out$pmcid,
      "PMC6784763"
    )
    
    testthat::expect_identical(
      out$url,
      "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6784763/"
    )
    
    testthat::expect_identical(
      out$provider,
      "ncbi"
    )
  }
)

testthat::test_that(
  ".meta_pmcid_ncbi_batch() handles missing optional fields",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list(
          result = list(
            `6784763` = list(
              title = "Title"
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmcid_ncbi_batch(
      x = "PMC6784763",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      nrow(out),
      1L
    )
    
    testthat::expect_identical(
      out$title,
      "Title"
    )
    
    testthat::expect_identical(
      out$year,
      NA_integer_
    )
    
    testthat::expect_identical(
      out$container,
      NA_character_
    )
    
    testthat::expect_identical(
      out$doi,
      NA_character_
    )
    
    testthat::expect_identical(
      out$pmid,
      NA_character_
    )
  }
)

testthat::test_that(
  ".meta_pmcid_ncbi_batch() ignores non-DOI elocationid",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        "response"
      },
      .scholidonline_resp_status = function(resp) {
        200L
      },
      .scholidonline_resp_body_json = function(resp, simplifyVector = TRUE) {
        list(
          result = list(
            `6784763` = list(
              title = "Title",
              pubdate = "2019",
              source = "Journal",
              elocationid = "e12345"
            )
          )
        )
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .meta_pmcid_ncbi_batch(
      x = "PMC6784763",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out$doi,
      NA_character_
    )
  }
)