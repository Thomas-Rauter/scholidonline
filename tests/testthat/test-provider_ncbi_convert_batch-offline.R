testthat::test_that(
  ".convert_pmid_to_doi_ncbi_batch() rejects non-character input",
  {
    testthat::expect_error(
      .convert_pmid_to_doi_ncbi_batch(
        x = 31469695,
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".convert_pmid_to_doi_ncbi_batch() returns NA for no valid input",
  {
    out <- .convert_pmid_to_doi_ncbi_batch(
      x = c(NA_character_, ""),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_pmid_to_doi_ncbi_batch() returns NA when NCBI fails",
  {
    testthat::local_mocked_bindings(
      .scholidonline_esummary_pubmed = function(id, ..., quiet = FALSE) {
        NULL
      }
    )
    
    out <- .convert_pmid_to_doi_ncbi_batch(
      x = c("31469695", "999999999", NA_character_),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_pmid_to_doi_ncbi_batch() returns NA without result block",
  {
    testthat::local_mocked_bindings(
      .scholidonline_esummary_pubmed = function(id, ..., quiet = FALSE) {
        list()
      }
    )
    
    out <- .convert_pmid_to_doi_ncbi_batch(
      x = c("31469695", "999999999"),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_pmid_to_doi_ncbi_batch() parses DOI values in input order",
  {
    testthat::local_mocked_bindings(
      .scholidonline_esummary_pubmed = function(id, ..., quiet = FALSE) {
        testthat::expect_identical(
          id,
          "31469695,999999999"
        )
        
        list(
          result = list(
            `31469695` = list(
              articleids = list(
                list(
                  idtype = "pubmed",
                  value = "31469695"
                ),
                list(
                  idtype = "doi",
                  value = "10.1097/EDE.0000000000001091"
                )
              )
            ),
            `999999999` = list(
              articleids = list(
                list(
                  idtype = "pubmed",
                  value = "999999999"
                )
              )
            )
          )
        )
      }
    )
    
    out <- .convert_pmid_to_doi_ncbi_batch(
      x = c("31469695", "999999999", NA_character_),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(
        "10.1097/EDE.0000000000001091",
        NA_character_,
        NA_character_
      )
    )
  }
)

testthat::test_that(
  ".convert_pmid_to_doi_ncbi_batch() skips missing and error records",
  {
    testthat::local_mocked_bindings(
      .scholidonline_esummary_pubmed = function(id, ..., quiet = FALSE) {
        list(
          result = list(
            `31469695` = list(
              error = "cannot get document summary"
            )
          )
        )
      }
    )
    
    out <- .convert_pmid_to_doi_ncbi_batch(
      x = c("31469695", "999999999"),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_pmid_to_doi_ncbi_batch() parses data.frame articleids",
  {
    testthat::local_mocked_bindings(
      .scholidonline_esummary_pubmed = function(id, ..., quiet = FALSE) {
        list(
          result = list(
            `31469695` = list(
              articleids = data.frame(
                idtype = c("pubmed", "doi"),
                value = c(
                  "31469695",
                  "10.1097/EDE.0000000000001091"
                ),
                stringsAsFactors = FALSE
              )
            )
          )
        )
      }
    )
    
    out <- .convert_pmid_to_doi_ncbi_batch(
      x = "31469695",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      "10.1097/EDE.0000000000001091"
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() rejects non-character input",
  {
    testthat::expect_error(
      .convert_doi_to_pmid_ncbi_batch(
        x = 1,
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() returns NA for no valid input",
  {
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = c(NA_character_, ""),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() returns NA on ESearch request failure",
  {
    testthat::local_mocked_bindings(
      .scholidonline_req_perform_safe = function(req) {
        NULL
      },
      .ncbi_rate_limit = function(quiet = FALSE) {
        invisible(NULL)
      }
    )
    
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = c("10.1097/EDE.0000000000001091", "10.0000/not-real"),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() returns NA on ESearch HTTP error",
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
    
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = "10.1097/EDE.0000000000001091",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      NA_character_
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() returns NA on ESearch JSON parse failure",
  {
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
    
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = "10.1097/EDE.0000000000001091",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      NA_character_
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() returns NA when ESearch finds no PMIDs",
  {
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
        list(
          esearchresult = list(
            idlist = list()
          )
        )
      },
    .ncbi_rate_limit = function(quiet = FALSE) {
      invisible(NULL)
    }
    )
    
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = "10.1097/EDE.0000000000001091",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      NA_character_
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() returns NA when ESummary fails",
  {
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
        list(
          esearchresult = list(
            idlist = list("31469695")
          )
        )
      },
    .scholidonline_esummary_pubmed = function(
    id,
    ...,
    quiet = FALSE
    ) {
      NULL
    },
    .ncbi_rate_limit = function(quiet = FALSE) {
      invisible(NULL)
    }
    )
    
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = "10.1097/EDE.0000000000001091",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      NA_character_
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() returns NA when ESummary has no result",
  {
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
        list(
          esearchresult = list(
            idlist = list("31469695")
          )
        )
      },
    .scholidonline_esummary_pubmed = function(
    id,
    ...,
    quiet = FALSE
    ) {
      list()
    },
    .ncbi_rate_limit = function(quiet = FALSE) {
      invisible(NULL)
    }
    )
    
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = "10.1097/EDE.0000000000001091",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      NA_character_
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() maps DOI to PMID through ESummary article IDs",
  {
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
        list(
          esearchresult = list(
            idlist = list("31469695")
          )
        )
      },
    .scholidonline_esummary_pubmed = function(
    id,
    ...,
    quiet = FALSE
    ) {
      testthat::expect_identical(
        id,
        "31469695"
      )
      
      list(
        result = list(
          `31469695` = list(
            articleids = list(
              list(
                idtype = "pubmed",
                value = "31469695"
              ),
              list(
                idtype = "doi",
                value = "10.1097/EDE.0000000000001091"
              )
            )
          )
        )
      )
    },
    .ncbi_rate_limit = function(quiet = FALSE) {
      invisible(NULL)
    }
    )
    
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = c(
        "10.1097/EDE.0000000000001091",
        "10.0000/not-real",
        NA_character_
      ),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c("31469695", NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() matches DOI case-insensitively",
  {
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
        list(
          esearchresult = list(
            idlist = list("31469695")
          )
        )
      },
    .scholidonline_esummary_pubmed = function(
    id,
    ...,
    quiet = FALSE
    ) {
      list(
        result = list(
          `31469695` = list(
            articleids = list(
              list(
                idtype = "doi",
                value = "10.1097/ede.0000000000001091"
              )
            )
          )
        )
      )
    },
    .ncbi_rate_limit = function(quiet = FALSE) {
      invisible(NULL)
    }
    )
    
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = "10.1097/EDE.0000000000001091",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      "31469695"
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmid_ncbi_batch() skips missing and error ESummary records",
  {
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
        list(
          esearchresult = list(
            idlist = list("31469695", "999999999")
          )
        )
      },
    .scholidonline_esummary_pubmed = function(
    id,
    ...,
    quiet = FALSE
    ) {
      list(
        result = list(
          `31469695` = list(
            error = "cannot get document summary"
          )
        )
      )
    },
    .ncbi_rate_limit = function(quiet = FALSE) {
      invisible(NULL)
    }
    )
    
    out <- .convert_doi_to_pmid_ncbi_batch(
      x = c(
        "10.1097/EDE.0000000000001091",
        "10.0000/not-real"
      ),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_)
    )
  }
)


testthat::test_that(
  ".convert_*_ncbi_batch() wrappers forward to shared ID converter helper",
  {
    testthat::local_mocked_bindings(
      .convert_ncbi_idconv_batch = function(
    x,
    from,
    to,
    ...,
    quiet = FALSE
      ) {
        testthat::expect_identical(
          x,
          c("31469695", "999999999")
        )
        testthat::expect_identical(from, "pmid")
        testthat::expect_identical(to, "pmcid")
        testthat::expect_identical(quiet, TRUE)
        
        c("PMC6784763", NA_character_)
      }
    )
    
    out <- .convert_pmid_to_pmcid_ncbi_batch(
      c("31469695", "999999999"),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c("PMC6784763", NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_pmcid_to_pmid_ncbi_batch() forwards to shared ID converter helper",
  {
    testthat::local_mocked_bindings(
      .convert_ncbi_idconv_batch = function(
    x,
    from,
    to,
    ...,
    quiet = FALSE
      ) {
        testthat::expect_identical(
          x,
          c("PMC6784763", "PMC999999999")
        )
        testthat::expect_identical(from, "pmcid")
        testthat::expect_identical(to, "pmid")
        testthat::expect_identical(quiet, TRUE)
        
        c("31469695", NA_character_)
      }
    )
    
    out <- .convert_pmcid_to_pmid_ncbi_batch(
      c("PMC6784763", "PMC999999999"),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c("31469695", NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_pmcid_to_doi_ncbi_batch() forwards to shared ID converter helper",
  {
    testthat::local_mocked_bindings(
      .convert_ncbi_idconv_batch = function(
    x,
    from,
    to,
    ...,
    quiet = FALSE
      ) {
        testthat::expect_identical(
          x,
          c("PMC6784763", "PMC999999999")
        )
        testthat::expect_identical(from, "pmcid")
        testthat::expect_identical(to, "doi")
        testthat::expect_identical(quiet, TRUE)
        
        c("10.1097/EDE.0000000000001091", NA_character_)
      }
    )
    
    out <- .convert_pmcid_to_doi_ncbi_batch(
      c("PMC6784763", "PMC999999999"),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c("10.1097/EDE.0000000000001091", NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_doi_to_pmcid_ncbi_batch() forwards to shared ID converter helper",
  {
    testthat::local_mocked_bindings(
      .convert_ncbi_idconv_batch = function(
    x,
    from,
    to,
    ...,
    quiet = FALSE
      ) {
        testthat::expect_identical(
          x,
          c("10.1097/EDE.0000000000001091", "10.0000/not-real")
        )
        testthat::expect_identical(from, "doi")
        testthat::expect_identical(to, "pmcid")
        testthat::expect_identical(quiet, TRUE)
        
        c("PMC6784763", NA_character_)
      }
    )
    
    out <- .convert_doi_to_pmcid_ncbi_batch(
      c("10.1097/EDE.0000000000001091", "10.0000/not-real"),
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c("PMC6784763", NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_ncbi_articleids_to_doi() handles NULL and malformed data.frame input",
  {
    testthat::expect_identical(
      .convert_ncbi_articleids_to_doi(NULL),
      NA_character_
    )
    
    testthat::expect_identical(
      .convert_ncbi_articleids_to_doi(
        data.frame(
          idtype = "pubmed",
          value = "31469695",
          stringsAsFactors = FALSE
        )
      ),
      NA_character_
    )
    
    testthat::expect_identical(
      .convert_ncbi_articleids_to_doi(
        data.frame(
          idtype = "doi",
          stringsAsFactors = FALSE
        )
      ),
      NA_character_
    )
  }
)

testthat::test_that(
  ".convert_ncbi_articleids_to_doi() handles empty DOI values",
  {
    testthat::expect_identical(
      .convert_ncbi_articleids_to_doi(
        data.frame(
          idtype = "doi",
          value = "",
          stringsAsFactors = FALSE
        )
      ),
      NA_character_
    )
    
    testthat::expect_identical(
      .convert_ncbi_articleids_to_doi(
        list(
          list(
            idtype = "doi",
            value = ""
          )
        )
      ),
      NA_character_
    )
  }
)

testthat::test_that(
  ".convert_ncbi_articleids_to_doi() returns NA when list has no DOI",
  {
    testthat::expect_identical(
      .convert_ncbi_articleids_to_doi(
        list(
          list(
            idtype = "pubmed",
            value = "31469695"
          )
        )
      ),
      NA_character_
    )
  }
)

testthat::test_that(
  ".convert_ncbi_idconv_batch() rejects invalid inputs",
  {
    testthat::expect_error(
      .convert_ncbi_idconv_batch(
        x = 1,
        from = "pmid",
        to = "pmcid",
        quiet = TRUE
      ),
      "`x` must be a character vector.",
      fixed = TRUE
    )
    
    testthat::expect_error(
      .convert_ncbi_idconv_batch(
        x = "31469695",
        from = "bad",
        to = "pmcid",
        quiet = TRUE
      ),
      "`from` must be one of \"pmid\", \"pmcid\", or \"doi\".",
      fixed = TRUE
    )
    
    testthat::expect_error(
      .convert_ncbi_idconv_batch(
        x = "31469695",
        from = "pmid",
        to = "bad",
        quiet = TRUE
      ),
      "`to` must be one of \"pmid\", \"pmcid\", or \"doi\".",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  ".convert_ncbi_idconv_batch() returns NA for no valid input",
  {
    out <- .convert_ncbi_idconv_batch(
      x = c(NA_character_, ""),
      from = "pmid",
      to = "pmcid",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_ncbi_idconv_batch() returns NA when NCBI fails",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        NULL
      }
    )
    
    out <- .convert_ncbi_idconv_batch(
      x = c("31469695", "999999999", NA_character_),
      from = "pmid",
      to = "pmcid",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_ncbi_idconv_batch() returns NA without records",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        list(records = list())
      }
    )
    
    out <- .convert_ncbi_idconv_batch(
      x = c("31469695", "999999999"),
      from = "pmid",
      to = "pmcid",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_ncbi_idconv_batch() maps PMID to PMCID in input order",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        testthat::expect_identical(
          ids,
          "31469695,999999999"
        )
        
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
      }
    )
    
    out <- .convert_ncbi_idconv_batch(
      x = c("31469695", "999999999", NA_character_),
      from = "pmid",
      to = "pmcid",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c("PMC6784763", NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_ncbi_idconv_batch() maps PMCID to PMID and DOI",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        list(
          records = list(
            list(
              pmid = "31469695",
              pmcid = "PMC6784763",
              doi = "10.1097/EDE.0000000000001091"
            )
          )
        )
      }
    )
    
    out_pmid <- .convert_ncbi_idconv_batch(
      x = "PMC6784763",
      from = "pmcid",
      to = "pmid",
      quiet = TRUE
    )
    
    out_doi <- .convert_ncbi_idconv_batch(
      x = "PMC6784763",
      from = "pmcid",
      to = "doi",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out_pmid,
      "31469695"
    )
    
    testthat::expect_identical(
      out_doi,
      "10.1097/EDE.0000000000001091"
    )
  }
)

testthat::test_that(
  ".convert_ncbi_idconv_batch() maps DOI to PMCID case-insensitively",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        list(
          records = list(
            list(
              pmid = "31469695",
              pmcid = "PMC6784763",
              doi = "10.1097/ede.0000000000001091"
            )
          )
        )
      }
    )
    
    out <- .convert_ncbi_idconv_batch(
      x = "10.1097/EDE.0000000000001091",
      from = "doi",
      to = "pmcid",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      "PMC6784763"
    )
  }
)

testthat::test_that(
  ".convert_ncbi_idconv_batch() leaves unmatched and empty target values as NA",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        list(
          records = list(
            list(
              pmid = "31469695",
              pmcid = "",
              doi = "10.1097/EDE.0000000000001091"
            )
          )
        )
      }
    )
    
    out <- .convert_ncbi_idconv_batch(
      x = c("31469695", "999999999"),
      from = "pmid",
      to = "pmcid",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      c(NA_character_, NA_character_)
    )
  }
)

testthat::test_that(
  ".convert_ncbi_idconv_record_value() handles missing values",
  {
    testthat::expect_identical(
      .convert_ncbi_idconv_record_value(
        rec = list(),
        type = "pmid"
      ),
      NA_character_
    )
    
    testthat::expect_identical(
      .convert_ncbi_idconv_record_value(
        rec = list(pmid = NA_character_),
        type = "pmid"
      ),
      NA_character_
    )
    
    testthat::expect_identical(
      .convert_ncbi_idconv_record_value(
        rec = list(pmid = "31469695"),
        type = "unknown"
      ),
      NA_character_
    )
  }
)

testthat::test_that(
  ".convert_ncbi_idconv_normalize_key() lowercases DOI only",
  {
    testthat::expect_identical(
      .convert_ncbi_idconv_normalize_key(
        x = "10.1097/EDE.0000000000001091",
        type = "doi"
      ),
      "10.1097/ede.0000000000001091"
    )
    
    testthat::expect_identical(
      .convert_ncbi_idconv_normalize_key(
        x = "PMC6784763",
        type = "pmcid"
      ),
      "PMC6784763"
    )
  }
)


testthat::test_that(
  ".convert_ncbi_idconv_batch() skips matched error records",
  {
    testthat::local_mocked_bindings(
      .scholidonline_pmc_idconv = function(ids, ..., quiet = FALSE) {
        list(
          records = list(
            list(
              pmid = "31469695",
              pmcid = "PMC6784763",
              status = "error"
            )
          )
        )
      }
    )
    
    out <- .convert_ncbi_idconv_batch(
      x = "31469695",
      from = "pmid",
      to = "pmcid",
      quiet = TRUE
    )
    
    testthat::expect_identical(
      out,
      NA_character_
    )
  }
)